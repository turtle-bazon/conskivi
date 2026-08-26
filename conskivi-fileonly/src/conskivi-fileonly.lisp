;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; Database class

(defclass conskivi-fileonly-database (conskivi-core:conskivi-database)
  ((db-path
    :initarg :db-path
    :initform (error "Specify db-path"))
   (key-locks
    :initform (make-hash-table :test #'equal))
   (key-locks-lock
    :initform (bt2:make-lock :name "key-locks-lock"))
   (state
    :initform :stopped)
   (multi-queue
    :initform nil)
   (watched-keys
    :initform (make-hash-table :test #'equal))
   (pubsub-channels
    :initform (make-hash-table :test #'equal))
   (pubsub-patterns
    :initform (make-hash-table :test #'equal))
   (pubsub-locks
    :initform (make-hash-table :test #'equal))
   (ttl-interval
    :initarg :ttl-interval
    :initform 100)
   (expiration-thread
    :initform nil)
   (collection-index
    :initform (make-hash-table :test #'equal))))

;;; Lock management

(defun get-key-lock (database key)
  (let ((locks (slot-value database 'key-locks))
        (locks-lock (slot-value database 'key-locks-lock)))
    (or (gethash key locks)
        (bt2:with-lock-held (locks-lock)
          (or (gethash key locks)
              (setf (gethash key locks)
                    (bt2:make-lock :name (format nil "key-lock-~a" key))))))))

(defun get-pubsub-lock (database name)
  (let ((locks (slot-value database 'pubsub-locks)))
    (or (gethash name locks)
        (setf (gethash name locks)
              (bt2:make-lock :name (format nil "pubsub-lock-~a" name))))))

;;; TTL support

(defun check-expiration (database key)
  (let ((path (key-file-path database key)))
    (when (probe-file path)
      (with-open-file (stream path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (when stream
          (let ((type-tag (read-u8 stream))
                (expiration (read-i64 stream)))
            (declare (ignore type-tag))
            (when (and (plusp expiration)
                       (> (get-universal-time) expiration))
              (delete-file path)
              t)))))))

(defun set-expiration-impl (database key seconds)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (value type old-exp)
          (read-key-file database key)
        (declare (ignore old-exp))
        (when type
          (write-key-file database key value type
                          (+ (get-universal-time) seconds)))))))

(defun get-ttl-impl (database key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((path (key-file-path database key)))
        (if (probe-file path)
            (with-open-file (stream path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
              (if stream
                  (let ((type-tag (read-u8 stream))
                        (expiration (read-i64 stream)))
                    (declare (ignore type-tag))
                    (if (plusp expiration)
                        (max 0 (- expiration (get-universal-time)))
                        -1))
                  -2))
            -2)))))

(defun persist-key-impl (database key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (value type old-exp)
          (read-key-file database key)
        (declare (ignore old-exp))
        (when type
          (write-key-file database key value type 0))))))

;;; Active expiration thread

(defun start-expiration-thread (database)
  (let ((interval (slot-value database 'ttl-interval)))
    (setf (slot-value database 'expiration-thread)
          (bt2:make-thread
           (lambda ()
             (loop
               (sleep (/ interval 1000.0))
               (unless (eq (slot-value database 'state) :running)
                 (return))
               (dolist (key (list-all-keys database))
                 (check-expiration database key))))
           :name "expiration-thread"))))

;;; Transaction support

(defun queue-or-execute (database function-name &rest args)
  (if (slot-value database 'multi-queue)
      (push (cons function-name args) (slot-value database 'multi-queue))
      (let ((impl-fn (intern (format nil "~a-IMPL" function-name) :conskivi-fileonly)))
        (apply impl-fn database args))))

;;; Lifecycle

(defmethod conskivi-load ((database conskivi-fileonly-database))
  (ensure-directories-exist (slot-value database 'db-path)))

(defmethod conskivi-save ((database conskivi-fileonly-database))
  (flush-all-collections database)
  nil)

(defun build-skiplist-from-btree (entry tree)
  "Build skiplist from B+tree entries for sorted set scoring."
  (setf (cie-score-tree entry) (make-zset-skiplist))
  (let ((root (meta-score-root (btree-meta tree))))
    (when (> root 0)
      (btree-map-entries tree root
        (lambda (key-bytes entry-bytes)
          (declare (ignore key-bytes))
          (multiple-value-bind (status score member-bytes)
              (decode-zset-leaf-entry entry-bytes 0)
            (when (= status #x01)
              (let ((score-key (make-zset-key score member-bytes)))
                (skiplist-insert (cie-score-tree entry) score-key t)
                (incf (cie-count entry))))))))))

(defun load-btree-collection (path key index)
  "Load a single B+tree collection file into the index."
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                                :if-does-not-exist nil)
    (unless stream
      (return-from load-btree-collection nil))
    (let ((header (make-array 13 :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence header stream) 13)
        (return-from load-btree-collection nil))
      (let ((type-byte (aref header 0)))
        (unless (member type-byte (list +type-set+ +type-hash+ +type-zset+))
          (return-from load-btree-collection nil))
        (file-position stream 0)
        (let* ((meta-data (make-array +page-size+ :element-type '(unsigned-byte 8)))
               (n (read-sequence meta-data stream)))
          (unless (= n +page-size+)
            (return-from load-btree-collection nil))
          (let ((meta (read-meta meta-data)))
            (unless (= (meta-magic meta) +meta-magic+)
              (return-from load-btree-collection nil))
            (let* ((entry-type (cond ((= type-byte +type-set+) :set)
                                     ((= type-byte +type-hash+) :hash)
                                     ((= type-byte +type-zset+) :sorted-set)))
                   (entry (make-collection-index-entry
                           :key key
                           :type entry-type
                           :expiration (meta-expiration meta)))
                   (tree (btree-open-from-meta path meta-data)))
              (setf (cie-tree entry) tree)
              (when (eq entry-type :sorted-set)
                (build-skiplist-from-btree entry tree))
              (unless (eq entry-type :sorted-set)
                (setf (cie-count entry) (meta-entry-count (btree-meta tree))))
              (setf (gethash key index) entry))))))))

(defmethod conskivi-start ((database conskivi-fileonly-database))
  (conskivi-load database)
  (let ((db-path (slot-value database 'db-path))
        (index (get-collection-index database)))
    (when (probe-file db-path)
      (dolist (path (directory (merge-pathnames #p"*.ck" db-path)))
        (let* ((name (format nil "~a.~a" (pathname-name path) (pathname-type path)))
               (key (intern (string-upcase (subseq name 0 (- (length name) 3))) :keyword)))
          (load-btree-collection path key index)))))
  (setf (slot-value database 'state) :running)
  (start-expiration-thread database))

(defmethod conskivi-stop ((database conskivi-fileonly-database))
  (setf (slot-value database 'state) :stopped)
  (let ((thread (slot-value database 'expiration-thread)))
    (when thread
      (bt2:join-thread thread)
      (setf (slot-value database 'expiration-thread) nil)))
  ;; Flush all B+trees to disk
  (flush-all-collections database))

(defun simulate-crash (database)
  "Simulate a crash: stop without flushing. WAL entries survive for recovery."
  (setf (slot-value database 'state) :stopped)
  (let ((thread (slot-value database 'expiration-thread)))
    (when thread
      (setf (slot-value database 'expiration-thread) nil))))

;;; Basic operations

(defmethod conskivi-core:conskivi-put ((database conskivi-fileonly-database) key value)
  (queue-or-execute database 'conskivi-put key value))

(defmethod conskivi-put-impl ((database conskivi-fileonly-database) key value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (write-key-file database key value (detect-type value) 0))))

(defmethod conskivi-core:conskivi-get ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (when (check-expiration database key)
        (return-from conskivi-core:conskivi-get nil))
      (multiple-value-bind (value type expiration)
          (read-key-file database key)
        (declare (ignore type expiration))
        value))))

(defmethod conskivi-core:conskivi-del ((database conskivi-fileonly-database) key)
  (queue-or-execute database 'conskivi-del key))

(defmethod conskivi-del-impl ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (delete-key-file database key))))

;;; Key operations

(defmethod conskivi-core:conskivi-exists ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (if (probe-file (key-file-path database key)) 1 nil))))

(defmethod conskivi-core:conskivi-type ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (value type expiration)
          (read-key-file database key)
        (declare (ignore value expiration))
        type))))

(defmethod conskivi-core:conskivi-keys ((database conskivi-fileonly-database) &optional pattern)
  (let ((keys (list-all-keys database)))
    (if pattern
        (remove-if-not (lambda (k) (cl-ppcre:scan pattern (if (symbolp k) (symbol-name k) k))) keys)
        keys)))

(defmethod conskivi-core:conskivi-scan ((database conskivi-fileonly-database) cursor &optional pattern count)
  (let* ((all-keys (list-all-keys database))
         (cnt (or count 10))
         (start (or cursor 0))
         (result '())
         (next-cursor nil))
    (loop for i from start
          for key in (nthcdr start all-keys)
          repeat cnt
          when (or (null pattern) (cl-ppcre:scan pattern key))
          do (push key result))
    (when (<= (+ start cnt) (length all-keys))
      (setf next-cursor (+ start cnt)))
    (list (reverse result) next-cursor)))

(defmethod conskivi-core:conskivi-expire ((database conskivi-fileonly-database) key seconds)
  (set-expiration-impl database key seconds))

(defmethod conskivi-core:conskivi-ttl ((database conskivi-fileonly-database) key)
  (get-ttl-impl database key))

(defmethod conskivi-core:conskivi-persist ((database conskivi-fileonly-database) key)
  (persist-key-impl database key))

;;; Transactions

(defmethod conskivi-core:conskivi-multi ((database conskivi-fileonly-database))
  (setf (slot-value database 'multi-queue) (list (cons 'transaction-start nil))))

(defmethod conskivi-core:conskivi-exec ((database conskivi-fileonly-database))
  (let ((queue (slot-value database 'multi-queue)))
    (setf (slot-value database 'multi-queue) nil)
    (let ((watched (slot-value database 'watched-keys)))
      (when watched
        (maphash (lambda (key expected)
                   (multiple-value-bind (current-value current-type)
                       (read-key-file database key)
                     (declare (ignore current-type))
                     (unless (equal current-value expected)
                       (error "Watched key ~a was modified" key))))
                 watched))
      (setf (slot-value database 'watched-keys) nil)
      (let ((ops (remove-if-not #'cdr queue)))
        (mapcar (lambda (op)
                  (let ((impl-fn (intern (format nil "~a-IMPL" (car op)) :conskivi-fileonly)))
                    (apply impl-fn database (cdr op))))
                ops)))))

(defmethod conskivi-core:conskivi-discard ((database conskivi-fileonly-database))
  (setf (slot-value database 'multi-queue) nil)
  (setf (slot-value database 'watched-keys) nil))

(defmethod conskivi-core:conskivi-watch ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (value type expiration)
          (read-key-file database key)
        (declare (ignore expiration))
        (setf (gethash key (slot-value database 'watched-keys))
              (when type value))))))

(defmethod conskivi-core:conskivi-unwatch ((database conskivi-fileonly-database))
  (setf (slot-value database 'watched-keys) nil))

;;; Pub/Sub

(defmethod conskivi-core:conskivi-subscribe ((database conskivi-fileonly-database) channel callback)
  (let ((lock (get-pubsub-lock database channel)))
    (bt2:with-lock-held (lock)
      (push callback (gethash channel (slot-value database 'pubsub-channels) nil)))))

(defmethod conskivi-core:conskivi-unsubscribe ((database conskivi-fileonly-database) channel)
  (let ((lock (get-pubsub-lock database channel)))
    (bt2:with-lock-held (lock)
      (remhash channel (slot-value database 'pubsub-channels)))))

(defmethod conskivi-core:conskivi-psubscribe ((database conskivi-fileonly-database) pattern callback)
  (let ((lock (get-pubsub-lock database pattern)))
    (bt2:with-lock-held (lock)
      (push callback (gethash pattern (slot-value database 'pubsub-patterns) nil)))))

(defmethod conskivi-core:conskivi-punsubscribe ((database conskivi-fileonly-database) pattern)
  (let ((lock (get-pubsub-lock database pattern)))
    (bt2:with-lock-held (lock)
      (remhash pattern (slot-value database 'pubsub-patterns)))))

(defmethod conskivi-core:conskivi-publish ((database conskivi-fileonly-database) channel message)
  (let ((lock (get-pubsub-lock database channel))
        (channel-name (if (symbolp channel) (symbol-name channel) channel)))
    (bt2:with-lock-held (lock)
      (let ((callbacks (gethash channel (slot-value database 'pubsub-channels) nil)))
        (dolist (callback callbacks)
          (funcall callback message))))
    (maphash (lambda (pattern callbacks)
               (when (cl-ppcre:scan pattern channel-name)
                 (dolist (callback callbacks)
                   (funcall callback message))))
             (slot-value database 'pubsub-patterns)))
  0)

;;; Strings

(defmethod conskivi-core:conskivi-append ((database conskivi-fileonly-database) key value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (if (and type (stringp existing))
            (let ((new-value (concatenate 'string existing (if (stringp value) value (format nil "~a" value)))))
              (write-key-file database key new-value :string expiration)
              (length new-value))
            (let ((new-value (if (stringp value) value (format nil "~a" value))))
              (write-key-file database key new-value :string 0)
              (length new-value)))))))

(defmethod conskivi-core:conskivi-getset ((database conskivi-fileonly-database) key value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (old-value type expiration)
          (read-key-file database key)
        (declare (ignore type expiration))
        (write-key-file database key value (detect-type value) 0)
        old-value))))

(defmethod conskivi-core:conskivi-setnx ((database conskivi-fileonly-database) key value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((path (key-file-path database key)))
        (if (probe-file path)
            nil
            (progn
              (write-key-file database key value (detect-type value) 0)
              t))))))

(defmethod conskivi-core:conskivi-mget ((database conskivi-fileonly-database) keys)
  (mapcar (lambda (k) (conskivi-core:conskivi-get database k)) keys))

(defmethod conskivi-core:conskivi-mset ((database conskivi-fileonly-database) key-value-plist)
  (loop for (key value) on key-value-plist by #'cddr
        do (conskivi-core:conskivi-put database key value))
  t)

;;; Integers

(defun incr-impl (database key increment)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (value type expiration)
          (read-key-file database key)
        (when (and type (not (integerp value)))
          (error "Value at key ~a is not an integer" key))
        (let ((current (if (and type (integerp value)) value 0)))
          (let ((new-value (+ current increment)))
            (write-key-file database key new-value :integer (or expiration 0))
            new-value))))))

(defmethod conskivi-core:conskivi-incr ((database conskivi-fileonly-database) key)
  (incr-impl database key 1))

(defmethod conskivi-core:conskivi-incrby ((database conskivi-fileonly-database) key increment)
  (incr-impl database key increment))

(defmethod conskivi-core:conskivi-decr ((database conskivi-fileonly-database) key)
  (incr-impl database key -1))

(defmethod conskivi-core:conskivi-decrby ((database conskivi-fileonly-database) key decrement)
  (incr-impl database key (- decrement)))

;;; Lists

(defmethod conskivi-core:conskivi-lpush ((database conskivi-fileonly-database) key &rest values)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (let ((list (if (and type (listp existing)) existing nil)))
          (let ((new-list (append (reverse values) list)))
            (write-key-file database key new-list :list (or expiration 0))
            (length new-list)))))))

(defmethod conskivi-core:conskivi-rpush ((database conskivi-fileonly-database) key &rest values)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (let ((list (if (and type (listp existing)) existing nil)))
          (let ((new-list (append list values)))
            (write-key-file database key new-list :list (or expiration 0))
            (length new-list)))))))

(defmethod conskivi-core:conskivi-lpop ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (when (and type (listp existing) existing)
          (let ((value (car existing))
                (new-list (cdr existing)))
            (if new-list
                (write-key-file database key new-list :list (or expiration 0))
                (delete-key-file database key))
            value))))))

(defmethod conskivi-core:conskivi-rpop ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (when (and type (listp existing) existing)
          (if (null (cdr existing))
              (progn
                (delete-key-file database key)
                (car existing))
              (let ((value (car (last existing)))
                    (new-list (butlast existing)))
                (write-key-file database key new-list :list (or expiration 0))
                value)))))))

(defmethod conskivi-core:conskivi-lrange ((database conskivi-fileonly-database) key start stop)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (declare (ignore expiration))
        (when (and type (listp existing))
          (let ((len (length existing)))
            (let ((start (if (minusp start) (max 0 (+ len start)) (min start (1- len))))
                  (stop (if (minusp stop) (+ len stop) (min stop (1- len)))))
              (when (<= start stop)
                (subseq existing start (1+ stop))))))))))

(defmethod conskivi-core:conskivi-llen ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (declare (ignore expiration))
        (if (and type (listp existing))
            (length existing)
            0)))))

(defmethod conskivi-core:conskivi-lindex ((database conskivi-fileonly-database) key index)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (declare (ignore expiration))
        (when (and type (listp existing))
          (let ((len (length existing)))
            (when (< (abs index) len)
              (if (minusp index)
                  (nth (+ len index) existing)
                  (nth index existing)))))))))

(defmethod conskivi-core:conskivi-lset ((database conskivi-fileonly-database) key index value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (unless (and type (listp existing))
          (error "Key ~a is not a list" key))
        (let ((len (length existing)))
          (when (>= (abs index) len)
            (error "Index out of range"))
          (let ((new-list (copy-list existing))
                (i (if (minusp index) (+ len index) index)))
            (setf (nth i new-list) value)
            (write-key-file database key new-list :list (or expiration 0))
            t))))))

(defmethod conskivi-core:conskivi-lscan ((database conskivi-fileonly-database) key cursor &optional pattern count)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (multiple-value-bind (existing type expiration)
          (read-key-file database key)
        (declare (ignore expiration))
        (when (and type (listp existing))
          (let ((result '())
                (remaining (or count 10))
                (next-cursor nil))
            (loop for i from (or cursor 0)
                  for value in existing
                  while (> remaining 0)
                  when (or (null pattern)
                           (cl-ppcre:scan pattern (format nil "~a" value)))
                  do (push value result)
                     (decf remaining)
                     (when (= remaining 0)
                       (setf next-cursor (1+ i))))
            (list (reverse result) next-cursor)))))))

;;; Sets

(defmethod conskivi-core:conskivi-sadd ((database conskivi-fileonly-database) key &rest members)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :set)))
        (let ((added 0))
          (dolist (member members)
            (when (index-sadd entry member)
              (incf added)))
          (setf (btree-dirty (cie-tree entry)) t)
          added)))))

(defmethod conskivi-core:conskivi-srem ((database conskivi-fileonly-database) key &rest members)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :set)))
        (let ((removed 0))
          (dolist (member members)
            (when (index-srem entry member)
              (incf removed)))
          (setf (btree-dirty (cie-tree entry)) t)
          removed)))))

(defmethod conskivi-core:conskivi-smembers ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-smembers entry)
            nil)))))

(defmethod conskivi-core:conskivi-sismember ((database conskivi-fileonly-database) key member)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if (and entry (index-sismember entry member))
            1 nil)))))

(defmethod conskivi-core:conskivi-scard ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry (index-scard entry) 0)))))

(defmethod conskivi-core:conskivi-sunion ((database conskivi-fileonly-database) keys)
  (index-sunion database keys))

(defmethod conskivi-core:conskivi-sinter ((database conskivi-fileonly-database) keys)
  (index-sinter database keys))

(defmethod conskivi-core:conskivi-sdiff ((database conskivi-fileonly-database) keys)
  (index-sdiff database keys))

(defmethod conskivi-core:conskivi-sscan ((database conskivi-fileonly-database) key cursor &optional pattern count)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-sscan entry cursor pattern count)
            (list nil nil))))))

;;; Sorted sets

(defmethod conskivi-core:conskivi-zadd ((database conskivi-fileonly-database) key score member &rest score-member-pairs)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :sorted-set))
            (added 0))
        (when (index-zadd entry score member)
          (incf added))
        (loop for (s m) on score-member-pairs by #'cddr
              when (index-zadd entry s m)
                do (incf added))
        (setf (btree-dirty (cie-tree entry)) t)
        added))))

(defmethod conskivi-core:conskivi-zrem ((database conskivi-fileonly-database) key &rest members)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database)))
            (removed 0))
        (when entry
          (dolist (member members)
            (when (index-zrem entry member)
              (incf removed)))
          (setf (btree-dirty (cie-tree entry)) t))
        removed))))

(defmethod conskivi-core:conskivi-zrange ((database conskivi-fileonly-database) key start stop &optional withscores)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (if withscores
                (index-zrange-withscores entry start stop)
                (index-zrange entry start stop))
            nil)))))

(defmethod conskivi-core:conskivi-zrevrange ((database conskivi-fileonly-database) key start stop &optional withscores)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-zrevrange entry start stop withscores)
            nil)))))

(defmethod conskivi-core:conskivi-zscore ((database conskivi-fileonly-database) key member)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-zscore entry member)
            nil)))))

(defmethod conskivi-core:conskivi-zcard ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-zcard entry)
            0)))))

(defmethod conskivi-core:conskivi-zincrby ((database conskivi-fileonly-database) key increment member)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :sorted-set)))
        (let ((result (index-zincrby entry increment member)))
          (setf (btree-dirty (cie-tree entry)) t)
          result)))))

(defmethod conskivi-core:conskivi-zscan ((database conskivi-fileonly-database) key cursor &optional pattern count)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-zscan entry cursor pattern count)
            (list nil nil))))))

;;; Hashes

(defmethod conskivi-core:conskivi-hset ((database conskivi-fileonly-database) key field value)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :hash)))
        (let ((result (index-hset entry field value)))
          (setf (btree-dirty (cie-tree entry)) t)
          result)))))

(defmethod conskivi-core:conskivi-hget ((database conskivi-fileonly-database) key field)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-hget entry field)
            nil)))))

(defmethod conskivi-core:conskivi-hdel ((database conskivi-fileonly-database) key field)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if (and entry (index-hdel entry field))
            (progn
              (setf (btree-dirty (cie-tree entry)) t)
              1)
            nil)))))

(defmethod conskivi-core:conskivi-hgetall ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-hgetall entry)
            nil)))))

(defmethod conskivi-core:conskivi-hkeys ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-hkeys entry)
            nil)))))

(defmethod conskivi-core:conskivi-hvals ((database conskivi-fileonly-database) key)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-hvals entry)
            nil)))))

(defmethod conskivi-core:conskivi-hexists ((database conskivi-fileonly-database) key field)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if (and entry (index-hexists entry field))
            1 nil)))))

(defmethod conskivi-core:conskivi-hincrby ((database conskivi-fileonly-database) key field increment)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (ensure-collection-index database key :hash)))
        (let ((result (index-hincrby entry field increment)))
          (setf (btree-dirty (cie-tree entry)) t)
          result)))))

(defmethod conskivi-core:conskivi-hscan ((database conskivi-fileonly-database) key cursor &optional pattern count)
  (let ((lock (get-key-lock database key)))
    (bt2:with-lock-held (lock)
      (let ((entry (gethash key (get-collection-index database))))
        (if entry
            (index-hscan entry cursor pattern count)
            (list nil nil))))))
