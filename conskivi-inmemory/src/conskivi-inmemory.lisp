;;;; -*- mode: lisp -*-

(in-package #:conskivi-inmemory)

;;; Configuration parameters

(defparameter *stripes-count* 256
  "Number of striped locks for per-key locking.")

(defparameter *type-threshold* 128
  "Threshold for switching between small and large type encodings.")

(defparameter *ttl-interval* 100
  "Active expiration check interval in milliseconds.")

(defparameter *save-interval* 60
  "Automatic save interval in seconds.")

(defparameter *save-extension* ".ck.saving"
  "Extension for temporary save files.")

;;; Internal data structures

(defstruct (key-data (:constructor make-key-data))
  value
  type
  expiration)

(defstruct (sorted-set (:constructor make-sorted-set))
  scores
  members)

;;; Helper functions for hash-tables

(defun hash-table-keys (ht)
  "Return a list of all keys in hash-table HT."
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht)
    keys))

(defun hash-table-alist (ht)
  "Return an alist of all key-value pairs in hash-table HT."
  (let ((alist '()))
    (maphash (lambda (k v) (push (cons k v) alist)) ht)
    alist))

(defun copy-hash-table (ht)
  "Return a copy of hash-table HT."
  (let ((copy (make-hash-table :test (hash-table-test ht)
                                :size (hash-table-size ht)
                                :rehash-size (hash-table-rehash-size ht)
                                :rehash-threshold (hash-table-rehash-threshold ht))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) ht)
    copy))

(defun get-lock (database name)
  "Get or create a named lock for the database."
  (let ((locks (slot-value database 'pubsub-locks)))
    (or (gethash name locks)
        (let ((lock (bt2:make-lock :name (format nil "pubsub-lock-~a" name))))
          (setf (gethash name locks) lock)))))

;;; Database class

(defclass conskivi-inmemory-database (conskivi-database)
  ((base-data
    :initform (make-hash-table :test #'equal)
    :documentation "Base data hash-table (read during save)")
   (new-data
    :initform (make-hash-table :test #'equal)
    :documentation "New data hash-table (writes go here)")
   (deleted-keys
    :initform (make-hash-table :test #'equal)
    :documentation "Tombstones for deleted keys")
   (data-lock
    :initform (bt2:make-lock :name "conskivi-data-lock")
    :documentation "Lock for data operations")
   (save-lock
    :initform (bt2:make-lock :name "conskivi-save-lock")
    :documentation "Lock for save operations")
   (db-path
    :initarg :db-path
    :initform (error "db-path must be defined"))
   (state
    :initform :stopped)
   (pubsub-channels
    :initform (make-hash-table :test #'equal))
   (pubsub-patterns
    :initform (make-hash-table :test #'equal))
   (watched-keys
    :initform (make-hash-table :test #'equal))
   (multi-queue
    :initform nil)
   (ttl-interval
    :initarg :ttl-interval
    :initform *ttl-interval*)
   (save-interval
    :initarg :save-interval
    :initform *save-interval*)
   (type-threshold
    :initarg :type-threshold
    :initform *type-threshold*)
   (stripes-count
    :initarg :stripes-count
    :initform *stripes-count*)
   (saving-p
    :initform nil
    :documentation "Flag indicating save in progress")
   (save-requested
    :initform nil
    :documentation "Flag indicating save requested")
    (periodical-save-thread)
    (expiration-thread)
    (pubsub-locks
     :initform (make-hash-table :test #'equal)
     :documentation "Hash-table of per-channel/per-pattern locks for pub/sub")))

;;; Data access helpers

(defmethod get-key-data ((database conskivi-inmemory-database) key)
  "Get key-data from new-data first, then base-data."
  (let ((new-val (gethash key (slot-value database 'new-data)))
        (base-val (gethash key (slot-value database 'base-data))))
    (or new-val base-val)))

(defmethod set-key-data ((database conskivi-inmemory-database) key key-data)
  "Set key-data in new-data."
  (setf (gethash key (slot-value database 'new-data)) key-data))

(defmethod delete-key-data ((database conskivi-inmemory-database) key)
  "Delete key by adding tombstone to deleted-keys and removing from new-data."
  (setf (gethash key (slot-value database 'deleted-keys)) t)
  (remhash key (slot-value database 'new-data)))

(defmethod is-deleted-p ((database conskivi-inmemory-database) key)
  "Check if key is deleted (in tombstone set)."
  (gethash key (slot-value database 'deleted-keys)))

;;; Type detection

(defmethod detect-type (value)
  (cond
    ((typep value 'integer) :integer)
    ((typep value 'float) :float)
    ((typep value 'string) :string)
    ((typep value 'boolean) :boolean)
    ((typep value 'keyword) :keyword)
    ((typep value 'vector) :array)
    ((typep value 'list) :list)
    ((typep value 'hash-table) :hash)
    ((typep value 'sorted-set) :sorted-set)
    (t (error "Unsupported type: ~a" (type-of value)))))

;;; TTL support

(defmethod set-expiration ((database conskivi-inmemory-database) key seconds)
  (let ((key-data (get-key-data database key)))
    (when key-data
      (setf (key-data-expiration key-data)
            (+ (get-universal-time) seconds)))))

(defmethod get-ttl ((database conskivi-inmemory-database) key)
  (let ((key-data (get-key-data database key)))
    (if key-data
        (let ((exp (key-data-expiration key-data)))
          (if exp
              (max 0 (- exp (get-universal-time)))
              -1))
        -2)))

(defmethod persist-key ((database conskivi-inmemory-database) key)
  (let ((key-data (get-key-data database key)))
    (when key-data
      (setf (key-data-expiration key-data) nil))))

(defmethod check-expiration ((database conskivi-inmemory-database) key)
  (let ((key-data (get-key-data database key)))
    (when (and key-data (key-data-expiration key-data))
      (when (> (get-universal-time) (key-data-expiration key-data))
        (delete-key-data database key)
        t))))

;;; Active expiration thread

(defmethod start-expiration-thread ((database conskivi-inmemory-database))
  (let ((interval (slot-value database 'ttl-interval)))
    (setf (slot-value database 'expiration-thread)
          (bt2:make-thread
           (lambda ()
             (loop
               (sleep (/ interval 1000.0))
               (maphash (lambda (key key-data)
                          (when (and (key-data-expiration key-data)
                                     (> (get-universal-time) (key-data-expiration key-data)))
                            (delete-key-data database key)))
                        (slot-value database 'base-data))
               (maphash (lambda (key key-data)
                          (when (and (key-data-expiration key-data)
                                     (> (get-universal-time) (key-data-expiration key-data)))
                            (delete-key-data database key)))
                        (slot-value database 'new-data))))
           :name "conskivi-expiration"))))

(defmethod stop-expiration-thread ((database conskivi-inmemory-database))
  (when (slot-value database 'expiration-thread)
    (bt2:destroy-thread (slot-value database 'expiration-thread))
    (setf (slot-value database 'expiration-thread) nil)))

;;; Merge logic

(defmethod merge-data ((database conskivi-inmemory-database))
  "Merge new-data into base-data. New wins. Tombstones remove from base."
  (let ((base (slot-value database 'base-data))
        (new (slot-value database 'new-data))
        (deleted (slot-value database 'deleted-keys)))
    ;; First, remove deleted keys from base
    (maphash (lambda (key val)
               (declare (ignore val))
               (remhash key base))
             deleted)
    ;; Then, merge new data into base
    (maphash (lambda (key key-data)
               (setf (gethash key base) key-data))
             new)
    ;; Clear new data and tombstones
    (clrhash new)
    (clrhash deleted)))

;;; Save logic

(defmethod write-to-disk ((database conskivi-inmemory-database))
  "Write base-data to disk with atomic rename."
  (let* ((db-path (slot-value database 'db-path))
         (save-path (make-pathname :defaults db-path
                                   :name (pathname-name db-path)
                                   :type (subseq *save-extension* 1))))
    (with-open-file (stream save-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (maphash (lambda (key key-data)
                 (write (cons key (key-data-value key-data)) :stream stream))
               (slot-value database 'base-data)))
    ;; Atomic rename
    (rename-file save-path db-path)))

(defmethod save-thread ((database conskivi-inmemory-database))
  "Background save thread."
  (let ((interval (slot-value database 'save-interval)))
    (setf (slot-value database 'periodical-save-thread)
          (bt2:make-thread
           (lambda ()
             (loop
               (sleep interval)
               (when (and (not (slot-value database 'saving-p))
                          (slot-value database 'save-requested))
                 (setf (slot-value database 'saving-p) t)
                 (setf (slot-value database 'save-requested) nil)
                 (bt2:with-lock-held ((slot-value database 'save-lock))
                   (merge-data database)
                   (write-to-disk database))
                 (setf (slot-value database 'saving-p) nil))))
           :name "conskivi-save"))))

(defmethod request-save ((database conskivi-inmemory-database))
  "Request a save operation."
  (setf (slot-value database 'save-requested) t))

(defmethod save-now ((database conskivi-inmemory-database))
  "Perform save immediately (synchronous)."
  (bt2:with-lock-held ((slot-value database 'save-lock))
    (merge-data database)
    (write-to-disk database)))

;;; Pub/Sub

(defmethod subscribe ((database conskivi-inmemory-database) channel callback)
  (let ((lock (get-lock database (format nil "pubsub-~a" channel))))
    (bt2:with-lock-held (lock)
      (push callback (gethash channel (slot-value database 'pubsub-channels))))))

(defmethod unsubscribe ((database conskivi-inmemory-database) channel)
  (let ((lock (get-lock database (format nil "pubsub-~a" channel))))
    (bt2:with-lock-held (lock)
      (remhash channel (slot-value database 'pubsub-channels)))))

(defmethod psubscribe ((database conskivi-inmemory-database) pattern callback)
  (let ((lock (get-lock database (format nil "pubsub-pattern-~a" pattern))))
    (bt2:with-lock-held (lock)
      (push callback (gethash pattern (slot-value database 'pubsub-patterns))))))

(defmethod punsubscribe ((database conskivi-inmemory-database) pattern)
  (let ((lock (get-lock database (format nil "pubsub-pattern-~a" pattern))))
    (bt2:with-lock-held (lock)
      (remhash pattern (slot-value database 'pubsub-patterns)))))

(defmethod publish ((database conskivi-inmemory-database) channel message)
  (let ((lock (get-lock database (format nil "pubsub-~a" channel))))
    (bt2:with-lock-held (lock)
      (let ((callbacks (gethash channel (slot-value database 'pubsub-channels))))
        (dolist (callback callbacks)
          (funcall callback message)))))
  (maphash (lambda (pattern callbacks)
             (when (cl-ppcre:scan pattern (format nil "~a" channel))
               (dolist (callback callbacks)
                 (funcall callback message))))
           (slot-value database 'pubsub-patterns)))

;;; Transactions

(defmethod multi ((database conskivi-inmemory-database))
  (setf (slot-value database 'multi-queue) (list (cons 'transaction-start nil))))

(defmethod exec ((database conskivi-inmemory-database))
  (let ((queue (slot-value database 'multi-queue)))
    (setf (slot-value database 'multi-queue) nil)
    (let ((watched (slot-value database 'watched-keys)))
      (when watched
        (maphash (lambda (key expected)
                   (let ((key-data (get-key-data database key)))
                     (when (or (not key-data)
                               (not (equal (key-data-value key-data) expected)))
                       (error "Watched key ~a was modified" key))))
                 watched))
      (setf (slot-value database 'watched-keys) nil)
      (let ((ops (remove-if-not #'cdr queue)))
        (mapcar (lambda (op)
                  (let ((impl-fn (intern (format nil "~a-IMPL" (car op)) :conskivi-inmemory)))
                    (apply impl-fn database (cdr op))))
                ops)))))

(defmethod discard ((database conskivi-inmemory-database))
  (setf (slot-value database 'multi-queue) nil)
  (setf (slot-value database 'watched-keys) nil))

(defmethod watch ((database conskivi-inmemory-database) key)
  (let ((key-data (get-key-data database key)))
    (setf (gethash key (slot-value database 'watched-keys))
          (when key-data (key-data-value key-data)))))

(defmethod unwatch ((database conskivi-inmemory-database))
  (setf (slot-value database 'watched-keys) nil))

;;; Helper to queue or execute

(defmethod queue-or-execute ((database conskivi-inmemory-database) function-name &rest args)
  (if (slot-value database 'multi-queue)
      (push (cons function-name args) (slot-value database 'multi-queue))
      (let ((impl-fn (intern (format nil "~a-IMPL" function-name) :conskivi-inmemory)))
        (apply impl-fn database args))))

;;; Basic operations

(defmethod conskivi-put ((database conskivi-inmemory-database) key value)
  (queue-or-execute database 'conskivi-put key value))

(defmethod conskivi-put-impl ((database conskivi-inmemory-database) key value)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (set-key-data database key
                    (make-key-data :value value
                                  :type (detect-type value))))))

(defmethod conskivi-get ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (when (check-expiration database key)
        (return-from conskivi-get nil))
      (let ((key-data (get-key-data database key)))
        (when key-data
          (key-data-value key-data))))))

(defmethod conskivi-del ((database conskivi-inmemory-database) key)
  (queue-or-execute database 'conskivi-del key))

(defmethod conskivi-del-impl ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (delete-key-data database key))))

;;; Key operations

(defmethod conskivi-exists ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (when (check-expiration database key)
        (return-from conskivi-exists nil))
      (get-key-data database key))))

(defmethod conskivi-type ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (when (check-expiration database key)
        (return-from conskivi-type nil))
      (let ((key-data (get-key-data database key)))
        (when key-data
          (key-data-type key-data))))))

(defmethod conskivi-keys ((database conskivi-inmemory-database) &optional pattern)
  (let ((result '()))
    (maphash (lambda (key key-data)
               (when (and (not (is-deleted-p database key))
                          (not (key-data-expiration key-data))
                          (or (null pattern)
                              (cl-ppcre:scan pattern (format nil "~a" key))))
                 (push key result)))
             (slot-value database 'base-data))
    (maphash (lambda (key key-data)
               (when (and (not (is-deleted-p database key))
                          (not (key-data-expiration key-data))
                          (or (null pattern)
                              (cl-ppcre:scan pattern (format nil "~a" key)))
                           (not (member key result)))
                  (push key result)))
              (slot-value database 'new-data))
    result))

(defmethod conskivi-scan ((database conskivi-inmemory-database) cursor &optional pattern count)
  (let ((result '())
        (remaining (or count 10))
        (next-cursor nil))
    (maphash (lambda (key key-data)
               (when (and (not (is-deleted-p database key))
                          (not (key-data-expiration key-data))
                          (or (null pattern)
                              (cl-ppcre:scan pattern (format nil "~a" key)))
                          (> remaining 0))
                 (push key result)
                 (decf remaining)
                 (when (= remaining 0)
                   (setf next-cursor key))))
             (slot-value database 'base-data))
    (maphash (lambda (key key-data)
               (when (and (not (is-deleted-p database key))
                          (not (key-data-expiration key-data))
                          (or (null pattern)
                              (cl-ppcre:scan pattern (format nil "~a" key)))
                          (> remaining 0)
                          (not (member key result)))
                 (push key result)
                 (decf remaining)
                 (when (= remaining 0)
                   (setf next-cursor key))))
             (slot-value database 'new-data))
    (list (reverse result) next-cursor)))

(defmethod conskivi-expire ((database conskivi-inmemory-database) key seconds)
  (set-expiration database key seconds))

(defmethod conskivi-ttl ((database conskivi-inmemory-database) key)
  (get-ttl database key))

(defmethod conskivi-persist ((database conskivi-inmemory-database) key)
  (persist-key database key))

;;; Transactions

(defmethod conskivi-multi ((database conskivi-inmemory-database))
  (multi database))

(defmethod conskivi-exec ((database conskivi-inmemory-database))
  (exec database))

(defmethod conskivi-discard ((database conskivi-inmemory-database))
  (discard database))

;;; Locking

(defmethod conskivi-watch ((database conskivi-inmemory-database) key)
  (watch database key))

(defmethod conskivi-unwatch ((database conskivi-inmemory-database))
  (unwatch database))

;;; Pub/Sub

(defmethod conskivi-subscribe ((database conskivi-inmemory-database) channel callback)
  (subscribe database channel callback))

(defmethod conskivi-unsubscribe ((database conskivi-inmemory-database) channel)
  (unsubscribe database channel))

(defmethod conskivi-psubscribe ((database conskivi-inmemory-database) pattern callback)
  (psubscribe database pattern callback))

(defmethod conskivi-punsubscribe ((database conskivi-inmemory-database) pattern)
  (punsubscribe database pattern))

(defmethod conskivi-publish ((database conskivi-inmemory-database) channel message)
  (publish database channel message))

;;; String operations

(defmethod conskivi-append ((database conskivi-inmemory-database) key value)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (stringp (key-data-value key-data))
                (error "Key ~a is not a string" key))
              (let ((new-value (concatenate 'string (key-data-value key-data) value)))
                (set-key-data database key
                              (make-key-data :value new-value
                                            :type :string
                                            :expiration (key-data-expiration key-data)))
                (length new-value)))
            (progn
              (set-key-data database key
                            (make-key-data :value value :type :string))
              (length value)))))))

(defmethod conskivi-getset ((database conskivi-inmemory-database) key value)
  (let ((old-value (conskivi-get database key)))
    (conskivi-put database key value)
    old-value))

(defmethod conskivi-setnx ((database conskivi-inmemory-database) key value)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (unless (get-key-data database key)
        (set-key-data database key
                      (make-key-data :value value :type (detect-type value)))
        t))))

(defmethod conskivi-mget ((database conskivi-inmemory-database) keys)
  (mapcar (lambda (key) (conskivi-get database key)) keys))

(defmethod conskivi-mset ((database conskivi-inmemory-database) key-value-plist)
  (iter (for (key value) on key-value-plist by #'cddr)
    (conskivi-put database key value)))

;;; Integer operations

(defmethod conskivi-incr ((database conskivi-inmemory-database) key)
  (conskivi-incrby database key 1))

(defmethod conskivi-incrby ((database conskivi-inmemory-database) key increment)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (typep (key-data-value key-data) 'integer)
                (error "Key ~a is not an integer" key))
              (let ((new-value (+ (key-data-value key-data) increment)))
                (set-key-data database key
                              (make-key-data :value new-value
                                            :type :integer
                                            :expiration (key-data-expiration key-data)))
                new-value))
            (progn
              (set-key-data database key
                            (make-key-data :value increment :type :integer))
              increment))))))

(defmethod conskivi-decr ((database conskivi-inmemory-database) key)
  (conskivi-decrby database key 1))

(defmethod conskivi-decrby ((database conskivi-inmemory-database) key decrement)
  (conskivi-incrby database key (- decrement)))

;;; List operations

(defmethod conskivi-lpush ((database conskivi-inmemory-database) key &rest values)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (listp (key-data-value key-data))
                (error "Key ~a is not a list" key))
              (let ((new-value (append values (key-data-value key-data))))
                (set-key-data database key
                              (make-key-data :value new-value
                                            :type :list
                                            :expiration (key-data-expiration key-data)))
                (length new-value)))
            (progn
              (set-key-data database key
                            (make-key-data :value (reverse values) :type :list))
              (length values)))))))

(defmethod conskivi-rpush ((database conskivi-inmemory-database) key &rest values)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (listp (key-data-value key-data))
                (error "Key ~a is not a list" key))
              (let ((new-value (append (key-data-value key-data) values)))
                (set-key-data database key
                              (make-key-data :value new-value
                                            :type :list
                                            :expiration (key-data-expiration key-data)))
                (length new-value)))
            (progn
              (set-key-data database key
                            (make-key-data :value values :type :list))
              (length values)))))))

(defmethod conskivi-lpop ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (listp (key-data-value key-data)))
          (let ((value (car (key-data-value key-data)))
                (new-list (cdr (key-data-value key-data))))
            (if new-list
                (set-key-data database key
                              (make-key-data :value new-list
                                            :type :list
                                            :expiration (key-data-expiration key-data)))
                (delete-key-data database key))
            value))))))

(defmethod conskivi-rpop ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (listp (key-data-value key-data)))
          (let ((value (car (last (key-data-value key-data))))
                (new-list (butlast (key-data-value key-data))))
            (if new-list
                (set-key-data database key
                              (make-key-data :value new-list
                                            :type :list
                                            :expiration (key-data-expiration key-data)))
                (delete-key-data database key))
            value))))))

(defmethod conskivi-lrange ((database conskivi-inmemory-database) key start stop)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (listp (key-data-value key-data)))
          (let ((list (key-data-value key-data))
                (len (length (key-data-value key-data))))
            (let ((start (if (minusp start) (max 0 (+ len start)) (min start (1- len))))
                  (stop (if (minusp stop) (+ len stop) (min stop (1- len)))))
              (when (<= start stop)
                (subseq list start (1+ stop))))))))))

(defmethod conskivi-llen ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if (and key-data (listp (key-data-value key-data)))
            (length (key-data-value key-data))
            0)))))

(defmethod conskivi-lindex ((database conskivi-inmemory-database) key index)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (listp (key-data-value key-data)))
          (let ((list (key-data-value key-data))
                (len (length (key-data-value key-data))))
            (when (< (abs index) len)
              (if (minusp index)
                  (nth (+ len index) list)
                  (nth index list)))))))))

(defmethod conskivi-lset ((database conskivi-inmemory-database) key index value)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (unless (and key-data (listp (key-data-value key-data)))
          (error "Key ~a is not a list" key))
        (let ((len (length (key-data-value key-data))))
          (when (>= (abs index) len)
            (error "Index out of range"))
          (let ((new-list (copy-list (key-data-value key-data))))
            (if (minusp index)
                (setf (nth (+ len index) new-list) value)
                (setf (nth index new-list) value))
            (set-key-data database key
                          (make-key-data :value new-list
                                        :type :list
                                        :expiration (key-data-expiration key-data)))
            t))))))

(defmethod conskivi-lscan ((database conskivi-inmemory-database) key cursor &optional pattern count)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (listp (key-data-value key-data)))
          (let ((list (key-data-value key-data))
                (result '())
                (remaining (or count 10))
                (next-cursor nil))
            (iter (for i from (or cursor 0))
              (for value in list)
              (when (and (or (null pattern)
                             (cl-ppcre:scan pattern (format nil "~a" value)))
                         (> remaining 0))
                (push value result)
                (decf remaining)
                (when (= remaining 0)
                  (setf next-cursor (1+ i))))
              (finally (return (list (reverse result) next-cursor))))))))))

;;; Set operations

(defmethod conskivi-sadd ((database conskivi-inmemory-database) key &rest members)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (or (listp (key-data-value key-data))
                          (hash-table-p (key-data-value key-data)))
                (error "Key ~a is not a set" key))
              (let ((added 0)
                    (new-value (if (hash-table-p (key-data-value key-data))
                                   (copy-hash-table (key-data-value key-data))
                                   (let ((ht (make-hash-table :test #'equal)))
                                     (dolist (m (key-data-value key-data))
                                       (setf (gethash m ht) t))
                                     ht))))
                (dolist (member members)
                  (unless (gethash member new-value)
                    (incf added)
                    (setf (gethash member new-value) t)))
                (set-key-data database key
                              (make-key-data :value new-value
                                            :type :set
                                            :expiration (key-data-expiration key-data)))
                added))
            (progn
              (let ((ht (make-hash-table :test #'equal)))
                (dolist (m members)
                  (setf (gethash m ht) t))
                (set-key-data database key
                              (make-key-data :value ht :type :set))
                (length members))))))))

(defmethod conskivi-srem ((database conskivi-inmemory-database) key &rest members)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data
                   (or (listp (key-data-value key-data))
                       (hash-table-p (key-data-value key-data))))
          (let ((removed 0)
                (new-value (if (hash-table-p (key-data-value key-data))
                               (copy-hash-table (key-data-value key-data))
                               (let ((ht (make-hash-table :test #'equal)))
                                 (dolist (m (key-data-value key-data))
                                   (setf (gethash m ht) t))
                                 ht))))
            (dolist (member members)
              (when (gethash member new-value)
                (incf removed)
                (remhash member new-value)))
            (set-key-data database key
                          (make-key-data :value new-value
                                        :type :set
                                        :expiration (key-data-expiration key-data)))
            removed))))))

(defmethod conskivi-smembers ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data
                   (or (listp (key-data-value key-data))
                       (hash-table-p (key-data-value key-data))))
          (if (hash-table-p (key-data-value key-data))
              (hash-table-keys (key-data-value key-data))
              (key-data-value key-data)))))))

(defmethod conskivi-sismember ((database conskivi-inmemory-database) key member)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data
                   (or (listp (key-data-value key-data))
                       (hash-table-p (key-data-value key-data))))
          (if (hash-table-p (key-data-value key-data))
              (gethash member (key-data-value key-data))
              (member member (key-data-value key-data))))))))

(defmethod conskivi-scard ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if (and key-data
                 (or (listp (key-data-value key-data))
                     (hash-table-p (key-data-value key-data))))
            (if (hash-table-p (key-data-value key-data))
                (hash-table-count (key-data-value key-data))
                (length (key-data-value key-data)))
            0)))))

(defmethod conskivi-sunion ((database conskivi-inmemory-database) keys)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (key keys)
      (let ((members (conskivi-smembers database key)))
        (when members
          (dolist (member members)
            (setf (gethash member result) t)))))
    (hash-table-keys result)))

(defmethod conskivi-sinter ((database conskivi-inmemory-database) keys)
  (let ((sets (mapcar (lambda (key) (conskivi-smembers database key)) keys)))
    (when (every #'identity sets)
      (let ((result (make-hash-table :test #'equal)))
        (dolist (member (car sets))
          (setf (gethash member result) t))
        (dolist (set (cdr sets))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (unless (member k set)
                       (remhash k result)))
                   result))
        (hash-table-keys result)))))

(defmethod conskivi-sdiff ((database conskivi-inmemory-database) keys)
  (let ((first-set (conskivi-smembers database (car keys)))
        (rest-sets (mapcar (lambda (key) (conskivi-smembers database key)) (cdr keys))))
    (when first-set
      (let ((result (make-hash-table :test #'equal)))
        (dolist (member first-set)
          (setf (gethash member result) t))
        (dolist (set rest-sets)
          (when set
            (dolist (member set)
              (remhash member result))))
        (hash-table-keys result)))))

(defmethod conskivi-sscan ((database conskivi-inmemory-database) key cursor &optional pattern count)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data
                   (or (listp (key-data-value key-data))
                       (hash-table-p (key-data-value key-data))))
          (let ((members (if (hash-table-p (key-data-value key-data))
                             (hash-table-keys (key-data-value key-data))
                             (key-data-value key-data)))
                (result '())
                (remaining (or count 10))
                (next-cursor nil))
            (iter (for i from (or cursor 0))
              (for member in members)
              (when (and (or (null pattern)
                             (cl-ppcre:scan pattern (format nil "~a" member)))
                         (> remaining 0))
                (push member result)
                (decf remaining)
                (when (= remaining 0)
                  (setf next-cursor (1+ i))))
              (finally (return (list (reverse result) next-cursor))))))))))

;;; Sorted set operations

(defmethod conskivi-zadd ((database conskivi-inmemory-database) key score member &rest score-member-pairs)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (sorted-set-p (key-data-value key-data))
                (error "Key ~a is not a sorted set" key))
              (let* ((ss (key-data-value key-data))
                     (new-scores (copy-hash-table (sorted-set-scores ss)))
                     (new-members (copy-hash-table (sorted-set-members ss)))
                     (added 0))
                (setf (gethash member new-scores) score)
                (unless (gethash member new-members)
                  (incf added)
                  (setf (gethash member new-members) t))
                (iter (for (s m) on score-member-pairs by #'cddr)
                  (unless (gethash m new-members)
                    (incf added))
                  (setf (gethash m new-scores) s)
                  (setf (gethash m new-members) t))
                (let ((new-ss (make-sorted-set :scores new-scores :members new-members)))
                  (set-key-data database key
                                (make-key-data :value new-ss
                                              :type :sorted-set
                                              :expiration (key-data-expiration key-data)))
                  added)))
            (progn
              (let ((ss (make-sorted-set :scores (make-hash-table :test #'equal)
                                        :members (make-hash-table :test #'equal))))
                (setf (gethash member (sorted-set-scores ss)) score)
                (setf (gethash member (sorted-set-members ss)) t)
                (iter (for (s m) on score-member-pairs by #'cddr)
                  (setf (gethash m (sorted-set-scores ss)) s)
                  (setf (gethash m (sorted-set-members ss)) t))
                (set-key-data database key
                              (make-key-data :value ss :type :sorted-set))
                (hash-table-count (sorted-set-members ss)))))))))

(defmethod conskivi-zrem ((database conskivi-inmemory-database) key &rest members)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (sorted-set-p (key-data-value key-data)))
          (let* ((ss (key-data-value key-data))
                 (new-scores (copy-hash-table (sorted-set-scores ss)))
                 (new-members (copy-hash-table (sorted-set-members ss)))
                 (removed 0))
            (dolist (member members)
              (when (gethash member new-members)
                (incf removed)
                (remhash member new-members)
                (remhash member new-scores)))
            (let ((new-ss (make-sorted-set :scores new-scores :members new-members)))
              (set-key-data database key
                            (make-key-data :value new-ss
                                          :type :sorted-set
                                          :expiration (key-data-expiration key-data))))
            removed))))))

(defmethod conskivi-zrange ((database conskivi-inmemory-database) key start stop &optional withscores)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (sorted-set-p (key-data-value key-data)))
          (let* ((ss (key-data-value key-data))
                 (sorted (sort (copy-list (hash-table-keys (sorted-set-scores ss)))
                              #'<
                              :key (lambda (m) (gethash m (sorted-set-scores ss))))))
            (let ((result '()))
              (iter (for i from (max 0 start) to (min (1- (length sorted)) stop))
                (let ((member (nth i sorted)))
                  (if withscores
                      (progn
                        (push member result)
                        (push (gethash member (sorted-set-scores ss)) result))
                      (push member result))))
              (reverse result))))))))

(defmethod conskivi-zrevrange ((database conskivi-inmemory-database) key start stop &optional withscores)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (sorted-set-p (key-data-value key-data)))
          (let* ((ss (key-data-value key-data))
                 (sorted (sort (copy-list (hash-table-keys (sorted-set-scores ss)))
                              #'>
                              :key (lambda (m) (gethash m (sorted-set-scores ss))))))
            (let ((result '()))
              (iter (for i from (max 0 start) to (min (1- (length sorted)) stop))
                (let ((member (nth i sorted)))
                  (if withscores
                      (progn
                        (push member result)
                        (push (gethash member (sorted-set-scores ss)) result))
                      (push member result))))
              (reverse result))))))))

(defmethod conskivi-zscore ((database conskivi-inmemory-database) key member)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (sorted-set-p (key-data-value key-data)))
          (gethash member (sorted-set-scores (key-data-value key-data))))))))

(defmethod conskivi-zcard ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if (and key-data (sorted-set-p (key-data-value key-data)))
            (hash-table-count (sorted-set-members (key-data-value key-data)))
            0)))))

(defmethod conskivi-zincrby ((database conskivi-inmemory-database) key increment member)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (unless (and key-data (sorted-set-p (key-data-value key-data)))
          (error "Key ~a is not a sorted set" key))
        (let* ((ss (key-data-value key-data))
               (new-scores (copy-hash-table (sorted-set-scores ss)))
               (new-members (copy-hash-table (sorted-set-members ss)))
               (current (gethash member new-scores 0)))
          (setf (gethash member new-scores) (+ current increment))
          (setf (gethash member new-members) t)
          (let ((new-ss (make-sorted-set :scores new-scores :members new-members)))
            (set-key-data database key
                          (make-key-data :value new-ss
                                        :type :sorted-set
                                        :expiration (key-data-expiration key-data))))
          (+ current increment))))))

(defmethod conskivi-zscan ((database conskivi-inmemory-database) key cursor &optional pattern count)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (sorted-set-p (key-data-value key-data)))
          (let* ((ss (key-data-value key-data))
                 (sorted (sort (copy-list (hash-table-keys (sorted-set-scores ss)))
                              #'<
                              :key (lambda (m) (gethash m (sorted-set-scores ss)))))
                (result '())
                (remaining (or count 10))
                (next-cursor nil))
            (iter (for i from (or cursor 0))
              (for member in sorted)
              (when (and (or (null pattern)
                             (cl-ppcre:scan pattern (format nil "~a" member)))
                         (> remaining 0))
                (push member result)
                (push (gethash member (sorted-set-scores ss)) result)
                (decf remaining)
                (when (= remaining 0)
                  (setf next-cursor (1+ i))))
              (finally (return (list (reverse result) next-cursor))))))))))

;;; Hash operations

(defmethod conskivi-hset ((database conskivi-inmemory-database) key field value)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (if key-data
            (progn
              (unless (hash-table-p (key-data-value key-data))
                (error "Key ~a is not a hash" key))
              (let ((new-ht (copy-hash-table (key-data-value key-data))))
                (setf (gethash field new-ht) value)
                (set-key-data database key
                              (make-key-data :value new-ht
                                            :type :hash
                                            :expiration (key-data-expiration key-data)))
                1))
            (progn
              (let ((ht (make-hash-table :test #'equal)))
                (setf (gethash field ht) value)
                (set-key-data database key
                              (make-key-data :value ht :type :hash))
                1)))))))

(defmethod conskivi-hget ((database conskivi-inmemory-database) key field)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (gethash field (key-data-value key-data)))))))

(defmethod conskivi-hdel ((database conskivi-inmemory-database) key field)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (when (gethash field (key-data-value key-data))
            (let ((new-ht (copy-hash-table (key-data-value key-data))))
              (remhash field new-ht)
              (set-key-data database key
                            (make-key-data :value new-ht
                                          :type :hash
                                          :expiration (key-data-expiration key-data))))
            t))))))

(defmethod conskivi-hgetall ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (let ((result '()))
            (maphash (lambda (k v)
                       (push k result)
                       (push v result))
                     (key-data-value key-data))
            (reverse result)))))))

(defmethod conskivi-hkeys ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (hash-table-keys (key-data-value key-data)))))))

(defmethod conskivi-hvals ((database conskivi-inmemory-database) key)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (let ((result '()))
            (maphash (lambda (k v) (declare (ignore k)) (push v result))
                     (key-data-value key-data))
            result))))))

(defmethod conskivi-hexists ((database conskivi-inmemory-database) key field)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (gethash field (key-data-value key-data)))))))

(defmethod conskivi-hincrby ((database conskivi-inmemory-database) key field increment)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (unless (and key-data (hash-table-p (key-data-value key-data)))
          (error "Key ~a is not a hash" key))
        (let* ((ht (key-data-value key-data))
               (new-ht (copy-hash-table ht))
               (current (gethash field new-ht 0)))
          (unless (typep current 'integer)
            (error "Field ~a is not an integer" field))
          (setf (gethash field new-ht) (+ current increment))
          (set-key-data database key
                        (make-key-data :value new-ht
                                      :type :hash
                                      :expiration (key-data-expiration key-data)))
          (+ current increment))))))

(defmethod conskivi-hscan ((database conskivi-inmemory-database) key cursor &optional pattern count)
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (let ((key-data (get-key-data database key)))
        (when (and key-data (hash-table-p (key-data-value key-data)))
          (let ((result '())
                (remaining (or count 10))
                (next-cursor nil))
            (iter (for (k v) on (hash-table-alist (key-data-value key-data)) by #'cdr)
              (when (and (or (null pattern)
                             (cl-ppcre:scan pattern (format nil "~a" k)))
                         (> remaining 0))
                (push k result)
                (push v result)
                (decf remaining)
                (when (= remaining 0)
                  (setf next-cursor k)))
              (finally (return (list (reverse result) next-cursor))))))))))

;;; Lifecycle

(defmethod conskivi-load ((database conskivi-inmemory-database))
  (let ((lock (slot-value database 'data-lock)))
    (bt2:with-lock-held (lock)
      (when (probe-file (slot-value database 'db-path))
        (with-open-file (stream (slot-value database 'db-path) :direction :input)
          (iter
            (for key-value = (read stream nil nil))
            (while key-value)
            (for key = (car key-value))
            (for value = (cdr key-value))
            (setf (gethash key (slot-value database 'base-data))
                  (make-key-data :value value :type (detect-type value)))))))))

(defmethod conskivi-save ((database conskivi-inmemory-database))
  (save-now database))

(defmethod conskivi-start ((database conskivi-inmemory-database))
  (setf (slot-value database 'state) :running)
  (start-expiration-thread database)
  (save-thread database))

(defmethod conskivi-stop ((database conskivi-inmemory-database))
  (stop-expiration-thread database)
  (when (slot-value database 'periodical-save-thread)
    (bt2:destroy-thread (slot-value database 'periodical-save-thread))
    (setf (slot-value database 'periodical-save-thread) nil))
  (setf (slot-value database 'state) :stopped))