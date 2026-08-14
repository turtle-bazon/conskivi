;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; B+tree-backed index for sets, hashes, and sorted sets.
;;;
;;; All reads go through the B+tree on disk. No in-memory hash tables.
;;; Sorted sets use a skiplist for score-based ordering (ZRANGE/ZREVRANGE).

;;; Collection index entry structure

(defstruct (collection-index-entry (:conc-name cie-))
  (key nil)                               ; conskivi key
  (type :set)                             ; :set, :hash, :sorted-set
  (file-path nil)                         ; pathname to .ck file
  (tree nil)                              ; btree struct
  ;; Sorted set: skiplist for score ordering (score . member-bytes) -> T
  (score-tree nil)
  ;; Metadata
  (count 0 :type integer)
  (expiration 0 :type (signed-byte 64)))

;;; Database-level index

(defun get-collection-index (database)
  "Get the collection index hash-table from the database."
  (slot-value database 'collection-index))

(defun ensure-collection-index (database key type &optional expiration)
  "Get or create a collection index entry for a key."
  (let ((index (get-collection-index database))
        (type-byte (ecase type
                     (:set +type-set+)
                     (:hash +type-hash+)
                     (:sorted-set +type-zset+))))
    (or (gethash key index)
        (let ((path (key-file-path database key))
              (entry (make-collection-index-entry
                      :key key
                      :type type
                      :expiration (or expiration 0))))
          ;; Create or open B+tree
          (setf (cie-tree entry)
                (if (probe-file path)
                    (btree-open-existing path type-byte)
                    (btree-create path type-byte expiration)))
          ;; Build skiplist from B+tree for sorted sets
          (when (and (cie-tree entry) (eq type :sorted-set))
            (setf (cie-score-tree entry) (make-zset-skiplist))
            (let ((root (meta-score-root (btree-meta (cie-tree entry)))))
              (when (> root 0)
                (btree-map-entries (cie-tree entry) root
                   (lambda (key-bytes entry-bytes)
                     (declare (ignore key-bytes))
                     (multiple-value-bind (status score member-bytes)
                         (decode-zset-leaf-entry entry-bytes 0)
                       (when (= status #x01)
                         (let ((score-key (make-zset-key score member-bytes)))
                           (skiplist-insert (cie-score-tree entry) score-key t)
                           (incf (cie-count entry))))))))))
          ;; Count entries for non-zset types
          (when (and (cie-tree entry) (not (eq type :sorted-set)))
            (setf (cie-count entry)
                  (meta-entry-count (btree-meta (cie-tree entry)))))
          (setf (gethash key index) entry)
          entry))))

(defun btree-open-existing (file-path type)
  "Open an existing B+tree file."
  (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                    :if-does-not-exist nil)
    (when stream
      (let ((meta-data (make-array +page-size+ :element-type '(unsigned-byte 8))))
        (read-sequence meta-data stream)
        (let ((meta (read-meta meta-data)))
          ;; Verify magic and type
          (when (and (= (meta-magic meta) +meta-magic+)
                     (= (meta-type meta) type))
            ;; Load all pages into cache
            (let ((tree (make-btree :meta meta :file-path file-path)))
              (loop for page-num from 1 below (meta-page-count meta)
                    do (file-position stream (* page-num +page-size+))
                       (let ((page-data (make-array +page-size+ :element-type '(unsigned-byte 8))))
                         (read-sequence page-data stream)
                         (let ((page (make-btree-page :number page-num)))
                           (replace (page-data page) page-data)
                           (read-page-header page)
                           (btree-cache-page tree page))))
              tree)))))))

;;; Helper: convert member/field string to key bytes

(defun member-to-key-bytes (member)
  "Convert a member or field to key bytes for B+tree lookup."
  (let ((member-str (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
    (values member-str (value-to-key-bytes member-str))))

;;; Set operations via B+tree

(defun index-sadd (entry member)
  "Add a member to set. Returns T if new."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree)))
           (existing (btree-lookup-entry tree root member-key-bytes)))
      (unless existing
        (let ((entry-bytes (encode-set-entry #x01 (value-to-key-bytes member-str))))
          (setf (meta-score-root (btree-meta tree))
                (btree-insert-entry tree root member-key-bytes entry-bytes))
          (incf (cie-count entry))
          (incf (meta-entry-count (btree-meta tree)))
          (setf (btree-dirty tree) t))
        t))))

(defun index-sismember (entry member)
  "Check if member is in set."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (declare (ignore member-str))
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree))))
      (btree-lookup-entry tree root member-key-bytes))))

(defun index-srem (entry member)
  "Remove a member from set. Returns T if found."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (declare (ignore member-str))
    (let* ((tree (cie-tree entry))
           (root-page-num (meta-score-root (btree-meta tree))))
      (when (> root-page-num 0)
        (let ((root-page (btree-read-page tree root-page-num)))
          (when (btree-delete-from-leaf tree root-page member-key-bytes)
            (decf (cie-count entry))
            (decf (meta-entry-count (btree-meta tree)))
            (setf (btree-dirty tree) t)
            t))))))

(defun index-smembers (entry)
  "Return all members as a list by scanning the B+tree."
  (let* ((result nil)
         (tree (cie-tree entry))
         (root (meta-score-root (btree-meta tree))))
    (when (> root 0)
      (btree-map-entries tree root
        (lambda (key-bytes entry-bytes)
          (declare (ignore key-bytes))
          (multiple-value-bind (status member-bytes)
              (decode-set-entry entry-bytes 0)
            (when (= status #x01)
              (push (flexi-streams:octets-to-string member-bytes
                                                    :external-format :utf-8)
                    result))))))
    (nreverse result)))

(defun index-scard (entry)
  "Return member count."
  (cie-count entry))

;;; Hash operations via B+tree

(defun index-hset (entry field value)
  "Set a hash field. Always returns 1 (Redis compatibility)."
  (multiple-value-bind (field-str field-key-bytes)
      (member-to-key-bytes field)
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree)))
           (existing (btree-lookup-entry tree root field-key-bytes))
           (is-new (not existing)))
      (let ((value-bytes (encode-value-to-bytes value))
            (entry-bytes (encode-hash-entry #x01
                                            (value-to-key-bytes field-str)
                                            (encode-value-to-bytes value))))
        (setf (meta-score-root (btree-meta tree))
              (btree-insert-entry tree root field-key-bytes entry-bytes))
        (when is-new
          (incf (cie-count entry))
          (incf (meta-entry-count (btree-meta tree))))
        (setf (btree-dirty tree) t))
      1)))

(defun index-hget (entry field)
  "Get a hash field value by looking up in the B+tree."
  (multiple-value-bind (field-str field-key-bytes)
      (member-to-key-bytes field)
    (declare (ignore field-str))
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree))))
      (when (> root 0)
        (let ((entry-bytes (btree-lookup-entry tree root field-key-bytes)))
          (when entry-bytes
            (multiple-value-bind (status field-bytes value-bytes)
                (decode-hash-entry entry-bytes 0)
              (declare (ignore status field-bytes))
              (decode-hash-value value-bytes))))))))

(defun index-hdel (entry field)
  "Delete a hash field. Returns T if found."
  (multiple-value-bind (field-str field-key-bytes)
      (member-to-key-bytes field)
    (declare (ignore field-str))
    (let* ((tree (cie-tree entry))
           (root-page-num (meta-score-root (btree-meta tree))))
      (when (> root-page-num 0)
        (let ((root-page (btree-read-page tree root-page-num)))
          (when (btree-delete-from-leaf tree root-page field-key-bytes)
            (decf (cie-count entry))
            (decf (meta-entry-count (btree-meta tree)))
            (setf (btree-dirty tree) t)
            t))))))

(defun index-hgetall (entry)
  "Return flat list of (field1 value1 field2 value2 ...) by scanning B+tree."
  (let* ((result nil)
         (tree (cie-tree entry))
         (root (meta-score-root (btree-meta tree))))
    (when (> root 0)
      (btree-map-entries tree root
        (lambda (key-bytes entry-bytes)
          (declare (ignore key-bytes))
          (multiple-value-bind (status field-bytes value-bytes)
              (decode-hash-entry entry-bytes 0)
            (when (= status #x01)
              (let ((field (flexi-streams:octets-to-string field-bytes
                                                           :external-format :utf-8))
                    (value (decode-hash-value value-bytes)))
                (push value result)
                (push field result)))))))
    (nreverse result)))

(defun index-hkeys (entry)
  "Return all field names by scanning B+tree."
  (let* ((result nil)
         (tree (cie-tree entry))
         (root (meta-score-root (btree-meta tree))))
    (when (> root 0)
      (btree-map-entries tree root
        (lambda (key-bytes entry-bytes)
          (declare (ignore key-bytes))
          (multiple-value-bind (status field-bytes)
              (decode-hash-entry entry-bytes 0)
            (when (= status #x01)
              (push (flexi-streams:octets-to-string field-bytes
                                                    :external-format :utf-8)
                    result))))))
    (nreverse result)))

(defun index-hvals (entry)
  "Return all field values by scanning B+tree."
  (let* ((result nil)
         (tree (cie-tree entry))
         (root (meta-score-root (btree-meta tree))))
    (when (> root 0)
      (btree-map-entries tree root
        (lambda (key-bytes entry-bytes)
          (declare (ignore key-bytes))
          (multiple-value-bind (status field-bytes value-bytes)
              (decode-hash-entry entry-bytes 0)
            (declare (ignore field-bytes))
            (when (= status #x01)
              (push (decode-hash-value value-bytes) result))))))
    (nreverse result)))

(defun index-hexists (entry field)
  "Check if hash field exists by looking up in B+tree."
  (multiple-value-bind (field-str field-key-bytes)
      (member-to-key-bytes field)
    (declare (ignore field-str))
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree))))
      (when (> root 0)
        (btree-lookup-entry tree root field-key-bytes)))))

;;; Sorted set operations via B+tree + skiplist

(defun index-zadd (entry score member)
  "Add member with score to sorted set. Returns T if new."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree)))
           (existing (btree-lookup-entry tree root member-key-bytes)))
      (if existing
          ;; Update existing - remove old score from skiplist, add new
          (progn
            ;; Decode old entry to get old score
            (multiple-value-bind (old-status old-score old-member-bytes)
                (decode-zset-leaf-entry existing 0)
              (declare (ignore old-status old-member-bytes))
              ;; Remove old score entry from skiplist
              (let ((old-key (make-zset-key old-score member-key-bytes)))
                (skiplist-delete (cie-score-tree entry) old-key)))
            ;; Insert new entry into B+tree
            (let ((entry-bytes (encode-zset-leaf-entry #x01 score member-key-bytes)))
              (setf (meta-score-root (btree-meta tree))
                    (btree-insert-entry tree root member-key-bytes entry-bytes))
              (setf (btree-dirty tree) t))
            ;; Add new score entry to skiplist
            (let ((new-key (make-zset-key score member-key-bytes)))
              (skiplist-insert (cie-score-tree entry) new-key t))
            nil) ; not new
          ;; New member
          (progn
            ;; Insert into B+tree
            (let ((entry-bytes (encode-zset-leaf-entry #x01 score member-key-bytes)))
              (setf (meta-score-root (btree-meta tree))
                    (btree-insert-entry tree root member-key-bytes entry-bytes))
              (incf (cie-count entry))
              (incf (meta-entry-count (btree-meta tree)))
              (setf (btree-dirty tree) t))
            ;; Add to skiplist
            (let ((key (make-zset-key score member-key-bytes)))
              (skiplist-insert (cie-score-tree entry) key t))
            t)))))

(defun index-zscore (entry member)
  "Get score for a sorted set member by looking up in B+tree."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (declare (ignore member-str))
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree))))
      (when (> root 0)
        (let ((entry-bytes (btree-lookup-entry tree root member-key-bytes)))
          (when entry-bytes
            (multiple-value-bind (status score member-bytes)
                (decode-zset-leaf-entry entry-bytes 0)
              (declare (ignore member-bytes))
              (when (= status #x01)
                (if (= score (coerce score 'single-float))
                    (coerce score 'single-float)
                    score)))))))))

(defun index-zrange (entry start stop &optional withscores)
  "Get range from sorted set by rank. Uses skiplist for score ordering."
  (let ((all nil))
    (skiplist-map (cie-score-tree entry)
                  (lambda (key val)
                    (declare (ignore val))
                    (push (cons (car key) (cdr key)) all)))
    (setf all (nreverse all))
    (let ((len (length all)))
      (when (< start 0) (setf start (max 0 (+ len start))))
      (when (< stop 0) (setf stop (+ len stop)))
      (setf start (max 0 (min start (1- len))))
      (setf stop (max 0 (min stop (1- len))))
      (when (<= start stop)
        (let ((result '()))
          (loop for i from start to stop
                for entry = (nth i all)
                do (if withscores
                       (progn
                         (push (flexi-streams:octets-to-string (cdr entry) :external-format :utf-8) result)
                         (let ((score (car entry)))
                           (push (if (= score (coerce score 'single-float))
                                     (coerce score 'single-float)
                                     score)
                                 result)))
                       (push (flexi-streams:octets-to-string (cdr entry) :external-format :utf-8) result)))
          (nreverse result))))))

(defun index-zrem (entry member)
  "Remove a member from sorted set. Returns T if found."
  (multiple-value-bind (member-str member-key-bytes)
      (member-to-key-bytes member)
    (declare (ignore member-str))
    (let* ((tree (cie-tree entry))
           (root (meta-score-root (btree-meta tree))))
      (when (> root 0)
        (let ((entry-bytes (btree-lookup-entry tree root member-key-bytes)))
          (when entry-bytes
            (multiple-value-bind (status score member-bytes)
                (decode-zset-leaf-entry entry-bytes 0)
              (declare (ignore status member-bytes))
              ;; Remove from skiplist
              (let ((key (make-zset-key score member-key-bytes)))
                (skiplist-delete (cie-score-tree entry) key))
              ;; Delete from B+tree
              (let ((root-page (btree-read-page tree root)))
                (btree-delete-from-leaf tree root-page member-key-bytes)
                (decf (cie-count entry))
                (decf (meta-entry-count (btree-meta tree)))
                (setf (btree-dirty tree) t))
              t)))))))

(defun index-zcard (entry)
  "Return sorted set cardinality."
  (cie-count entry))

(defun index-zincrby (entry increment member)
  "Increment score. Returns new score."
  (let* ((current (index-zscore entry member))
         (new-score (+ (if current
                           (if (numberp current) current 0)
                           0)
                       increment)))
    (index-zadd entry new-score member)
    (if (= new-score (coerce new-score 'single-float))
        (coerce new-score 'single-float)
        new-score)))

(defun index-zrange-withscores (entry start stop)
  "Get range with scores as flat list (member1 score1 member2 score2 ...)."
  (index-zrange entry start stop t))

(defun index-zrevrange (entry start stop &optional withscores)
  "Get reverse range."
  (let ((all nil))
    (skiplist-map (cie-score-tree entry)
                  (lambda (key val)
                    (declare (ignore val))
                    (push (cons (car key) (cdr key)) all)))
    ;; all is already in descending order from push
    (let ((len (length all)))
      (when (< start 0) (setf start (max 0 (+ len start))))
      (when (< stop 0) (setf stop (+ len stop)))
      (setf start (max 0 (min start (1- len))))
      (setf stop (max 0 (min stop (1- len))))
      (when (<= start stop)
        (let ((result '()))
          (loop for i from start to stop
                for entry = (nth i all)
                do (if withscores
                       (progn
                         (push (flexi-streams:octets-to-string (cdr entry) :external-format :utf-8) result)
                         (let ((score (car entry)))
                           (push (if (= score (coerce score 'single-float))
                                     (coerce score 'single-float)
                                     score)
                                 result)))
                       (push (flexi-streams:octets-to-string (cdr entry) :external-format :utf-8) result)))
          (nreverse result))))))

;;; Set algebra operations via B+tree scans

(defun index-sunion (database keys)
  "Union of sets. Returns list of all members across all sets."
  (let ((result (make-hash-table :test #'equal)))
    (dolist (k keys)
      (let ((entry (gethash k (get-collection-index database))))
        (when entry
          (let ((tree (cie-tree entry))
                (root (meta-score-root (btree-meta (cie-tree entry)))))
            (when (> root 0)
              (btree-map-entries tree root
                (lambda (key-bytes entry-bytes)
                  (declare (ignore key-bytes))
                  (multiple-value-bind (status member-bytes)
                      (decode-set-entry entry-bytes 0)
                    (when (= status #x01)
                      (let ((member (flexi-streams:octets-to-string member-bytes
                                                                    :external-format :utf-8)))
                        (setf (gethash member result) t)))))))))))
    (hash-table-keys result)))

(defun index-sinter (database keys)
  "Intersection of sets. Returns members common to all sets."
  (when keys
    (let ((first-members nil)
          (rest-keys (cdr keys)))
      (let ((entry (gethash (car keys) (get-collection-index database))))
        (when entry
          (setf first-members (make-hash-table :test #'equal))
          (let ((tree (cie-tree entry))
                (root (meta-score-root (btree-meta (cie-tree entry)))))
            (when (> root 0)
              (btree-map-entries tree root
                (lambda (key-bytes entry-bytes)
                  (declare (ignore key-bytes))
                  (multiple-value-bind (status member-bytes)
                      (decode-set-entry entry-bytes 0)
                    (when (= status #x01)
                      (let ((member (flexi-streams:octets-to-string member-bytes
                                                                    :external-format :utf-8)))
                        (setf (gethash member first-members) t))))))))))
      (when first-members
        (dolist (k rest-keys)
          (let ((entry (gethash k (get-collection-index database))))
            (when entry
              (let ((new-set (make-hash-table :test #'equal))
                    (tree (cie-tree entry))
                    (root (meta-score-root (btree-meta (cie-tree entry)))))
                (when (> root 0)
                  (btree-map-entries tree root
                    (lambda (key-bytes entry-bytes)
                      (declare (ignore key-bytes))
                      (multiple-value-bind (status member-bytes)
                          (decode-set-entry entry-bytes 0)
                        (when (= status #x01)
                          (let ((member (flexi-streams:octets-to-string member-bytes
                                                                        :external-format :utf-8)))
                            (when (gethash member first-members)
                               (setf (gethash member new-set) t))))))))
                (setf first-members new-set)))))
        (when first-members
          (hash-table-keys first-members))))))

(defun index-sdiff (database keys)
  "Difference of sets. Returns members in first set not in others."
  (when keys
    (let ((first-members nil)
          (rest-keys (cdr keys)))
      (let ((entry (gethash (car keys) (get-collection-index database))))
        (when entry
          (setf first-members (make-hash-table :test #'equal))
          (let ((tree (cie-tree entry))
                (root (meta-score-root (btree-meta (cie-tree entry)))))
            (when (> root 0)
              (btree-map-entries tree root
                (lambda (key-bytes entry-bytes)
                  (declare (ignore key-bytes))
                  (multiple-value-bind (status member-bytes)
                      (decode-set-entry entry-bytes 0)
                    (when (= status #x01)
                      (let ((member (flexi-streams:octets-to-string member-bytes
                                                                    :external-format :utf-8)))
                        (setf (gethash member first-members) t))))))))))
      (when first-members
        (dolist (k rest-keys)
          (let ((entry (gethash k (get-collection-index database))))
            (when entry
              (let ((tree (cie-tree entry))
                    (root (meta-score-root (btree-meta (cie-tree entry)))))
                (when (> root 0)
                  (btree-map-entries tree root
                    (lambda (key-bytes entry-bytes)
                      (declare (ignore key-bytes))
                      (multiple-value-bind (status member-bytes)
                          (decode-set-entry entry-bytes 0)
                        (when (= status #x01)
                          (let ((member (flexi-streams:octets-to-string member-bytes
                                                                        :external-format :utf-8)))
                            (remhash member first-members)))))))))))
        (when first-members
          (hash-table-keys first-members))))))

;;; Set scan via B+tree

(defun index-sscan (entry cursor &optional pattern count)
  "Scan set members with cursor. Returns (values result next-cursor)."
  (let ((members (index-smembers entry))
        (remaining (or count 10))
        (result '())
        (next-cursor nil))
    (loop for i from (or cursor 0)
          for member in (nthcdr (or cursor 0) members)
          while (> remaining 0)
          when (or (null pattern)
                   (cl-ppcre:scan pattern (format nil "~a" member)))
          do (push member result)
             (decf remaining)
             (when (= remaining 0)
               (setf next-cursor (1+ i))))
    (list (reverse result) next-cursor)))

;;; Hash scan via B+tree

(defun index-hscan (entry cursor &optional pattern count)
  "Scan hash fields with cursor. Returns flat list (field1 val1 field2 val2 ...)."
  (let ((all-pairs (index-hgetall entry))
        (remaining (or count 10))
        (result '())
        (next-cursor nil))
    (loop for i from (or cursor 0)
          for (field val . rest) on (nthcdr (* 2 (or cursor 0)) all-pairs) by #'cddr
          while (and field val (> remaining 0))
          when (or (null pattern)
                   (cl-ppcre:scan pattern (format nil "~a" field)))
          do (push val result)
             (push field result)
             (decf remaining)
             (when (= remaining 0)
               (setf next-cursor (1+ i))))
    (list (reverse result) next-cursor)))

;;; Hash incrby via B+tree

(defun index-hincrby (entry field increment)
  "Increment a hash field by increment. Returns new value."
  (let* ((current (index-hget entry field))
         (new-val (+ (if current
                         (if (numberp current) current 0)
                         0)
                     increment)))
    (index-hset entry field new-val)
    new-val))

;;; Sorted set scan via B+tree

(defun index-zscan (entry cursor &optional pattern count)
  "Scan sorted set members with cursor. Returns flat list (member1 score1 member2 score2 ...)."
  (let ((all nil))
    (skiplist-map (cie-score-tree entry)
                  (lambda (key val)
                    (declare (ignore val))
                    (push (cons (car key) (cdr key)) all)))
    (setf all (nreverse all))
    (let ((remaining (or count 10))
          (result '())
          (next-cursor nil))
      (loop for i from (or cursor 0)
            for pair in (nthcdr (or cursor 0) all)
            while (> remaining 0)
            when (or (null pattern)
                     (cl-ppcre:scan pattern (format nil "~a"
                                                    (flexi-streams:octets-to-string (cdr pair)
                                                                                    :external-format :utf-8))))
            do (push (flexi-streams:octets-to-string (cdr pair) :external-format :utf-8) result)
               (push (let ((score (car pair)))
                       (if (= score (coerce score 'single-float))
                           (coerce score 'single-float)
                           score))
                     result)
               (decf remaining)
               (when (= remaining 0)
                 (setf next-cursor (1+ i))))
      (list (reverse result) next-cursor))))

(defun encode-value-to-bytes (value)
  "Encode a value to bytes using the typed encoding."
  (let ((output-stream (flexi-streams:make-in-memory-output-stream)))
    (let ((flex (flexi-streams:make-flexi-stream output-stream
                                                   :external-format :utf-8)))
      (encode-value-typed flex value)
      (flexi-streams:get-output-stream-sequence output-stream))))

(defun decode-hash-value (value-bytes)
  "Decode a typed value from a byte vector."
  (let ((stream (flexi-streams:make-flexi-stream
                 (flexi-streams:make-in-memory-input-stream value-bytes)
                 :external-format :utf-8)))
    (decode-value-type stream)))

;;; Flush collection to disk

(defun flush-collection (entry)
  "Flush a collection's B+tree to disk."
  (when (and (cie-tree entry) (btree-dirty (cie-tree entry)))
    (btree-flush-to-file (cie-tree entry))))

(defun flush-all-collections (database)
  "Flush all collection B+trees to disk."
    (maphash (lambda (key entry)
               (declare (ignore key))
               (flush-collection entry))
              (get-collection-index database)))
