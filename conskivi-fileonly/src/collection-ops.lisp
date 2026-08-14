;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; Compound type append-only log format:
;;;   [type_tag(1)][expiration(8)][count(4)][entry1]...[entryN]
;;; Each entry:
;;;   [status(1)][data...]
;;;   status = #x01 active, #x00 tombstone
;;;
;;; This avoids full file rewrites on every mutation.

;;; ---- Compound header I/O ----

(defun write-compound-header (stream type expiration count)
  (write-byte type stream)
  (write-i64 stream (or expiration 0))
  (write-u32 stream count))

(defun read-compound-header (stream)
  (values (read-u8 stream)    ; type tag
          (read-i64 stream)   ; expiration
          (read-u32 stream))) ; count

(defun update-compound-header-count (file-path new-count)
  (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                     :direction :output
                                     :if-exists :overwrite)
    (file-position stream 9)
    (write-u32 stream new-count)))

;;; ---- Set operations ----

(defun file-append-set-member (file-path expiration member)
  "Append a member to set file. Returns T if new."
  (if (probe-file file-path)
      (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                        :direction :io
                                        :if-does-not-exist nil)
        (when stream
          (multiple-value-bind (type exp count) (read-compound-header stream)
            (declare (ignore type exp))
            (let ((mkey (if (stringp member) member
                            (if (symbolp member) (symbol-name member)
                                (format nil "~a" member)))))
              (let ((exists nil))
                (loop while (< (file-position stream) (file-length stream))
                      for status = (read-u8 stream)
                      for val = (decode-value-type stream)
                      for vkey = (if (stringp val) val
                                     (if (symbolp val) (symbol-name val)
                                         (format nil "~a" val)))
                      when (and (= status #x01) (equal vkey mkey))
                        do (setf exists t))
                (unless exists
                  (file-position stream (file-length stream))
                  (write-byte #x01 stream)
                  (encode-value-typed stream member)
                  (update-compound-header-count file-path (1+ count))
                  t))))))
      ;; New file
      (progn
        (ensure-directories-exist file-path)
        (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
          (write-compound-header stream +type-set+ expiration 1)
          (write-byte #x01 stream)
          (encode-value-typed stream member)
          t))))

(defun file-read-set-members (file-path)
  "Read all active members from set file."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (multiple-value-bind (type exp count) (read-compound-header stream)
          (declare (ignore type exp count))
          (let ((result '()))
            (loop while (< (file-position stream) (file-length stream))
                  for status = (read-u8 stream)
                  for val = (decode-value-type stream)
                  when (= status #x01)
                    do (push val result))
            (nreverse result)))))))

(defun file-set-member-count (file-path)
  "Count active members."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (multiple-value-bind (type exp count) (read-compound-header stream)
          (declare (ignore type exp))
          (let ((active 0))
            (loop while (< (file-position stream) (file-length stream))
                  for status = (read-u8 stream)
                  for val = (decode-value-type stream)
                  when (= status #x01) do (incf active))
            active))))))

(defun file-is-set-member (file-path member)
  "Check if member is in set."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((mkey (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for vkey = (if (stringp val) val
                               (if (symbolp val) (symbol-name val)
                                   (format nil "~a" val)))
                when (and (= status #x01) (equal vkey mkey))
                  do (return t)
                finally (return nil)))))))

(defun file-mark-set-deleted (file-path member)
  "Mark a member as tombstone."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :direction :io
                                      :if-does-not-exist nil)
      (when stream
        (let ((mkey (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
          (file-position stream 13)
          (loop while (< (file-position stream) (file-length stream))
                for pos = (file-position stream)
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for vkey = (if (stringp val) val
                               (if (symbolp val) (symbol-name val)
                                   (format nil "~a" val)))
                when (and (= status #x01) (equal vkey mkey))
                  do (file-position stream pos)
                     (write-byte #x00 stream)
                     (return t)))))))

;;; ---- Hash operations ----

(defun file-hash-set-field (file-path expiration field value)
  "Set a hash field. Returns 1."
  (if (probe-file file-path)
      (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                        :direction :io
                                        :if-does-not-exist nil)
        (when stream
          (multiple-value-bind (type exp count) (read-compound-header stream)
            (declare (ignore type exp))
            (let ((fkey (if (stringp field) field
                            (if (symbolp field) (symbol-name field)
                                (format nil "~a" field)))))
              (let ((found-pos nil))
                (loop while (< (file-position stream) (file-length stream))
                      for pos = (file-position stream)
                      for status = (read-u8 stream)
                      for fv = (decode-value-type stream)
                      for fvkey = (if (stringp fv) fv
                                      (if (symbolp fv) (symbol-name fv)
                                          (format nil "~a" fv)))
                      do (progn
                           (decode-value-type stream)
                           (when (and (= status #x01) (equal fvkey fkey))
                             (setf found-pos pos))))
                (when found-pos
                  (file-position stream found-pos)
                  (write-byte #x00 stream))
                (file-position stream (file-length stream))
                (write-byte #x01 stream)
                (encode-value-typed stream field)
                (encode-value-typed stream value)
                (unless found-pos
                  (update-compound-header-count file-path (1+ count)))
                1)))))
      ;; New file
      (progn
        (ensure-directories-exist file-path)
        (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
          (write-compound-header stream +type-hash+ expiration 1)
          (write-byte #x01 stream)
          (encode-value-typed stream field)
          (encode-value-typed stream value)
          1))))

(defun file-hash-get-field (file-path field)
  "Get a hash field value."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((fkey (if (stringp field) field
                        (if (symbolp field) (symbol-name field)
                            (format nil "~a" field)))))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for fv = (decode-value-type stream)
                for fvkey = (if (stringp fv) fv
                                (if (symbolp fv) (symbol-name fv)
                                    (format nil "~a" fv)))
                when (= status #x01)
                  do (let ((val (decode-value-type stream)))
                       (when (equal fvkey fkey)
                         (return val)))
                else do (decode-value-type stream)
                finally (return nil)))))))

(defun file-hash-del-field (file-path field)
  "Delete a hash field. Returns T if found."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :direction :io
                                      :if-does-not-exist nil)
      (when stream
        (let ((fkey (if (stringp field) field
                        (if (symbolp field) (symbol-name field)
                            (format nil "~a" field)))))
          (file-position stream 13)
          (loop while (< (file-position stream) (file-length stream))
                for pos = (file-position stream)
                for status = (read-u8 stream)
                for fv = (decode-value-type stream)
                for fvkey = (if (stringp fv) fv
                                (if (symbolp fv) (symbol-name fv)
                                    (format nil "~a" fv)))
                do (progn
                     (decode-value-type stream)
                     (when (and (= status #x01) (equal fvkey fkey))
                       (file-position stream pos)
                       (write-byte #x00 stream)
                       (return t)))))))))

(defun file-hash-exists-field (file-path field)
  "Check if hash field exists."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((fkey (if (stringp field) field
                        (if (symbolp field) (symbol-name field)
                            (format nil "~a" field)))))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for fv = (decode-value-type stream)
                for fvkey = (if (stringp fv) fv
                                (if (symbolp fv) (symbol-name fv)
                                    (format nil "~a" fv)))
                do (progn
                     (decode-value-type stream)
                     (when (and (= status #x01) (equal fvkey fkey))
                       (return t)))
                finally (return nil)))))))

(defun file-hash-get-all (file-path)
  "Get all hash field-value pairs as flat list (field1 value1 field2 value2 ...)."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((result '()))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for field = (decode-value-type stream)
                for value = (decode-value-type stream)
                when (= status #x01)
                  do (push value result)
                     (push field result))
          (nreverse result))))))

(defun file-hash-field-count (file-path)
  "Count active hash fields."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((count 0))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for fv = (decode-value-type stream)
                for val = (decode-value-type stream)
                when (= status #x01) do (incf count))
          count)))))

(defun file-hash-field-keys (file-path)
  "Get all active hash field keys."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((result '()))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for field = (decode-value-type stream)
                for val = (decode-value-type stream)
                when (= status #x01)
                  do (push field result))
          (nreverse result))))))

(defun file-hash-field-values (file-path)
  "Get all active hash field values."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((result '()))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for field = (decode-value-type stream)
                for val = (decode-value-type stream)
                when (= status #x01)
                  do (push val result))
          (nreverse result))))))

;;; ---- Sorted set operations ----

(defun file-zset-add (file-path expiration score member)
  "Add member to sorted set. Returns T if new, NIL if updated."
  (if (probe-file file-path)
      (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                        :direction :io
                                        :if-does-not-exist nil)
        (when stream
          (multiple-value-bind (type exp count) (read-compound-header stream)
            (declare (ignore type exp))
            (let ((mkey (if (stringp member) member
                            (if (symbolp member) (symbol-name member)
                                (format nil "~a" member)))))
              (let ((exists nil))
                (loop while (< (file-position stream) (file-length stream))
                      for pos = (file-position stream)
                      for status = (read-u8 stream)
                      for val = (decode-value-type stream)
                      for sc = (read-double stream)
                      for vkey = (if (stringp val) val
                                     (if (symbolp val) (symbol-name val)
                                         (format nil "~a" val)))
                      when (and (= status #x01) (equal vkey mkey))
                        do (setf exists t))
                (if exists
                    ;; Tombstone old entry and append new one
                    (progn
                      (file-position stream 13)
                      (loop while (< (file-position stream) (file-length stream))
                            for pos = (file-position stream)
                            for status = (read-u8 stream)
                            for val = (decode-value-type stream)
                            for sc = (read-double stream)
                            for vkey = (if (stringp val) val
                                           (if (symbolp val) (symbol-name val)
                                               (format nil "~a" val)))
                            when (and (= status #x01) (equal vkey mkey))
                              do (file-position stream pos)
                                 (write-byte #x00 stream)
                                 (return))
                      (file-position stream (file-length stream))
                      (write-byte #x01 stream)
                      (encode-value-typed stream member)
                      (write-double stream (coerce score 'double-float))
                      nil)
                    ;; New entry
                    (progn
                      (file-position stream (file-length stream))
                      (write-byte #x01 stream)
                      (encode-value-typed stream member)
                      (write-double stream (coerce score 'double-float))
                      (update-compound-header-count file-path (1+ count))
                      t)))))))
      ;; New file
      (progn
        (ensure-directories-exist file-path)
        (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
          (write-compound-header stream +type-zset+ expiration 1)
          (write-byte #x01 stream)
          (encode-value-typed stream member)
          (write-double stream (coerce score 'double-float))
          t))))

(defun file-zset-score (file-path member)
  "Get score for a sorted set member."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((mkey (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for sc = (read-double stream)
                for vkey = (if (stringp val) val
                               (if (symbolp val) (symbol-name val)
                                   (format nil "~a" val)))
                when (and (= status #x01) (equal vkey mkey))
                  do (return (coerce sc 'single-float))
                finally (return nil)))))))

(defun file-zset-card (file-path)
  "Count active sorted set members."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((count 0))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for sc = (read-double stream)
                when (= status #x01) do (incf count))
          count)))))

(defun file-zset-remove (file-path member)
  "Remove a member from sorted set. Returns T if found."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :direction :io
                                      :if-does-not-exist nil)
      (when stream
        (let ((mkey (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
          (file-position stream 13)
          (loop while (< (file-position stream) (file-length stream))
                for pos = (file-position stream)
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for sc = (read-double stream)
                for vkey = (if (stringp val) val
                               (if (symbolp val) (symbol-name val)
                                   (format nil "~a" val)))
                when (and (= status #x01) (equal vkey mkey))
                  do (file-position stream pos)
                     (write-byte #x00 stream)
                     (return t)))))))

(defun file-zset-range (file-path start stop &optional withscores)
  "Get range from sorted set."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                      :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((entries '()))
          (loop while (< (file-position stream) (file-length stream))
                for status = (read-u8 stream)
                for val = (decode-value-type stream)
                for sc = (read-double stream)
                when (= status #x01)
                  do (push (cons (coerce sc 'single-float) val) entries))
          (setf entries (sort entries #'< :key #'car))
          (let ((len (length entries)))
            (when (< start 0) (setf start (max 0 (+ len start))))
            (when (< stop 0) (setf stop (+ len stop)))
            (setf start (max 0 (min start (1- len))))
            (setf stop (max 0 (min stop (1- len))))
            (when (<= start stop)
              (let ((result '()))
                (loop for i from start to stop
                      for entry = (nth i entries)
                      do (if withscores
                             (progn
                               (push (cdr entry) result)
                               (push (car entry) result))
                             (push (cdr entry) result)))
                (nreverse result)))))))))

(defun file-zset-incrby (file-path increment member)
  "Increment score. Returns new score."
  (when (probe-file file-path)
    (with-open-file (stream file-path :element-type '(unsigned-byte 8)
                                       :direction :io
                                       :if-does-not-exist nil)
      (when stream
        (read-compound-header stream)
        (let ((mkey (if (stringp member) member
                        (if (symbolp member) (symbol-name member)
                            (format nil "~a" member)))))
          (let ((current-score 0.0)
                (found nil))
            (loop while (< (file-position stream) (file-length stream))
                  for status = (read-u8 stream)
                  for val = (decode-value-type stream)
                  for sc = (read-double stream)
                  for vkey = (if (stringp val) val
                                 (if (symbolp val) (symbol-name val)
                                     (format nil "~a" val)))
                  when (and (= status #x01) (equal vkey mkey))
                    do (setf current-score sc found t))
            (if found
                (let ((new-score (+ current-score increment)))
                  (file-position stream 13)
                  (loop while (< (file-position stream) (file-length stream))
                        for pos = (file-position stream)
                        for status = (read-u8 stream)
                        for val = (decode-value-type stream)
                        for sc = (read-double stream)
                        for vkey = (if (stringp val) val
                                       (if (symbolp val) (symbol-name val)
                                           (format nil "~a" val)))
                        when (and (= status #x01) (equal vkey mkey))
                          do (file-position stream pos)
                             (write-byte #x00 stream)
                             (return))
                  (file-position stream (file-length stream))
                  (write-byte #x01 stream)
                  (encode-value-typed stream member)
                  (write-double stream new-score)
                  (coerce new-score 'single-float))
                (let ((new-path (pathname stream)))
                  (file-position stream (file-length stream))
                  (write-byte #x01 stream)
                  (encode-value-typed stream member)
                  (write-double stream increment)
                  (update-compound-header-count
                   new-path
                   (1+ (multiple-value-bind (type exp count)
                            (progn (file-position stream 0)
                                   (read-compound-header stream))
                          (declare (ignore type exp))
                          count)))
                   (coerce increment 'single-float)))))))))
