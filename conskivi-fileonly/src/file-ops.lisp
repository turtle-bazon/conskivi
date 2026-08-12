;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; Key encoding for filesystem

(defun encode-key-for-file (key)
  "Convert a key to a safe filename."
  (let ((name (if (symbolp key)
                  (symbol-name key)
                  (format nil "~a" key))))
    (let ((result (make-array (length name) :element-type 'character :adjustable t :fill-pointer 0)))
      (loop for ch across name
            do (cond
                 ((or (alphanumericp ch)
                      (char= ch #\-)
                      (char= ch #\_)
                      (char= ch #\.))
                  (vector-push-extend ch result))
                 (t
                  (vector-push-extend #\_ result)
                  (vector-push-extend (char (format nil "~2,'0X" (char-code ch)) 0) result)
                  (vector-push-extend (char (format nil "~2,'0X" (char-code ch)) 1) result))))
      (concatenate 'string result ".ck"))))

(defun decode-key-from-file (filename)
  "Convert a filename back to a key string."
  (let ((name (subseq filename 0 (- (length filename) 3))))
    (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
          (i 0))
      (loop while (< i (length name))
            do (let ((ch (char name i)))
                 (if (char= ch #\_)
                     (progn
                       (let* ((hex1 (char name (+ i 1)))
                              (hex2 (char name (+ i 2)))
                              (code (+ (* 16 (digit-char-p hex1 16))
                                       (digit-char-p hex2 16))))
                         (vector-push-extend (code-char code) result))
                       (incf i 3))
                     (progn
                       (vector-push-extend ch result)
                       (incf i 1)))))
      result)))

;;; File path helpers

(defun key-file-path (database key)
  "Get the file path for a given key."
  (let* ((db-path (slot-value database 'db-path))
         (encoded (encode-key-for-file key))
         (dot-pos (position #\. encoded :from-end t))
         (name-part (subseq encoded 0 dot-pos))
         (type-part (subseq encoded (1+ dot-pos))))
    (merge-pathnames (make-pathname :name name-part
                                    :type type-part)
                     db-path)))

;;; File I/O operations

(defun read-key-file (database key)
  "Read key data from its file. Returns (values value type expiration) or nil."
  (let ((path (key-file-path database key)))
    (when (probe-file path)
      (with-open-file (stream path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (when stream
          (decode-key-data stream))))))

(defun write-key-file (database key value type expiration)
  "Write key data to its file."
  (let* ((path (key-file-path database key))
         (temp-path (merge-pathnames (make-pathname :name (encode-key-for-file key)
                                                    :type "tmp")
                                     (slot-value database 'db-path))))
    (ensure-directories-exist path)
    (with-open-file (stream temp-path
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (encode-key-data stream value type expiration))
    (rename-file temp-path path)))

(defun delete-key-file (database key)
  "Delete a key's file."
  (let ((path (key-file-path database key)))
    (when (probe-file path)
      (delete-file path))))

(defun list-all-keys (database)
  "List all keys in the database directory."
  (let ((db-path (slot-value database 'db-path)))
    (mapcar (lambda (k) (intern (string-upcase k) :keyword))
            (mapcar #'decode-key-from-file
                    (mapcar (lambda (p) (format nil "~a.~a" (pathname-name p) (pathname-type p)))
                            (directory (merge-pathnames #p"*.ck" db-path)))))))
