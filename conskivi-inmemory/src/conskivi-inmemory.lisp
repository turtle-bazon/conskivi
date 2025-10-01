;;;; -*- mode: lisp -*-

(in-package #:conskivi-inmemory)

(defclass conskivi-inmemory-database (conskivi-database)
  ((data
    :initform (make-hash-table :test #'equal))
   (data-lock
    :initform (bt2:make-lock :name "conskivi-inmemory-database-lock"))
   (db-path
    :initarg :db-path
    :initform (error "db-path must be defined"))
   (state
    :initform :stopped)
   (periodical-save-thread)))

(defmethod conskivi-load ((database conskivi-inmemory-database))
  (bind (((:slots data data-lock db-path) database))
    (when (probe-file db-path)
      (with-open-file (stream db-path :direction :input)
        (iter
          (for key-value = (read stream nil nil))
          (while key-value)
          (for key = (car key-value))
          (for value = (cdr key-value))
          (setf (gethash key data) value)
          (format t "~A, ~a, ~a~%" key-value key value))))))

(defmethod conskivi-save ((database conskivi-inmemory-database))
  (bind (((:slots data data-lock db-path) database))
    (bt2:with-lock-held (data-lock)
      (with-open-file (stream db-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
        (maphash
         (lambda (key value)
           (write (cons key value) :stream stream))
         data)))))

(defmethod conskivi-put ((database conskivi-inmemory-database) key value)
  (bind (((:slots data data-lock) database))
    (bt2:with-lock-held (data-lock)
      (setf (gethash key data) value))))

(defmethod conskivi-get ((database conskivi-inmemory-database) key)
  (bind (((:slots data data-lock) database))
    (bt2:with-lock-held (data-lock)
      (gethash key data))))

(defmethod conskivi-del ((database conskivi-inmemory-database) key)
  (bind (((:slots data data-lock) database))
    (bt2:with-lock-held (data-lock)
      (remhash key data))))
