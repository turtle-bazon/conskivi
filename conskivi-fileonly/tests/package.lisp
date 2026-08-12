;;; -*- lisp -*-

(defpackage #:conskivi-fileonly-tests
  (:use
   #:cl
   #:conskivi-core
   #:conskivi-fileonly
   #:iterate
   #:fiveam)
  (:export
    #:conskivi-run-all-tests)
  (:documentation "Cons Key Value FileOnly (test package)"))

(in-package :conskivi-fileonly-tests)

(defun delete-directory (path)
  (dolist (entry (directory (merge-pathnames #p"*.*" path)))
    (if (pathname-name entry)
        (delete-file entry)
        (delete-directory entry)))
  (sb-ext:delete-directory path))

(def-suite all-tests
  :description "All tests")

(def-suite basic-operations
  :description "Basic put/get/del operations"
  :in all-tests)

(def-suite key-operations
  :description "Key operations"
  :in all-tests)

(def-suite string-operations
  :description "String operations"
  :in all-tests)

(def-suite integer-operations
  :description "Integer operations"
  :in all-tests)

(def-suite list-operations
  :description "List operations"
  :in all-tests)

(def-suite set-operations
  :description "Set operations"
  :in all-tests)

(def-suite sorted-set-operations
  :description "Sorted set operations"
  :in all-tests)

(def-suite hash-operations
  :description "Hash operations"
  :in all-tests)

(def-suite pubsub-operations
  :description "Pub/Sub operations"
  :in all-tests)

(def-suite transaction-operations
  :description "Transaction operations"
  :in all-tests)

(defun conskivi-run-all-tests ()
  (run! 'all-tests))
