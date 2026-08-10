;;; -*- lisp -*-

(defpackage #:conskivi-inmemory-tests
  (:use
   #:cl
   #:conskivi-core
   #:conskivi-inmemory
   #:iterate
   #:fiveam)
  (:export
   #:conskivi-run-all-tests)
  (:documentation "Cons Key Value InMemory (test package)"))

(in-package :conskivi-inmemory-tests)

(def-suite all-tests
  :description "All tests")

(def-suite basic-operations
  :description "Basic put/get/del operations"
  :in all-tests)

(def-suite key-operations
  :description "Key operations (exists, type, keys, scan, expire, ttl, persist)"
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