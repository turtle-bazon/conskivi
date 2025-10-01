;;; -*- lisp -*-

(defpackage #:conskivi-inmemory-tests
  (:use
   #:cl
   #:conskivi-core
   #:conskivi-inmemory
   #:iterate
   #:fiveam)
  (:export
   #:run-all-tests)
  (:documentation "Cons Key Value InMemory (test package)"))

(in-package :conskivi-inmemory-tests)

(def-suite all-tests
  :description "All tests")
