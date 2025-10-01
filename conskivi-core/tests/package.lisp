;;; -*- lisp -*-

(defpackage :conskivi-core-tests
  (:use
   :cl
   :conskivi-core
   :iterate
   :fiveam)
  (:export
   :run-all-tests)
  (:documentation "Cons Key Value Core (test package)"))

(in-package :conskivi-core-tests)

(def-suite all-tests
  :description "All tests")
