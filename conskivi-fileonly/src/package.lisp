;;; -*- lisp -*-

(defpackage #:conskivi-fileonly
  (:use #:cl)
  (:export
   ;; Database class
   #:conskivi-fileonly-database

   ;; Lifecycle
   #:conskivi-load
   #:conskivi-save
   #:conskivi-start
   #:conskivi-stop))

(in-package #:conskivi-fileonly)
