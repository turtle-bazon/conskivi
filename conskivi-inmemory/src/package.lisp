;;;; -*- mode: lisp -*-

(defpackage #:conskivi-inmemory
  (:use
   #:cl
   #:conskivi-core
   #:iterate
   #:metabang-bind)
  (:export 
   #:conskivi-inmemory-database
   #:conskivi-load
   #:conskivi-save))
