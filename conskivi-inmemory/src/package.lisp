;;;; -*- mode: lisp -*-

(defpackage #:conskivi-inmemory
  (:use
   #:cl
   #:conskivi-core
   #:iterate
   #:metabang-bind)
  (:export
   ;; Database class
   #:conskivi-inmemory-database

   ;; Configuration
   #:conskivi-inmemory-stripes-count
   #:conskivi-inmemory-type-threshold
   #:conskivi-inmemory-ttl-interval

   ;; Lifecycle
   #:conskivi-load
   #:conskivi-save
   #:conskivi-start
   #:conskivi-stop))