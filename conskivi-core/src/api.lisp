;;;; -*- mode: lisp -*-

(in-package #:conskivi-core)

(defclass conskivi-database ()
  ((name
    :initarg :name)))

(defgeneric conskivi-put (database key value)
  )

(defgeneric conskivi-get (database key)
  )

(defgeneric conskivi-del (database key)
  )
