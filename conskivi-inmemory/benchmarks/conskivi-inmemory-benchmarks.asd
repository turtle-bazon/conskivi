;;;; -*- mode: lisp -*-

(asdf:defsystem #:conskivi-inmemory-benchmarks
  :description "Benchmarks for conskivi-inmemory vs Redis"
  :depends-on (#:conskivi-core
               #:conskivi-inmemory
               #:cl-redis
               #:bordeaux-threads)
  :components ((:file "benchmark")))
