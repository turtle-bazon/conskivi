;;;; -*- mode: lisp -*-
(asdf:defsystem #:conskivi-fileonly-benchmarks
  :description "Benchmarks for conskivi-fileonly vs Redis"
  :depends-on (#:conskivi-core
               #:conskivi-fileonly
               #:cl-redis
               #:bordeaux-threads)
  :components ((:file "benchmark")))
