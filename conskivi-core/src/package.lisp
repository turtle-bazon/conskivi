;;;; -*- mode: lisp -*-

(defpackage #:conskivi-core
  (:use
   #:cl)
  (:export
   ;; Infrastructure
   #:conskivi-database

   ;; Basic operations
   #:conskivi-put
   #:conskivi-get
   #:conskivi-del

   ;; Key operations
   #:conskivi-exists
   #:conskivi-type
   #:conskivi-keys
   #:conskivi-scan
   #:conskivi-expire
   #:conskivi-ttl
   #:conskivi-persist

   ;; Transactions
   #:conskivi-multi
   #:conskivi-exec
   #:conskivi-discard

   ;; Locking
   #:conskivi-watch
   #:conskivi-unwatch

   ;; Pub/Sub
   #:conskivi-subscribe
   #:conskivi-unsubscribe
   #:conskivi-psubscribe
   #:conskivi-punsubscribe
   #:conskivi-publish

   ;; Strings
   #:conskivi-append
   #:conskivi-getset
   #:conskivi-setnx
   #:conskivi-mget
   #:conskivi-mset

   ;; Integers
   #:conskivi-incr
   #:conskivi-incrby
   #:conskivi-decr
   #:conskivi-decrby

   ;; Lists
   #:conskivi-lpush
   #:conskivi-rpush
   #:conskivi-lpop
   #:conskivi-rpop
   #:conskivi-lrange
   #:conskivi-llen
   #:conskivi-lindex
   #:conskivi-lset
   #:conskivi-lscan

   ;; Sets
   #:conskivi-sadd
   #:conskivi-srem
   #:conskivi-smembers
   #:conskivi-sismember
   #:conskivi-scard
   #:conskivi-sunion
   #:conskivi-sinter
   #:conskivi-sdiff
   #:conskivi-sscan

   ;; Sorted sets
   #:conskivi-zadd
   #:conskivi-zrem
   #:conskivi-zrange
   #:conskivi-zrevrange
   #:conskivi-zscore
   #:conskivi-zcard
   #:conskivi-zincrby
   #:conskivi-zscan

   ;; Hashes
   #:conskivi-hset
   #:conskivi-hget
   #:conskivi-hdel
   #:conskivi-hgetall
   #:conskivi-hkeys
   #:conskivi-hvals
   #:conskivi-hexists
   #:conskivi-hincrby
   #:conskivi-hscan))