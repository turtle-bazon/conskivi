;;;; -*- mode: lisp -*-

(in-package #:conskivi-core)

;;; Infrastructure

(defclass conskivi-database ()
  ((name
    :initarg :name)))

;;; Basic operations

(defgeneric conskivi-put (database key value))
(defgeneric conskivi-get (database key))
(defgeneric conskivi-del (database key))

;;; Key operations

(defgeneric conskivi-exists (database key))
(defgeneric conskivi-type (database key))
(defgeneric conskivi-keys (database &optional pattern))
(defgeneric conskivi-scan (database cursor &optional pattern count))
(defgeneric conskivi-expire (database key seconds))
(defgeneric conskivi-ttl (database key))
(defgeneric conskivi-persist (database key))

;;; Transactions

(defgeneric conskivi-multi (database))
(defgeneric conskivi-exec (database))
(defgeneric conskivi-discard (database))

;;; Locking

(defgeneric conskivi-watch (database key))
(defgeneric conskivi-unwatch (database))

;;; Pub/Sub

(defgeneric conskivi-subscribe (database channel callback))
(defgeneric conskivi-unsubscribe (database channel))
(defgeneric conskivi-psubscribe (database pattern callback))
(defgeneric conskivi-punsubscribe (database pattern))
(defgeneric conskivi-publish (database channel message))

;;; Strings

(defgeneric conskivi-append (database key value))
(defgeneric conskivi-getset (database key value))
(defgeneric conskivi-setnx (database key value))
(defgeneric conskivi-mget (database keys))
(defgeneric conskivi-mset (database key-value-plist))

;;; Integers

(defgeneric conskivi-incr (database key))
(defgeneric conskivi-incrby (database key increment))
(defgeneric conskivi-decr (database key))
(defgeneric conskivi-decrby (database key decrement))

;;; Lists

(defgeneric conskivi-lpush (database key &rest values))
(defgeneric conskivi-rpush (database key &rest values))
(defgeneric conskivi-lpop (database key))
(defgeneric conskivi-rpop (database key))
(defgeneric conskivi-lrange (database key start stop))
(defgeneric conskivi-llen (database key))
(defgeneric conskivi-lindex (database key index))
(defgeneric conskivi-lset (database key index value))
(defgeneric conskivi-lscan (database key cursor &optional pattern count))

;;; Sets

(defgeneric conskivi-sadd (database key &rest members))
(defgeneric conskivi-srem (database key &rest members))
(defgeneric conskivi-smembers (database key))
(defgeneric conskivi-sismember (database key member))
(defgeneric conskivi-scard (database key))
(defgeneric conskivi-sunion (database keys))
(defgeneric conskivi-sinter (database keys))
(defgeneric conskivi-sdiff (database keys))
(defgeneric conskivi-sscan (database key cursor &optional pattern count))

;;; Sorted sets

(defgeneric conskivi-zadd (database key score member &rest score-member-pairs))
(defgeneric conskivi-zrem (database key &rest members))
(defgeneric conskivi-zrange (database key start stop &optional withscores))
(defgeneric conskivi-zrevrange (database key start stop &optional withscores))
(defgeneric conskivi-zscore (database key member))
(defgeneric conskivi-zcard (database key))
(defgeneric conskivi-zincrby (database key increment member))
(defgeneric conskivi-zscan (database key cursor &optional pattern count))

;;; Hashes

(defgeneric conskivi-hset (database key field value))
(defgeneric conskivi-hget (database key field))
(defgeneric conskivi-hdel (database key field))
(defgeneric conskivi-hgetall (database key))
(defgeneric conskivi-hkeys (database key))
(defgeneric conskivi-hvals (database key))
(defgeneric conskivi-hexists (database key field))
(defgeneric conskivi-hincrby (database key field increment))
(defgeneric conskivi-hscan (database key cursor &optional pattern count))