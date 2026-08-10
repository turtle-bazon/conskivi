;;; -*- lisp -*-

(in-package :conskivi-inmemory-tests)

(def-test string-append (:suite string-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "hello")
    (let ((len (conskivi-append db :key1 " world")))
      (is (= len 11))
      (is (equal (conskivi-get db :key1) "hello world")))))

(def-test string-append-new (:suite string-operations)
  (with-fixture test-database ()
    (let ((len (conskivi-append db :key1 "hello")))
      (is (= len 5))
      (is (equal (conskivi-get db :key1) "hello")))))

(def-test string-getset (:suite string-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "old")
    (let ((old (conskivi-getset db :key1 "new")))
      (is (equal old "old"))
      (is (equal (conskivi-get db :key1) "new")))))

(def-test string-setnx (:suite string-operations)
  (with-fixture test-database ()
    (is (conskivi-setnx db :key1 "value1"))
    (is (not (conskivi-setnx db :key1 "value2")))
    (is (equal (conskivi-get db :key1) "value1"))))

(def-test string-mget (:suite string-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "value1")
    (conskivi-put db :key2 "value2")
    (let ((values (conskivi-mget db (list :key1 :key2 :key3))))
      (is (equal (first values) "value1"))
      (is (equal (second values) "value2"))
      (is (null (third values))))))

(def-test string-mset (:suite string-operations)
  (with-fixture test-database ()
    (conskivi-mset db (list :key1 "value1" :key2 "value2"))
    (is (equal (conskivi-get db :key1) "value1"))
    (is (equal (conskivi-get db :key2) "value2"))))