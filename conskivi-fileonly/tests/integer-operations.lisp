;;; -*- lisp -*-

(in-package :conskivi-fileonly-tests)

(def-test integer-incr (:suite integer-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 0)
    (let ((result (conskivi-incr db :key1)))
      (is (= result 1))
      (is (= (conskivi-get db :key1) 1)))))

(def-test integer-incrby (:suite integer-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 0)
    (let ((result (conskivi-incrby db :key1 5)))
      (is (= result 5))
      (is (= (conskivi-get db :key1) 5)))))

(def-test integer-decr (:suite integer-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 10)
    (let ((result (conskivi-decr db :key1)))
      (is (= result 9))
      (is (= (conskivi-get db :key1) 9)))))

(def-test integer-decrby (:suite integer-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 10)
    (let ((result (conskivi-decrby db :key1 3)))
      (is (= result 7))
      (is (= (conskivi-get db :key1) 7)))))

(def-test integer-incr-new (:suite integer-operations)
  (with-fixture test-database ()
    (let ((result (conskivi-incr db :key1)))
      (is (= result 1))
      (is (= (conskivi-get db :key1) 1)))))

(def-test integer-incrby-new (:suite integer-operations)
  (with-fixture test-database ()
    (let ((result (conskivi-incrby db :key1 5)))
      (is (= result 5))
      (is (= (conskivi-get db :key1) 5)))))

(def-test integer-type-error (:suite integer-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "not an integer")
    (signals error (conskivi-incr db :key1))))