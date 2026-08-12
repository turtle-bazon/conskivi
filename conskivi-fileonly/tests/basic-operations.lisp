;;; -*- lisp -*-

(in-package :conskivi-fileonly-tests)

(def-fixture test-database ()
  (let ((db (make-instance 'conskivi-fileonly-database
                          :name "test-db"
                          :db-path #p"/tmp/test-conskivi-fileonly.db/")))
    (unwind-protect
         (&body)
      (when (probe-file #p"/tmp/test-conskivi-fileonly.db/")
        (when (probe-file #p"/tmp/test-conskivi-fileonly.db/") (delete-directory #p"/tmp/test-conskivi-fileonly.db/"))))))

(def-test basic-put-get (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "value1")
    (is (equal (conskivi-get db :key1) "value1"))))

(def-test basic-put-get-integer (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 42)
    (is (= (conskivi-get db :key1) 42))))

(def-test basic-put-get-float (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 3.14)
    (is (= (conskivi-get db :key1) 3.14))))

(def-test basic-put-get-keyword (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 :value)
    (is (eq (conskivi-get db :key1) :value))))

(def-test basic-put-get-boolean (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 t)
    (is (eq (conskivi-get db :key1) t))))

(def-test basic-put-get-nil (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 nil)
    (is (null (conskivi-get db :key1)))))

(def-test basic-put-overwrite (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "value1")
    (conskivi-put db :key1 "value2")
    (is (equal (conskivi-get db :key1) "value2"))))

(def-test basic-del (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "value1")
    (conskivi-del db :key1)
    (is (null (conskivi-get db :key1)))))

(def-test basic-del-nonexistent (:suite basic-operations)
  (with-fixture test-database ()
    (is (null (conskivi-del db :nonexistent)))))

(def-test basic-get-nonexistent (:suite basic-operations)
  (with-fixture test-database ()
    (is (null (conskivi-get db :nonexistent)))))

(def-test basic-multiple-keys (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :key1 "value1")
    (conskivi-put db :key2 "value2")
    (conskivi-put db :key3 "value3")
    (is (equal (conskivi-get db :key1) "value1"))
    (is (equal (conskivi-get db :key2) "value2"))
    (is (equal (conskivi-get db :key3) "value3"))))

(def-test basic-compound-types (:suite basic-operations)
  (with-fixture test-database ()
    (conskivi-put db :list '(1 2 3))
    (conskivi-put db :array #(1 2 3))
    (conskivi-put db :hash (make-hash-table :test #'equal))
    (is (equal (conskivi-get db :list) '(1 2 3)))
    (is (equalp (conskivi-get db :array) #(1 2 3)))
    (is (hash-table-p (conskivi-get db :hash)))))