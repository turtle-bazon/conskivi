;;; -*- lisp -*-
;;;
;;; Edge-case coverage for the in-memory backend: duplicate accounting,
;;; missing-key returns, negative zset ranges, creation via incrby,
;;; hscan pagination, TTL firing and save/load persistence.

(in-package :conskivi-inmemory-tests)

(def-suite edge-cases
  :description "In-memory edge cases"
  :in all-tests)

(def-fixture edge-database ()
  (let ((db (make-instance 'conskivi-inmemory-database
                           :name "edge-db"
                           :db-path #p"/tmp/test-conskivi-edge.db")))
    (unwind-protect
         (&body)
      (when (probe-file #p"/tmp/test-conskivi-edge.db")
        (delete-file #p"/tmp/test-conskivi-edge.db")))))

(def-fixture edge-started-database ()
  (let ((db (make-instance 'conskivi-inmemory-database
                           :name "edge-db"
                           :db-path #p"/tmp/test-conskivi-edge.db")))
    (conskivi-start db)
    (unwind-protect
         (&body)
      (progn
        (conskivi-stop db)
        (when (probe-file #p"/tmp/test-conskivi-edge.db")
          (delete-file #p"/tmp/test-conskivi-edge.db"))))))

(def-test edge-sadd-duplicate-members (:suite edge-cases)
  "SADD counts distinct members added, even within a single call.
Regression: the creation branch returned (length members)."
  (with-fixture edge-database ()
    (is (= 2 (conskivi-sadd db :k "a" "a" "b")))
    (is (= 2 (conskivi-scard db :k)))
    (is (= 1 (conskivi-sadd db :k "b" "c")))
    (is (= 3 (conskivi-scard db :k)))))

(def-test edge-remove-missing-returns-zero (:suite edge-cases)
  "SREM/ZREM on missing keys or members report 0 (Redis parity)."
  (with-fixture edge-database ()
    (is (= 0 (conskivi-srem db :missing "m")))
    (is (= 0 (conskivi-zrem db :missing "m")))
    (conskivi-sadd db :s "a")
    (is (= 0 (conskivi-srem db :s "ghost")))
    (conskivi-zadd db :z 1.0 "a")
    (is (= 0 (conskivi-zrem db :z "ghost")))))

(def-test edge-zrange-negative-indices (:suite edge-cases)
  "Negative ZRANGE/ZREVRANGE bounds count from the end."
  (with-fixture edge-database ()
    (conskivi-zadd db :z 1.0 "a" 2.0 "b" 3.0 "c" 4.0 "d" 5.0 "e")
    (is (equal '("c" "d" "e") (conskivi-zrange db :z -3 -1)))
    (is (equal '("c" 3.0 "d" 4.0 "e" 5.0)
               (conskivi-zrange db :z -3 -1 t)))
    (is (equal '("e" 5.0 "d" 4.0)
               (conskivi-zrevrange db :z 0 1 t)))
    (is (equal '("d" "e") (conskivi-zrange db :z 3 99)))
    (is (null (conskivi-zrange db :z 3 1)))))

(def-test edge-zrange-drained-set (:suite edge-cases)
  "Range reads on a fully-drained sorted set return NIL."
  (with-fixture edge-database ()
    (conskivi-zadd db :e 1.0 "x")
    (conskivi-zrem db :e "x")
    (is (= 0 (conskivi-zcard db :e)))
    (is (null (conskivi-zrange db :e 0 -1)))
    (is (null (conskivi-zrange db :e 0 -1 t)))
    (is (null (conskivi-zrevrange db :e 0 -1 t)))))

(def-test edge-incrby-creates-missing (:suite edge-cases)
  "ZINCRBY/HINCRBY on missing keys create them from zero."
  (with-fixture edge-database ()
    (is (= 2.5 (conskivi-zincrby db :z 2.5 "m")))
    (is (= 2.5 (conskivi-zscore db :z "m")))
    (is (= 5 (conskivi-hincrby db :h "f" 5)))
    (is (= 5 (conskivi-hget db :h "f")))))

(def-test edge-sinter-missing-key (:suite edge-cases)
  "A missing key is an empty set: SINTER with it is NIL."
  (with-fixture edge-database ()
    (conskivi-sadd db :a "x" "y")
    (conskivi-sadd db :b "y" "z")
    (is (null (conskivi-sinter db (list :a :missing))))
    (is (null (conskivi-sinter db (list :missing :a))))
    (let ((inter (conskivi-sinter db (list :a :b))))
      (is (= 1 (length inter)))
      (is (member "y" inter :test #'equal)))))

(def-test edge-hscan-paginates (:suite edge-cases)
  "HSCAN pages field/value pairs with an integer cursor to exhaustion.
Regression: the cursor was the field itself and pairs destructured
wrong, so the second page errored."
  (with-fixture edge-database ()
    (conskivi-hset db :h "f1" "v1")
    (conskivi-hset db :h "f2" "v2")
    (conskivi-hset db :h "f3" "v3")
    (let ((seen '())
          (cursor nil))
      (loop
        (let ((page (conskivi-hscan db :h cursor nil 2)))
          (setf seen (append seen (first page)))
          (setf cursor (second page))
          (unless cursor (return))))
      (is (= 6 (length seen)))
      (is (member "f1" seen :test #'equal))
      (is (member "v3" seen :test #'equal))
      ;; Pairs stay (field value) ordered.
      (is (equal "v1" (second (member "f1" seen :test #'equal)))))))

(def-test edge-string-ttl-fires (:suite edge-cases)
  "An expired string key reads as missing."
  (with-fixture edge-started-database ()
    (conskivi-put db :tmp "v")
    (conskivi-expire db :tmp 1)
    ;; universal-time has 1s resolution: sleep 2 to cross the boundary.
    (sleep 2)
    (is (null (conskivi-get db :tmp)))
    (is (null (conskivi-exists db :tmp)))))

(def-test edge-save-load-scalars (:suite edge-cases)
  "Save/load round-trips scalar values."
  (with-fixture edge-database ()
    (conskivi-put db :str "hello")
    (conskivi-put db :int 42)
    (conskivi-put db :list '(1 2 3))
    (conskivi-save db)
    (let ((db2 (make-instance 'conskivi-inmemory-database
                              :name "edge-db2"
                              :db-path #p"/tmp/test-conskivi-edge.db")))
      (conskivi-load db2)
      (is (equal "hello" (conskivi-get db2 :str)))
      (is (= 42 (conskivi-get db2 :int)))
      (is (equal '(1 2 3) (conskivi-get db2 :list))))))

(def-test edge-save-load-compound (:suite edge-cases)
  "Save/load round-trips sets, hashes, arrays and sorted sets.
Regression: PRINTed hash-tables (#<...>) made the save file
unreadable, so any compound value broke LOAD."
  (with-fixture edge-database ()
    (conskivi-sadd db :set "a" "b")
    (conskivi-hset db :hash "f" 1)
    (conskivi-zadd db :z 1.0 "m")
    (conskivi-put db :arr #(1 2))
    (conskivi-save db)
    (let ((db2 (make-instance 'conskivi-inmemory-database
                              :name "edge-db2"
                              :db-path #p"/tmp/test-conskivi-edge.db")))
      (conskivi-load db2)
      (is (eq :set (conskivi-type db2 :set)))
      (is (= 2 (conskivi-scard db2 :set)))
      (is (conskivi-sismember db2 :set "a"))
      (is (= 1 (conskivi-hget db2 :hash "f")))
      (is (= 1.0 (conskivi-zscore db2 :z "m")))
      (is (equalp #(1 2) (conskivi-get db2 :arr))))))

(def-test edge-save-load-nested (:suite edge-cases)
  "Nested structures (list of hash, odd field names) survive save/load."
  (with-fixture edge-database ()
    (conskivi-put db :nested '((:list 1) "x"))
    (conskivi-hset db :h ":sorted-set" 7)
    (conskivi-save db)
    (let ((db2 (make-instance 'conskivi-inmemory-database
                              :name "edge-db2"
                              :db-path #p"/tmp/test-conskivi-edge.db")))
      (conskivi-load db2)
      (is (equal '((:list 1) "x") (conskivi-get db2 :nested)))
      (is (= 7 (conskivi-hget db2 :h ":sorted-set"))))))
