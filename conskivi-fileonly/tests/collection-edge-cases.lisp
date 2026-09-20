;;; -*- lisp -*-
;;;
;;; Edge-case coverage for collection backends: large (multi-page)
;;; collections, key encoding, scan pagination, range boundaries,
;;; del/recreate cycles and TTL firing. Complements bug-report.lisp.

(in-package :conskivi-fileonly-tests)

(def-suite collection-edge-cases
  :description "Collection edge cases"
  :in all-tests)

(def-fixture edge-db ()
  (let ((edge-db-path #p"/tmp/test-conskivi-edge/"))
    (when (probe-file edge-db-path)
      (delete-directory edge-db-path))
    (ensure-directories-exist edge-db-path)
    (unwind-protect
         (&body)
      (when (probe-file edge-db-path)
        (delete-directory edge-db-path)))))

(defun make-edge-db (db-path)
  (let ((db (make-instance 'conskivi-fileonly-database
                           :name "edge"
                           :db-path db-path)))
    (conskivi-start db)
    db))

(def-test edge-large-hash-split-restart (:suite collection-edge-cases)
  "300 hash fields overflow a 4K leaf page (split + branch root) and
must round-trip through stop/start intact, including deletes."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (dotimes (i 300)
        (conskivi-hset db :big (format nil "field~a" i) i))
      (is (= 300 (length (conskivi-hkeys db :big))))
      (is (= 299 (conskivi-hget db :big "field299")))
      ;; Delete every even field.
      (dotimes (i 150)
        (conskivi-hdel db :big (format nil "field~a" (* 2 i))))
      (is (= 150 (length (conskivi-hkeys db :big))))
      (is (= 299 (conskivi-hget db :big "field299")))
      (is (null (conskivi-hget db :big "field298")))
      (conskivi-stop db))
    (let ((db2 (make-edge-db edge-db-path)))
      (is (= 150 (length (conskivi-hkeys db2 :big))))
      (is (= 299 (conskivi-hget db2 :big "field299")))
      (is (= 1 (conskivi-hget db2 :big "field1")))
      (is (null (conskivi-hget db2 :big "field0")))
      (conskivi-stop db2))))

(def-test edge-large-zset-scale (:suite collection-edge-cases)
  "200-member zset: ordering, scores and update-no-ghost at scale."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (dotimes (i 200)
        (conskivi-zadd db :z (float (1+ i)) (format nil "m~a" i)))
      (is (= 200 (conskivi-zcard db :z)))
      ;; Bump the first 50 members above the rest.
      (dotimes (i 50)
        (conskivi-zincrby db :z 1000.0 (format nil "m~a" i)))
      (is (= 200 (conskivi-zcard db :z)))
      (conskivi-stop db))
    (let ((db2 (make-edge-db edge-db-path)))
      (is (= 200 (conskivi-zcard db2 :z))
          "no ghosts after restart")
      (is (= 1001.0 (conskivi-zscore db2 :z "m0")))
      (is (= 200.0 (conskivi-zscore db2 :z "m199")))
      ;; Lowest two must be the untouched tail members.
      (is (equal '("m50" "m51") (conskivi-zrange db2 :z 0 1)))
      (conskivi-stop db2))))

(def-test edge-del-then-recreate-collection (:suite collection-edge-cases)
  "DEL followed by reuse of the key starts a clean collection."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-zadd db :z 1.0 "old")
      (conskivi-del db :z)
      (conskivi-zadd db :z 9.0 "new")
      (is (equal '("new" 9.0) (conskivi-zrange db :z 0 -1 t)))
      (is (null (conskivi-zscore db :z "old")))
      (conskivi-hset db :h "a" 1)
      (conskivi-del db :h)
      (conskivi-hset db :h "b" 2)
      (is (equal '("b" 2) (conskivi-hgetall db :h)))
      (conskivi-stop db))
    (let ((db2 (make-edge-db edge-db-path)))
      (is (equal '("new" 9.0) (conskivi-zrange db2 :z 0 -1 t)))
      (is (equal '("b" 2) (conskivi-hgetall db2 :h)))
      (conskivi-stop db2))))

(def-test edge-special-char-keys (:suite collection-edge-cases)
  "Keys with spaces, slashes and punctuation round-trip through
encode/decode, keys listing and deletion."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path))
          (names '("hello world" "a/b" "a:b?c+d")))
      (dolist (n names)
        (conskivi-put db (intern (string-upcase n) :keyword)
                      (concatenate 'string "v-" n)))
      (dolist (n names)
        (let ((k (intern (string-upcase n) :keyword)))
          (is (equal (concatenate 'string "v-" n) (conskivi-get db k)))
          (is (member k (conskivi-keys db)))))
      (conskivi-del db (intern (string-upcase "a/b") :keyword))
      (is (null (conskivi-get db (intern (string-upcase "a/b") :keyword))))
      (is (= 2 (length (conskivi-keys db))))
      (conskivi-stop db))))

(def-test edge-keys-include-collections (:suite collection-edge-cases)
  "keys lists scalar and collection keys alike; mget tolerates
collection keys (NIL, no crash)."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-put db :s "v")
      (conskivi-zadd db :z 1.0 "a")
      (conskivi-hset db :h "f" 1)
      (conskivi-sadd db :st "m")
      (let ((keys (conskivi-keys db)))
        (is (= 4 (length keys)))
        (is (member :s keys))
        (is (member :z keys))
        (is (member :h keys))
        (is (member :st keys)))
      (is (equal '("v" nil nil)
                 (conskivi-mget db (list :s :z :missing))))
      (conskivi-stop db))))

(def-test edge-scan-exhaustion (:suite collection-edge-cases)
  "Paginated scan with count visits every key exactly once and
terminates with a NIL cursor."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (dotimes (i 20)
        (conskivi-put db (intern (format nil "K~a" i) :keyword)
                      (format nil "v~a" i)))
      (let ((seen '())
            (cursor nil))
        (loop
          (let ((page (conskivi-scan db cursor nil 5)))
            (setf seen (append seen (first page)))
            (setf cursor (second page))
            (unless cursor (return))))
        (is (= 20 (length seen)))
        (is (= 20 (length (remove-duplicates seen))))
        (is (equal (sort (copy-list seen) #'string< :key #'symbol-name)
                       (sort (loop for i from 0 below 20
                                   collect (intern (format nil "K~a" i) :keyword))
                             #'string< :key #'symbol-name))))
      (conskivi-stop db))))

(def-test edge-scan-no-match (:suite collection-edge-cases)
  "A pattern matching nothing returns an empty first page."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-put db :a "1")
      (let ((page (conskivi-scan db nil "ZZZ-NO-MATCH.*")))
        (is (null (first page))))
      (conskivi-stop db))))

(def-test edge-zrange-boundaries (:suite collection-edge-cases)
  "Negative indices, clamping, withscores both directions and
inverted ranges."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-zadd db :z 1.0 "a" 2.0 "b" 3.0 "c" 4.0 "d" 5.0 "e")
      (is (equal '("c" "d" "e") (conskivi-zrange db :z -3 -1)))
      (is (equal '("c" 3.0 "d" 4.0 "e" 5.0)
                 (conskivi-zrange db :z -3 -1 t)))
      (is (equal '("e" 5.0 "d" 4.0)
                 (conskivi-zrevrange db :z 0 1 t)))
      (is (equal '("d" "e") (conskivi-zrange db :z 3 99)))
      (is (null (conskivi-zrange db :z 3 1)))
      (is (null (conskivi-zrevrange db :z 3 1)))
      (conskivi-stop db))))

(def-test edge-incrby-creates (:suite collection-edge-cases)
  "hincrby/zincrby on missing fields/members start from zero."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (is (= 5 (conskivi-hincrby db :h "new" 5)))
      (is (= 5 (conskivi-hget db :h "new")))
      (is (= 2.5 (conskivi-zincrby db :z 2.5 "new")))
      (is (= 2.5 (conskivi-zscore db :z "new")))
      (conskivi-stop db))))

(def-test edge-string-ttl-fires (:suite collection-edge-cases)
  "An expired string key reads as missing."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-put db :tmp "v")
      (conskivi-expire db :tmp 1)
      ;; universal-time has 1s resolution: sleep 2 to cross the boundary.
      (sleep 2)
      (is (null (conskivi-get db :tmp)))
      (is (null (conskivi-exists db :tmp)))
      (conskivi-stop db))))

(def-test edge-list-drain-and-recreate (:suite collection-edge-cases)
  "Popping the last element removes the file; the key is reusable."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-rpush db :l "a" "b")
      (is (equal "a" (conskivi-lpop db :l)))
      (is (equal "b" (conskivi-lpop db :l)))
      (is (null (conskivi-lpop db :l)))
      (is (null (conskivi-exists db :l)))
      (conskivi-rpush db :l "c")
      (is (equal '("c") (conskivi-get db :l)))
      (is (= 1 (conskivi-llen db :l)))
      (conskivi-stop db))))

(def-test edge-remove-missing-members (:suite collection-edge-cases)
  "Removing absent members/fields reports zero/NIL."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-zadd db :z 1.0 "a")
      (is (= 0 (conskivi-zrem db :z "ghost")))
      (is (= 0 (conskivi-zrem db :missing "a")))
      (conskivi-hset db :h "f" 1)
      (is (null (conskivi-hdel db :h "ghost")))
      (conskivi-sadd db :s "m")
      (is (= 0 (conskivi-srem db :s "ghost")))
      (conskivi-stop db))))

(def-test edge-hscan-field-value-order (:suite collection-edge-cases)
  "HSCAN pages (field value) pairs like HGETALL.
Regression: pushes were val-then-field, yielding (value field)."
  (with-fixture edge-db ()
    (let ((db (make-edge-db edge-db-path)))
      (conskivi-hset db :h "f1" "v1")
      (conskivi-hset db :h "f2" "v2")
      (conskivi-hset db :h "f3" "v3")
      (let ((page (conskivi-hscan db :h nil nil 2)))
        (is (equal '("f1" "v1" "f2" "v2") (first page)))
        (is (second page)))
      (let ((seen '())
            (cursor nil))
        (loop
          (let ((page (conskivi-hscan db :h cursor nil 2)))
            (setf seen (append seen (first page)))
            (setf cursor (second page))
            (unless cursor (return))))
        (is (equal '("f1" "v1" "f2" "v2" "f3" "v3") seen)))
      (conskivi-stop db))))
