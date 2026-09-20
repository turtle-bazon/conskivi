;;; -*- lisp -*-
;;;
;;; Regression tests for /tmp/conskivi-bugreport.md (conskivi-fileonly-20260826).
;;;
;;; Bug 1 (fatal) : warm-start persistence broken - loader checks byte 0
;;;                 (magic low byte 0x50) for the collection type, but the
;;;                 type lives at +meta-offset-type+ (offset 12).
;;;                 => after a process restart every collection comes back
;;;                 empty although the .ck file contains the data.
;;; Bug 2         : index-hgetall returns (value field) pairs instead of
;;;                 (field value).
;;; Bug 3         : "phantom member after reopen". The original non-update
;;;                 scenario did not reproduce; but updating an existing
;;;                 member/field left the superseded leaf entry in the B+tree,
;;;                 so scans (hgetall) and warm-load (build-skiplist-from-btree)
;;;                 saw duplicates. Covered by bug-3-update-* below.

(in-package :conskivi-fileonly-tests)

(def-suite bug-report
  :description "Bugs from conskivi-bugreport.md"
  :in all-tests)

(def-fixture bug-report-db ()
  (let ((bug-db-path #p"/tmp/test-conskivi-bugreport/"))
    (when (probe-file bug-db-path)
      (delete-directory bug-db-path))
    (ensure-directories-exist bug-db-path)
    (unwind-protect
         (&body)
      (when (probe-file bug-db-path)
        (delete-directory bug-db-path)))))

(defun make-fresh-db (db-path)
  "Create a freshly-started database instance in DB-PATH."
  (let ((db (make-instance 'conskivi-fileonly-database
                           :name "bugreport"
                           :db-path db-path)))
    (conskivi-start db)
    db))

(def-test bug-1-warm-start-hash-survives-restart (:suite bug-report)
  "hset then stop/start (fresh instance) must not lose the hash."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :k "a" 1)
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      ;; Expected: warm-start reloads K.ck and hget returns 1.
      (is (equal 1 (conskivi-hget db2 :k "a")))
      (is (equal '("a" 1) (conskivi-hgetall db2 :k)))
      (conskivi-stop db2))))

(def-test bug-1-loader-accepts-valid-file (:suite bug-report)
  "load-btree-collection must accept a file written by the writer."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :k "a" 1)
      (conskivi-stop db))
    (let* ((ck (first (directory (merge-pathnames #p"*.ck" bug-db-path))))
           (index (make-hash-table :test #'equal)))
      (is (not (null ck)))
      (is (not (null (conskivi-fileonly::load-btree-collection
                      ck :k index)))
          "loader rejected a valid .ck file (byte-0 type check)")
      (is (gethash :k index)))))

(def-test bug-2-hgetall-field-value-order (:suite bug-report)
  "hgetall must return (field1 value1 field2 value2 ...)."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :k "turtle" 3)
      (conskivi-hset db :k "horse" 1)
      ;; Tree order is by field byte: "horse" < "turtle".
      (is (equal '("horse" 1 "turtle" 3) (conskivi-hgetall db :k)))
      (conskivi-stop db))))

(defun zset-skiplist-pairs (db key)
  "Return rebuilt score-skiplist as sorted list of (score . member)."
  (let* ((index (conskivi-fileonly::get-collection-index db))
         (entry (gethash key index)))
    (unless entry
      (return-from zset-skiplist-pairs nil))
    (let ((pairs '()))
      (conskivi-fileonly::skiplist-map (conskivi-fileonly::cie-score-tree entry)
        (lambda (k v)
          (declare (ignore v))
          (push (cons (car k)
                      (flexi-streams:octets-to-string (cdr k)
                                                      :external-format :utf-8))
                pairs)))
      (sort pairs
            (lambda (a b)
              (if (= (car a) (car b))
                  (string< (cdr a) (cdr b))
                  (< (car a) (car b))))))))

(def-test bug-3-no-phantom-after-reopen (:suite bug-report)
  "After a reopen the rebuilt skiplist must match the stored members.
Confirms the 'phantom member' report does NOT reproduce on the clean
fresh-instance restart path."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :z 1.0 "turtle")
      (conskivi-zadd db :z 2.0 "horse")
      (is (= 2 (conskivi-zcard db :z)))
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      ;; Bug 1 kills the load, so nothing is in the index until a mutation
      ;; opens the existing file (ensure-collection-index/btree-open-existing).
      (conskivi-zadd db2 :z 3.0 "sheep")
      (let ((pairs (zset-skiplist-pairs db2 :z)))
        (is (= 3 (length pairs)) "skiplist must contain exactly 3 members")
        (is (equal '(("turtle" . 1.0d0) ("horse" . 2.0d0) ("sheep" . 3.0d0))
                   (mapcar (lambda (p) (cons (cdr p)
                                             (if (numberp (car p))
                                                 (coerce (car p) 'double-float)
                                                 (car p))))
                           pairs))
            "no phantom members; scores decode as float64")
        (is (= 3 (conskivi-zcard db2 :z)))
        (is (= 3 (conskivi-fileonly::skiplist-count
                  (conskivi-fileonly::cie-score-tree
                   (gethash :z (conskivi-fileonly::get-collection-index db2)))))
            "skiplist count must equal entry count"))
      (conskivi-stop db2))))

(def-test bug-3-equal-scores-rebuild (:suite bug-report)
  "Reopen with >=2 members sharing equal scores must not duplicate."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :z 1.0 "turtle")
      (conskivi-zadd db :z 1.0 "horse")
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (conskivi-zadd db2 :z 1.0 "sheep")
      (let ((pairs (zset-skiplist-pairs db2 :z)))
        (is (= 3 (length pairs)))
        (is (equal '("horse" "sheep" "turtle")
                   (mapcar #'cdr pairs))))
      (conskivi-stop db2))))

(def-test bug-3-zadd-update-no-duplicate-leaf (:suite bug-report)
  "Updating a member's score must not leave the old leaf entry behind.
Regression: index-zadd inserted a new entry without removing the old one,
so hgetall/zrange and warm-load saw a ghost member at the old score."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :scores 1.0 "turtle")
      (conskivi-zadd db :scores 1.5 "turtle")
      (conskivi-zadd db :scores 1.0 "horse")
      (is (= 2 (conskivi-zcard db :scores)))
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (is (= 2 (conskivi-zcard db2 :scores))
          "warm-load must not resurrect the superseded score")
      (let ((pairs (zset-skiplist-pairs db2 :scores)))
        (is (equal '("horse" "turtle") (mapcar #'cdr pairs)))
        (is (equal '(1.0d0 1.5d0)
                   (mapcar (lambda (p) (coerce (car p) 'double-float)) pairs))
            "turtle must keep only its updated score 1.5"))
      (conskivi-stop db2))))

(def-test bug-3-hset-update-no-duplicate-leaf (:suite bug-report)
  "Updating a hash field must not leave the old leaf entry behind."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :day "turtle" 1)
      (conskivi-hset db :day "turtle" 2)
      (conskivi-hset db :day "horse" 1)
      (is (equal '("horse" 1 "turtle" 2) (conskivi-hgetall db :day))
          "hgetall must not show the stale turtle field")
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (is (equal '("horse" 1 "turtle" 2) (conskivi-hgetall db2 :day)))
      (is (equal 2 (conskivi-hget db2 :day "turtle")))
      (conskivi-stop db2))))