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

(def-test bug-4-del-invalidates-collection-index (:suite bug-report)
  "conskivi-del must drop the in-memory collection entry, not just the .ck file.
Regression: exists/keys (disk-backed) reported the key gone while
zrange/hgetall/smembers (index-backed) kept serving the deleted members."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :z 1.0 "turtle")
      (conskivi-zadd db :z 2.0 "horse")
      (conskivi-del db :z)
      (is (null (conskivi-exists db :z)))
      (is (null (conskivi-zrange db :z 0 -1 t))
          "zrange must not serve a deleted key")
      (is (= 0 (conskivi-zcard db :z)))
      (is (null (conskivi-keys db)))
      (conskivi-hset db :h "f" 1)
      (conskivi-del db :h)
      (is (null (conskivi-hgetall db :h))
          "hgetall must not serve a deleted key")
      (conskivi-sadd db :s "m")
      (conskivi-del db :s)
      (is (null (conskivi-smembers db :s))
          "smembers must not serve a deleted key")
      (conskivi-stop db))))

(def-test bug-delete-variable-length-leaf-entries (:suite bug-report)
  "Deleting/updating a non-last leaf entry with unequal entry lengths
must not corrupt the remaining entries.
Regression: btree-delete-from-leaf shifted entries in place with
recomputed offsets, which corrupted variable-length pages (hget
returned NIL, hgetall lost entries, follow-up deletes signalled
BOUNDING-INDICES-BAD-ERROR)."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      ;; Unequal lengths: "a"(1) < "bbbbb"(5) < "cc"(2), mixed value sizes.
      (conskivi-hset db :k "a" 1)
      (conskivi-hset db :k "bbbbb" "a-much-longer-value-string")
      (conskivi-hset db :k "cc" 3)
      ;; Update the FIRST entry to a much longer value (delete+insert path).
      (conskivi-hset db :k "a" "now-a-is-also-a-long-value-12345")
      (is (equal "now-a-is-also-a-long-value-12345"
                 (conskivi-hget db :k "a")))
      (is (equal "a-much-longer-value-string"
                 (conskivi-hget db :k "bbbbb"))
          "neighbour entry must survive an update of the first entry")
      (is (equal 3 (conskivi-hget db :k "cc"))
          "last entry must survive an update of the first entry")
      ;; Delete the FIRST entry; survivors must stay intact.
      (conskivi-hdel db :k "a")
      (is (equal '("bbbbb" "a-much-longer-value-string" "cc" 3)
                 (conskivi-hgetall db :k)))
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (is (equal '("bbbbb" "a-much-longer-value-string" "cc" 3)
                 (conskivi-hgetall db2 :k))
          "survivors must persist across restart")
      (conskivi-stop db2))))

(def-test bug-delete-middle-leaf-entry (:suite bug-report)
  "Deleting a middle leaf entry must keep both neighbours intact."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :k "a" 1)
      (conskivi-hset db :k "mmmmmmmmmm" 2)
      (conskivi-hset db :k "z" 3)
      (conskivi-hdel db :k "mmmmmmmmmm")
      (is (equal '("a" 1 "z" 3) (conskivi-hgetall db :k)))
      (is (equal 1 (conskivi-hget db :k "a")))
      (is (equal 3 (conskivi-hget db :k "z")))
      (conskivi-stop db))))

(def-test bug-expiry-thread-preserves-collections (:suite bug-report)
  "The background expiration sweeper must not delete collection files.
Regression: check-expiration parsed the B+tree meta as a simple key
header, read a garbage expiration far in the past, and deleted every
flushed .ck file ~100ms after start. The second restart then lost
all collection data permanently."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :h "f" 1)
      (conskivi-zadd db :z 1.0 "a")
      (conskivi-sadd db :s "m")
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      ;; Let the expiration thread run several cycles.
      (sleep 0.6)
      (is (probe-file (merge-pathnames #p"H.ck" bug-db-path))
          "hash file must survive the expiry sweeper")
      (is (probe-file (merge-pathnames #p"Z.ck" bug-db-path))
          "zset file must survive the expiry sweeper")
      (is (probe-file (merge-pathnames #p"S.ck" bug-db-path))
          "set file must survive the expiry sweeper")
      (is (equal 1 (conskivi-hget db2 :h "f")))
      (is (equal '("a" 1.0) (conskivi-zrange db2 :z 0 -1 t)))
      (conskivi-stop db2))
    (let ((db3 (make-fresh-db bug-db-path)))
      (is (equal 1 (conskivi-hget db3 :h "f"))
          "data must survive a second restart")
      (is (equal '("a" 1.0) (conskivi-zrange db3 :z 0 -1 t)))
      (is (equal '("m") (conskivi-smembers db3 :s)))
      (conskivi-stop db3))))

(def-test bug-type-on-collection-keys (:suite bug-report)
  "conskivi-type must report collection types, not crash.
Regression: read-key-file ran the simple decoder over the B+tree
meta page; the magic byte 0x50 fell through the ECASE."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :h "f" 1)
      (conskivi-zadd db :z 1.0 "a")
      (conskivi-sadd db :s "m")
      (conskivi-put db :str "v")
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (is (eq :hash (conskivi-type db2 :h)))
      (is (eq :sorted-set (conskivi-type db2 :z)))
      (is (eq :set (conskivi-type db2 :s)))
      (is (eq :string (conskivi-type db2 :str)))
      (is (null (conskivi-type db2 :missing)))
      (conskivi-stop db2))))

(def-test bug-get-on-collection-key-preserves-file (:suite bug-report)
  "conskivi-get on a collection key returns NIL and must leave the
.ck file alone. Regression: check-expiration misread the meta page
as expired and DELETED the collection file."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :z 1.0 "a")
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (is (null (conskivi-get db2 :z)))
      (is (probe-file (merge-pathnames #p"Z.ck" bug-db-path))
          "get must not delete the collection file")
      (is (equal '("a" 1.0) (conskivi-zrange db2 :z 0 -1 t)))
      (conskivi-stop db2))))

(def-test bug-expire-ttl-persist-on-collections (:suite bug-report)
  "expire/ttl/persist must work on collection keys.
Regression: expire crashed on flushed files (ECASE over the meta
magic) and silently did nothing pre-flush; ttl reported garbage."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :h "f" 1)
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (conskivi-expire db2 :h 100)
      (let ((ttl (conskivi-ttl db2 :h)))
        (is (and (> ttl 0) (<= ttl 100))))
      (is (equal 1 (conskivi-hget db2 :h "f"))
          "unexpired key still readable")
      (conskivi-persist db2 :h)
      (is (= -1 (conskivi-ttl db2 :h)))
      (is (equal 1 (conskivi-hget db2 :h "f")))
      (conskivi-stop db2))))

(def-test bug-collection-expiry-fires (:suite bug-report)
  "An expired collection reads as missing (data + index evicted)."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-hset db :h "f" 1)
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path)))
      (conskivi-expire db2 :h 1)
      ;; universal-time has 1s resolution: sleep 2 to cross the boundary.
      (sleep 2)
      (is (null (conskivi-hgetall db2 :h)))
      (is (null (conskivi-exists db2 :h)))
      (is (not (member :h (conskivi-keys db2))))
      (conskivi-stop db2))))

(def-test bug-zrange-withscores-on-drained-set (:suite bug-report)
  "Range reads on a fully-drained sorted set return NIL, not a crash.
Regression: (nth 0 nil) produced a NIL entry whose score failed the
REAL type check."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-zadd db :e 1.0 "x")
      (conskivi-zrem db :e "x")
      (is (= 0 (conskivi-zcard db :e)))
      (is (null (conskivi-zrange db :e 0 -1)))
      (is (null (conskivi-zrange db :e 0 -1 t)))
      (is (null (conskivi-zrevrange db :e 0 -1)))
      (is (null (conskivi-zrevrange db :e 0 -1 t)))
      (conskivi-stop db))))

(def-test bug-sinter-missing-key (:suite bug-report)
  "A missing key is an empty set: sinter with it is NIL.
Regression: missing non-first keys were skipped, returning the
present set's members."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (conskivi-sadd db :a "x" "y")
      (conskivi-sadd db :b "y" "z")
      (is (null (conskivi-sinter db (list :a :missing))))
      (is (null (conskivi-sinter db (list :missing :a))))
      (let ((inter (conskivi-sinter db (list :a :b))))
        (is (= 1 (length inter)))
        (is (member "y" inter :test #'equal)))
      ;; union/difference skip missing keys (union with empty).
      (is (= 2 (length (conskivi-sunion db (list :a :missing)))))
      (is (= 2 (length (conskivi-sdiff db (list :a :missing)))))
      (conskivi-stop db))))

(def-test bug-srem-missing-key-creates-nothing (:suite bug-report)
  "SREM on a missing key returns 0 and creates no .ck file.
Regression: the method used ensure-collection-index, materialising
an empty collection file (plus mmap/WAL) on a read-like op."
  (with-fixture bug-report-db ()
    (let ((db (make-fresh-db bug-db-path)))
      (is (= 0 (conskivi-srem db :ghost "m")))
      (is (null (directory (merge-pathnames #p"*.ck" bug-db-path)))
          "no collection file may appear")
      (is (= 0 (conskivi-zrem db :ghost "m")))
      (is (null (conskivi-hdel db :ghost "f")))
      (conskivi-stop db))))

(def-test bug-mixed-case-keys-survive-restart (:suite bug-report)
  "Programmatically interned mixed-case keys (e.g. POST-<id>) must
resolve after a restart.
Regression: warm-load and list-all-keys re-interned keys UPPERCASED,
so the on-disk data (case-preserved) became unreachable through the
in-memory index: exists/type (file probe) worked while hgetall/keys
served NIL/upcased results."
  (with-fixture bug-report-db ()
    (let ((post (intern "POST-ft6bmba54irq7phne56udeqyyo" :keyword))
          (day (intern "DAY-2026-08-05" :keyword))
          (scores (intern "SCORES-abc123" :keyword))
          (db (make-fresh-db bug-db-path)))
      (conskivi-hset db post "field" 1)
      (conskivi-zadd db scores 2.0 "m")
      (conskivi-sadd db day "m")
      (conskivi-put db (intern "STR-AbC" :keyword) "v")
      (is (equal '("field" 1) (conskivi-hgetall db post)))
      (conskivi-stop db))
    (let ((db2 (make-fresh-db bug-db-path))
          (post (intern "POST-ft6bmba54irq7phne56udeqyyo" :keyword))
          (day (intern "DAY-2026-08-05" :keyword))
          (scores (intern "SCORES-abc123" :keyword))
          (str (intern "STR-AbC" :keyword)))
      (is (equal '("field" 1) (conskivi-hgetall db2 post))
          "hash readable through the original mixed-case key")
      (is (equal 1 (conskivi-hget db2 post "field")))
      (is (equal '("m" 2.0) (conskivi-zrange db2 scores 0 -1 t)))
      (is (equal '("m") (conskivi-smembers db2 day)))
      (is (equal "v" (conskivi-get db2 str)))
      (is (member post (conskivi-keys db2))
          "keys preserves exact case")
      (is (member str (conskivi-keys db2)))
      ;; Writes through the mixed-case key still land in the same entry.
      (conskivi-hset db2 post "field2" 2)
      (is (equal 2 (conskivi-hget db2 post "field2")))
      ;; Deletion resolves too.
      (conskivi-del db2 day)
      (is (null (conskivi-exists db2 day)))
      (is (null (conskivi-smembers db2 day)))
      (conskivi-stop db2))
    (let ((db3 (make-fresh-db bug-db-path))
          (post (intern "POST-ft6bmba54irq7phne56udeqyyo" :keyword)))
      (is (equal '("field" 1 "field2" 2) (conskivi-hgetall db3 post))
          "mixed-case data survives a second restart")
      (conskivi-stop db3))))