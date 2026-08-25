;;; -*- lisp -*-

(in-package :conskivi-fileonly-tests)

(def-suite crash-recovery
  :description "Crash recovery with WAL"
  :in all-tests)

(def-fixture crash-test-database ()
  (let ((crash-db-path #p"/tmp/test-conskivi-crash.db/"))
    (when (probe-file crash-db-path)
      (delete-directory crash-db-path))
    (ensure-directories-exist crash-db-path)
    (unwind-protect
         (&body)
      (when (probe-file crash-db-path)
        (delete-directory crash-db-path)))))

(def-test crash-recovery-set-get (:suite crash-recovery)
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      ;; Write data
      (conskivi-put db :key1 "value1")
      (conskivi-put db :key2 "value2")
      ;; Flush to disk (WAL entries written then applied)
      (conskivi-save db)
      ;; Simulate crash: stop without flushing
      (simulate-crash db)
      ;; Reopen — WAL replay should recover
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (equal (conskivi-get db2 :key1) "value1"))
        (is (equal (conskivi-get db2 :key2) "value2"))))))

(def-test crash-recovery-types (:suite crash-recovery)
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (conskivi-put db :str "hello")
      (conskivi-put db :int 42)
      (conskivi-put db :float 3.14d0)
      (conskivi-put db :bool t)
      (conskivi-put db :kw :test)
      (conskivi-put db :list '(1 2 3))
      (conskivi-save db)
      (simulate-crash db)
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (equal (conskivi-get db2 :str) "hello"))
        (is (= (conskivi-get db2 :int) 42))
        (is (= (conskivi-get db2 :float) 3.14d0))
        (is (eq (conskivi-get db2 :bool) t))
        (is (eq (conskivi-get db2 :kw) :test))
        (is (equal (conskivi-get db2 :list) '(1 2 3)))))))

(def-test crash-recovery-many-keys (:suite crash-recovery)
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (dotimes (i 50)
        (conskivi-put db (intern (format nil "KEY~d" i) :keyword)
                        (format nil "val~d" i)))
      (conskivi-save db)
      (simulate-crash db)
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (dotimes (i 50)
          (let ((key (intern (format nil "KEY~d" i) :keyword)))
            (is (equal (conskivi-get db2 key) (format nil "val~d" i)))))))))

(def-test crash-recovery-overwrite (:suite crash-recovery)
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (conskivi-put db :key "original")
      (conskivi-save db)
      (conskivi-put db :key "updated")
      (conskivi-save db)
      (simulate-crash db)
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (equal (conskivi-get db2 :key) "updated"))))))

(def-test crash-recovery-delete (:suite crash-recovery)
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (conskivi-put db :key "value")
      (conskivi-save db)
      (conskivi-del db :key)
      (conskivi-save db)
      (simulate-crash db)
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (null (conskivi-get db2 :key)))))))

(def-test crash-recovery-unflushed-persisted-by-wal (:suite crash-recovery)
  "Writes logged to WAL survive crash even without save."
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (conskivi-put db :saved "yes")
      (conskivi-save db)
      ;; This write goes to WAL immediately (crash-safe)
      (conskivi-put db :also-saved "yes-too")
      (simulate-crash db)
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (equal (conskivi-get db2 :saved) "yes"))
        ;; WAL replay recovers this even without save
        (is (equal (conskivi-get db2 :also-saved) "yes-too"))))))

(def-test crash-recovery-wal-corrupt-survives (:suite crash-recovery)
  "If WAL is corrupted, previously saved data is still intact."
  (with-fixture crash-test-database ()
    (let ((db (make-instance 'conskivi-fileonly-database
                             :name "crash-test"
                             :db-path crash-db-path)))
      (conskivi-put db :good "data")
      (conskivi-save db)
      ;; Write more data (logged to WAL)
      (conskivi-put db :pending "maybe")
      ;; Corrupt the WAL file
      (let ((wal-path (merge-pathnames #p"*.wal" crash-db-path)))
        (dolist (f (directory wal-path))
          (with-open-file (s f :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
            (write-byte #xFF s))))
      (simulate-crash db)
      ;; Reopen — corrupt WAL should be skipped, good data survives
      (let ((db2 (make-instance 'conskivi-fileonly-database
                                :name "crash-test"
                                :db-path crash-db-path)))
        (is (equal (conskivi-get db2 :good) "data"))))))
