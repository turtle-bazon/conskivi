;;;; -*- mode: lisp -*-
;;;; Benchmark: conskivi-fileonly vs Redis 7.0.15

(in-package #:cl-user)

(defpackage #:conskivi-fileonly-benchmark
  (:use #:cl
        #:conskivi-core
        #:conskivi-fileonly)
  (:export #:run-benchmark))

(in-package #:conskivi-fileonly-benchmark)

;;; Timing utility

(defmacro time-it ((&rest label) &body body)
  "Execute BODY, return result and elapsed seconds."
  (declare (ignore label))
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (values (progn ,@body)
               (/ (- (get-internal-real-time) ,start)
                  (float internal-time-units-per-second))))))

(defun print-header ()
  (format t "~%=== conskivi-fileonly vs Redis 7.0.15 Benchmark ===~%")
  (format t "File-only storage: one file per key on disk~%~%"))

(defun ops-per-sec (iterations time-secs)
  "Compute ops/sec, returning 0 if time is zero."
  (if (and time-secs (plusp time-secs))
      (round iterations time-secs)
      0))

(defun print-row (operation conskivi-ops redis-ops)
  (let ((ratio (if (plusp redis-ops)
                   (/ (float conskivi-ops) (float redis-ops))
                   0.0)))
    (format t "  ~20a | ~12,d | ~12,d | ~5,1fx~%"
            operation (or conskivi-ops 0) (or redis-ops 0) ratio)))

(defun print-thread-row (threads conskivi-ops redis-ops)
  (let ((ratio (if (plusp redis-ops)
                   (/ (float conskivi-ops) (float redis-ops))
                   0.0)))
    (format t "  ~6d | ~12,d | ~12,d | ~5,1fx~%"
            threads conskivi-ops redis-ops ratio)))

;;; conskivi-fileonly benchmarks

(defun bench-conskivi-set-get (database iterations)
  (let ((keys (loop for i from 0 below iterations
                    collect (format nil "bench:~d" i))))
    (multiple-value-bind (r set-time) (time-it ()
                                        (dolist (key keys)
                                          (conskivi-put database key "value")))
      (declare (ignore r))
      (multiple-value-bind (r get-time) (time-it ()
                                          (dolist (key keys)
                                            (conskivi-get database key)))
        (declare (ignore r))
        (dolist (key keys)
          (conskivi-del database key))
        (values nil (+ set-time get-time))))))

(defun bench-conskivi-lpush-lindex (database iterations)
  (let ((key "bench:list"))
    (multiple-value-bind (r push-time) (time-it ()
                                         (loop for i from 0 below iterations
                                               do (conskivi-lpush database key i)))
      (declare (ignore r))
      (multiple-value-bind (r index-time) (time-it ()
                                            (loop for i from 0 below (min iterations 1000)
                                                  do (conskivi-lindex database key i)))
        (declare (ignore r))
        (conskivi-del database key)
        (values nil (+ push-time index-time))))))

(defun bench-conskivi-sadd-scard (database iterations)
  (let ((key "bench:set"))
    (multiple-value-bind (r add-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (conskivi-sadd database key (format nil "member:~d" i))))
      (declare (ignore r))
      (multiple-value-bind (r card-time) (time-it ()
                                           (loop repeat 1000
                                                 do (conskivi-scard database key)))
        (declare (ignore r))
        (conskivi-del database key)
        (values nil (+ add-time card-time))))))

(defun bench-conskivi-hset-hget (database iterations)
  (let ((key "bench:hash"))
    (multiple-value-bind (r set-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (conskivi-hset database key
                                                                (format nil "field:~d" i) i)))
      (declare (ignore r))
      (multiple-value-bind (r get-time) (time-it ()
                                          (loop for i from 0 below (min iterations 1000)
                                                do (conskivi-hget database key (format nil "field:~d" i))))
        (declare (ignore r))
        (conskivi-del database key)
        (values nil (+ set-time get-time))))))

(defun bench-conskivi-zadd-zscore (database iterations)
  (let ((key "bench:zset"))
    (multiple-value-bind (r add-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (conskivi-zadd database key (float i) (format nil "member:~d" i))))
      (declare (ignore r))
      (multiple-value-bind (r score-time) (time-it ()
                                            (loop for i from 0 below (min iterations 1000)
                                                  do (conskivi-zscore database key (format nil "member:~d" i))))
        (declare (ignore r))
        (conskivi-del database key)
        (values nil (+ add-time score-time))))))

;;; Redis benchmarks

(defun bench-redis-set-get (iterations)
  (let ((keys (loop for i from 0 below iterations
                    collect (format nil "bench:~d" i))))
    (multiple-value-bind (r set-time) (time-it ()
                                        (dolist (key keys)
                                          (red:set key "value")))
      (declare (ignore r))
      (multiple-value-bind (r get-time) (time-it ()
                                          (dolist (key keys)
                                            (red:get key)))
        (declare (ignore r))
        (dolist (key keys)
          (red:del key))
        (values nil (+ set-time get-time))))))

(defun bench-redis-lpush-lindex (iterations)
  (let ((key "bench:list"))
    (multiple-value-bind (r push-time) (time-it ()
                                         (loop for i from 0 below iterations
                                               do (red:lpush key i)))
      (declare (ignore r))
      (multiple-value-bind (r index-time) (time-it ()
                                            (loop for i from 0 below (min iterations 1000)
                                                  do (red:lindex key i)))
        (declare (ignore r))
        (red:del key)
        (values nil (+ push-time index-time))))))

(defun bench-redis-sadd-scard (iterations)
  (let ((key "bench:set"))
    (multiple-value-bind (r add-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (red:sadd key (format nil "member:~d" i))))
      (declare (ignore r))
      (multiple-value-bind (r card-time) (time-it ()
                                           (loop repeat 1000
                                                 do (red:scard key)))
        (declare (ignore r))
        (red:del key)
        (values nil (+ add-time card-time))))))

(defun bench-redis-hset-hget (iterations)
  (let ((key "bench:hash"))
    (multiple-value-bind (r set-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (red:hset key (format nil "field:~d" i) i)))
      (declare (ignore r))
      (multiple-value-bind (r get-time) (time-it ()
                                          (loop for i from 0 below (min iterations 1000)
                                                do (red:hget key (format nil "field:~d" i))))
        (declare (ignore r))
        (red:del key)
        (values nil (+ set-time get-time))))))

(defun bench-redis-zadd-zscore (iterations)
  (let ((key "bench:zset"))
    (multiple-value-bind (r add-time) (time-it ()
                                        (loop for i from 0 below iterations
                                              do (red:zadd key (float i) (format nil "member:~d" i))))
      (declare (ignore r))
      (multiple-value-bind (r score-time) (time-it ()
                                            (loop for i from 0 below (min iterations 1000)
                                                  do (red:zscore key (format nil "member:~d" i))))
        (declare (ignore r))
        (red:del key)
        (values nil (+ add-time score-time))))))

;;; Multi-threaded benchmarks

(defun bench-conskivi-set-get-threaded (database iterations num-threads)
  (let* ((per-thread (floor iterations num-threads))
         (threads nil))
    (time-it ()
      (dotimes (tnum num-threads)
        (push (bt:make-thread
               (lambda ()
                 (loop for i from 0 below per-thread
                       for key = (format nil "bench:~a:~d" tnum i)
                       do (conskivi-put database key "value")
                          (conskivi-get database key)))
               :name (format nil "conskivi-bench-~d" tnum))
              threads))
      (dolist (thread threads)
        (bt:join-thread thread)))))

(defun bench-redis-set-get-threaded (iterations num-threads)
  (let* ((per-thread (floor iterations num-threads))
         (threads nil))
    (time-it ()
      (dotimes (tnum num-threads)
        (push (bt:make-thread
               (lambda ()
                 (redis:with-connection ()
                   (loop for i from 0 below per-thread
                         for key = (format nil "bench:~a:~d" tnum i)
                         do (red:set key "value")
                            (red:get key))))
               :name (format nil "redis-bench-~d" tnum))
              threads))
      (dolist (thread threads)
        (bt:join-thread thread)))))

;;; Cleanup

(defun delete-directory (path)
  "Recursively delete a directory."
  (when (probe-file path)
    (dolist (entry (directory (merge-pathnames "*.*" path)))
      (if (pathname-name entry)
          (delete-file entry)
          (delete-directory entry)))
    (sb-ext:delete-directory path)))

;;; Main benchmark runner

(defun run-benchmark (&key (iterations 100000) (threads-list '(1 2 4 8)))
  (let* ((db-path (merge-pathnames "conskivi-bench-fileonly/" (uiop:temporary-directory)))
         (database (make-instance 'conskivi-fileonly-database
                                  :db-path db-path
                                  :ttl-interval 60000)))
    (unwind-protect
         (progn
           (conskivi-start database)
           (redis:with-connection ()
             (red:flushall))

           (print-header)

           ;; === Single-threaded ===
           (format t "--- Single-threaded (~d iterations) ---~%~%" iterations)
           (format t "  ~20a | ~12a | ~12a | ~7a~%"
                   "Operation" "conskivi" "Redis" "Ratio")
           (format t "  --------------------+--------------+--------------+--------~%")

           ;; String SET/GET
           (multiple-value-bind (ct c-time) (bench-conskivi-set-get database iterations)
             (declare (ignore ct))
             (multiple-value-bind (rt r-time) (redis:with-connection () (bench-redis-set-get iterations))
               (declare (ignore rt))
               (print-row "SET/GET"
                          (ops-per-sec iterations c-time)
                          (ops-per-sec iterations r-time))))

           ;; List LPUSH/LINDEX
           (multiple-value-bind (ct c-time) (bench-conskivi-lpush-lindex database iterations)
             (declare (ignore ct))
             (multiple-value-bind (rt r-time) (redis:with-connection () (bench-redis-lpush-lindex iterations))
               (declare (ignore rt))
               (print-row "LPUSH/LINDEX"
                          (ops-per-sec iterations c-time)
                          (ops-per-sec iterations r-time))))

           ;; Set SADD/SCARD
           (multiple-value-bind (ct c-time) (bench-conskivi-sadd-scard database iterations)
             (declare (ignore ct))
             (multiple-value-bind (rt r-time) (redis:with-connection () (bench-redis-sadd-scard iterations))
               (declare (ignore rt))
               (print-row "SADD/SCARD"
                          (ops-per-sec iterations c-time)
                          (ops-per-sec iterations r-time))))

           ;; Hash HSET/HGET
           (multiple-value-bind (ct c-time) (bench-conskivi-hset-hget database iterations)
             (declare (ignore ct))
             (multiple-value-bind (rt r-time) (redis:with-connection () (bench-redis-hset-hget iterations))
               (declare (ignore rt))
               (print-row "HSET/HGET"
                          (ops-per-sec iterations c-time)
                          (ops-per-sec iterations r-time))))

           ;; Sorted Set ZADD/ZSCORE
           (multiple-value-bind (ct c-time) (bench-conskivi-zadd-zscore database iterations)
             (declare (ignore ct))
             (multiple-value-bind (rt r-time) (redis:with-connection () (bench-redis-zadd-zscore iterations))
               (declare (ignore rt))
               (print-row "ZADD/ZSCORE"
                          (ops-per-sec iterations c-time)
                          (ops-per-sec iterations r-time))))

           ;; === Multi-threaded ===
           (format t "~%--- Multi-threaded SET/GET (~d total iterations) ---~%~%" iterations)
           (format t "  ~6a | ~12a | ~12a | ~7a~%"
                   "Threads" "conskivi" "Redis" "Ratio")
           (format t "  ------+--------------+--------------+--------~%")

           (dolist (n threads-list)
              (multiple-value-bind (ct c-time)
                  (bench-conskivi-set-get-threaded database iterations n)
                (declare (ignore ct))
                (multiple-value-bind (rt r-time)
                    (bench-redis-set-get-threaded iterations n)
                  (declare (ignore rt))
                  (print-thread-row n
                                    (ops-per-sec iterations c-time)
                                    (ops-per-sec iterations r-time)))))

           (format t "~%=== Done ===~%"))

      ;; Cleanup
      (conskivi-stop database)
      (delete-directory db-path))))
