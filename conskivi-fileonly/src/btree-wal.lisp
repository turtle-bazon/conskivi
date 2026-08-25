;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; Write-Ahead Log (WAL) for crash safety
;;;
;;; Before modifying the mmap'd B+tree file, we first write the change
;;; to a WAL file. This ensures crash safety:
;;;
;;; 1. Write change to WAL file (sequential, fast)
;;; 2. fsync WAL file
;;; 3. Apply change to mmap'd B+tree file
;;; 4. msync B+tree file
;;; 5. Truncate WAL entry
;;;
;;; On recovery:
;;; - If WAL has uncommitted entries, replay them to the B+tree file
;;; - Then truncate the WAL

;;; WAL file format
;;;
;;; Each WAL entry:
;;;   4 bytes: magic (0x57414C31 = "WAL1")
;;;   4 bytes: entry type (1=page-write, 2=meta-update)
;;;   4 bytes: page number (for page-write) or 0 (for meta-update)
;;;   4 bytes: data length
;;;   N bytes: data (page data or meta data)
;;;   4 bytes: CRC32 checksum
;;;   4 bytes: end marker (0x454E4420 = "END ")

(defconstant +wal-magic+ #x57414C31)     ; "WAL1"
(defconstant +wal-end-marker+ #x454E4420) ; "END "

(defconstant +wal-entry-page-write+ 1)
(defconstant +wal-entry-meta-update+ 2)

(defconstant +wal-header-size+ 16)  ; magic + type + page-num + data-len
(defconstant +wal-footer-size+ 8)   ; checksum + end-marker

;;; WAL structure

(defstruct (wal (:conc-name wal-))
  (file-path nil)          ; pathname to .wal file
  (stream nil)             ; open output stream
  (fd nil)                 ; file descriptor for fsync
  (entry-count 0))         ; number of entries in WAL

;;; WAL entry structure

(defstruct (wal-entry (:conc-name wal-entry-))
  (type 0 :type (unsigned-byte 32))
  (page-num 0 :type (unsigned-byte 32))
  (data nil :type (or null (simple-array (unsigned-byte 8) (*)))))

;;; CRC32 checksum

(defun crc32 (data &optional (start 0) (end (length data)))
  "Compute CRC32 checksum of data."
  (let ((crc #xFFFFFFFF))
    (dotimes (i (- end start))
      (setf crc (logxor crc (aref data (+ start i))))
      (dotimes (j 8)
        (setf crc (if (logbitp 0 crc)
                      (logxor (ash crc -1) #xEDB88320)
                      (ash crc -1)))))
    (logxor crc #xFFFFFFFF)))

;;; WAL operations

(defun wal-create (file-path)
  "Create a new WAL file."
  (let ((wal (make-wal :file-path file-path)))
    (ensure-directories-exist file-path)
    (setf (wal-stream wal)
          (open file-path :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)))
    (setf (wal-fd wal)
          (sb-posix:open (namestring file-path)
                         (logior sb-posix:o-rdwr)
                         #o644))
    wal))

(defun wal-open (file-path)
  "Open an existing WAL file for appending."
  (let ((wal (make-wal :file-path file-path)))
    (when (probe-file file-path)
      (setf (wal-stream wal)
            (open file-path :direction :io :if-exists :append
                             :element-type '(unsigned-byte 8)))
      (setf (wal-fd wal)
            (sb-posix:open (namestring file-path)
                           (logior sb-posix:o-rdwr)
                           #o644))
      ;; Count existing entries
      (file-position (wal-stream wal) 0)
      (loop while (< (file-position (wal-stream wal))
                      (file-length (wal-stream wal)))
            do (let ((magic (read-u32 (wal-stream wal))))
                 (when (= magic +wal-magic+)
                   (let ((type (read-u32 (wal-stream wal)))
                         (page-num (read-u32 (wal-stream wal)))
                         (data-len (read-u32 (wal-stream wal))))
                     (declare (ignore type page-num))
                     (file-position (wal-stream wal)
                                    (+ (file-position (wal-stream wal)) data-len))
                     ;; Skip checksum and end marker
                     (read-u32 (wal-stream wal))
                     (read-u32 (wal-stream wal))
                     (incf (wal-entry-count wal))))))
      ;; Seek to end for appending
      (file-position (wal-stream wal)
                     (file-length (wal-stream wal))))
    wal))

(defun wal-close (wal)
  "Close the WAL file."
  (when (wal-stream wal)
    (close (wal-stream wal))
    (setf (wal-stream wal) nil))
  (when (wal-fd wal)
    (sb-posix:close (wal-fd wal))
    (setf (wal-fd wal) nil)))

(defun wal-fsync (wal)
  "Sync WAL file to disk."
  (when (wal-fd wal)
    (finish-output (wal-stream wal))
    (sb-posix:fsync (wal-fd wal))))

(defun wal-write-entry (wal entry)
  "Write a WAL entry to the file."
  (let ((stream (wal-stream wal))
        (data (wal-entry-data entry)))
    ;; Write header
    (write-u32 stream +wal-magic+)
    (write-u32 stream (wal-entry-type entry))
    (write-u32 stream (wal-entry-page-num entry))
    (write-u32 stream (length data))
    ;; Write data
    (write-sequence data stream)
    ;; Compute and write checksum
    (let* ((entry-len (+ 16 (length data)))
           (entry-data (make-array entry-len :element-type '(unsigned-byte 8))))
      ;; Copy header
      (setf (aref entry-data 0) #x31) ; "WAL1"
      (setf (aref entry-data 1) #x4C)
      (setf (aref entry-data 2) #x41)
      (setf (aref entry-data 3) #x57)
      ;; Copy type, page-num, data-len
      (u32-to-bytes entry-data 4 (wal-entry-type entry))
      (u32-to-bytes entry-data 8 (wal-entry-page-num entry))
      (u32-to-bytes entry-data 12 (length data))
      ;; Copy data
      (replace entry-data data :start1 16)
      ;; Compute checksum
      (let ((checksum (crc32 entry-data)))
        (write-u32 stream checksum)))
    ;; Write end marker
    (write-u32 stream +wal-end-marker+)
    (incf (wal-entry-count wal))))

(defun wal-truncate (wal)
  "Truncate the WAL file (after all entries are applied)."
  (when (wal-stream wal)
    (close (wal-stream wal))
    (setf (wal-stream wal) nil))
  (when (wal-fd wal)
    (sb-posix:close (wal-fd wal))
    (setf (wal-fd wal) nil))
  ;; Truncate file
  (when (probe-file (wal-file-path wal))
    (delete-file (wal-file-path wal)))
  (setf (wal-entry-count wal) 0))

(defun wal-empty-p (wal)
  "Check if WAL is empty."
  (zerop (wal-entry-count wal)))

;;; WAL replay

(defun wal-replay (wal mmap-sap)
  "Replay WAL entries to the mmap'd file."
  (when (wal-empty-p wal)
    (return-from wal-replay))
  (format t "Replaying ~d WAL entries...~%" (wal-entry-count wal))
  (let ((stream (open (wal-file-path wal) :direction :input
                                          :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop while (< (file-position stream) (file-length stream))
               do (wal-replay-entry stream mmap-sap))
      (close stream)))
  (sb-posix:msync mmap-sap 0 sb-posix:ms-sync)
  (format t "WAL replay complete.~%"))

(defun wal-replay-entry (stream mmap-sap)
  "Replay a single WAL entry from stream."
  (let ((magic (read-u32 stream)))
    (when (/= magic +wal-magic+)
      (return-from wal-replay-entry))
    (let* ((type (read-u32 stream))
           (page-num (read-u32 stream))
           (data-len (read-u32 stream))
           (data (make-array data-len :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      (let ((stored-checksum (read-u32 stream))
            (end-marker (read-u32 stream)))
        (when (/= end-marker +wal-end-marker+)
          (return-from wal-replay-entry))
        ;; Verify checksum
        (let ((entry-data (make-array (+ 16 data-len) :element-type '(unsigned-byte 8))))
          (u32-to-bytes entry-data 0 +wal-magic+)
          (u32-to-bytes entry-data 4 type)
          (u32-to-bytes entry-data 8 page-num)
          (u32-to-bytes entry-data 12 data-len)
          (replace entry-data data :start1 16)
          (let ((computed-checksum (crc32 entry-data)))
            (when (/= computed-checksum stored-checksum)
              (warn "WAL checksum mismatch at page ~d, skipping" page-num)
              (return-from wal-replay-entry))
            ;; Apply to mmap
            (case type
              (#.+wal-entry-page-write+
               (let ((offset (* page-num +page-size+)))
                 (dotimes (i data-len)
                   (setf (cffi:mem-ref mmap-sap :unsigned-char (+ offset i))
                         (aref data i)))))
              (#.+wal-entry-meta-update+
               (dotimes (i data-len)
                 (setf (cffi:mem-ref mmap-sap :unsigned-char i)
                       (aref data i)))))))))))

;;; Helper functions for reading/writing u32
;;; (read-u32, write-u32 are in serialization.lisp)
;;; (u32-to-bytes is in btree-page.lisp)
