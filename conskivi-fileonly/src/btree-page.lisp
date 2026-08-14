;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; B+Tree Page Layout Constants
;;;
;;; File format: meta page (page 0) + B+tree pages
;;; Each page is 4096 bytes. Page 0 is always the meta page.

(defconstant +page-size+ 4096)
(defconstant +page-header-size+ 16)
(defconstant +page-usable-size+ (- +page-size+ +page-header-size+))

;;; Page type constants

(defconstant +page-type-branch+  #x01)
(defconstant +page-type-leaf+    #x02)
(defconstant +page-type-free+    #xFF)

;;; Meta page layout (page 0)
;;;
;;; Offset  Size  Field
;;; 0       4     Magic: 0x434B5450 ("CKTP")
;;; 4       4     Format version (uint32 LE)
;;; 8       4     Page size (uint32 LE, always 4096)
;;; 12      1     Collection type tag (10=set, 7=hash, 8=zset)
;;; 13      8     Expiration (i64 LE, universal-time, 0=no expiry)
;;; 21      4     Score-tree root page (uint32 LE)
;;; 25      4     Member-tree root page (uint32 LE, 0 if not zset)
;;; 29      4     Total page count (uint32 LE)
;;; 33      4     Entry count (uint32 LE)
;;; 37      4     Free list head page (uint32 LE, 0=empty)
;;; 41      4     Free page count (uint32 LE)
;;; 45      451   Reserved

(defconstant +meta-magic+ #x434B5450) ; "CKTP"
(defconstant +meta-version+ 1)

(defconstant +meta-offset-magic+          0)
(defconstant +meta-offset-version+        4)
(defconstant +meta-offset-pagesize+       8)
(defconstant +meta-offset-type+           12)
(defconstant +meta-offset-expiration+     13)
(defconstant +meta-offset-score-root+     21)
(defconstant +meta-offset-member-root+    25)
(defconstant +meta-offset-page-count+     29)
(defconstant +meta-offset-entry-count+    33)
(defconstant +meta-offset-free-list+      37)
(defconstant +meta-offset-free-count+     41)

;;; Page header layout
;;;
;;; Offset  Size  Field
;;; 0       4     Page number (uint32 LE)
;;; 4       1     Page type (branch/leaf/free)
;;; 5       1     Flags (bit 0=dirty, bit 1=rightmost)
;;; 6       2     Entry count (uint16 LE)
;;; 8       4     Next leaf page (uint32 LE, 0=none)
;;; 12      2     Free space in page (uint16 LE)
;;; 14      2     Reserved

(defconstant +hdr-offset-pagenum+    0)
(defconstant +hdr-offset-type+       4)
(defconstant +hdr-offset-flags+      5)
(defconstant +hdr-offset-count+      6)
(defconstant +hdr-offset-next+       8)
(defconstant +hdr-offset-free-space+ 12)

;;; Flag bits

(defconstant +flag-dirty+     #x01)
(defconstant +flag-rightmost+ #x02)

;;; In-memory page structure

(defstruct (btree-page (:conc-name page-))
  (number    0             :type (unsigned-byte 32))
  (type      +page-type-leaf+ :type (unsigned-byte 8))
  (flags     0             :type (unsigned-byte 8))
  (count     0             :type (unsigned-byte 16))
  (next      0             :type (unsigned-byte 32))
  (free-space +page-usable-size+ :type (unsigned-byte 16))
  (data      (make-array +page-usable-size+ :element-type '(unsigned-byte 8) :initial-element 0)
             :type (simple-array (unsigned-byte 8) (*))))

;;; Meta page structure

(defstruct (btree-meta (:conc-name meta-))
  (magic       +meta-magic+    :type (unsigned-byte 32))
  (version     +meta-version+  :type (unsigned-byte 32))
  (page-size   +page-size+     :type (unsigned-byte 32))
  (type        0               :type (unsigned-byte 8))
  (expiration  0               :type (signed-byte 64))
  (score-root  0               :type (unsigned-byte 32))
  (member-root 0               :type (unsigned-byte 32))
  (page-count  0               :type (unsigned-byte 32))
  (entry-count 0               :type (unsigned-byte 32))
  (free-list   0               :type (unsigned-byte 32))
  (free-count  0               :type (unsigned-byte 32)))

;;; Slot directory entry (within a page)
;;; Stored at the END of the page, growing backward.
;;; Each slot: 4 bytes = [offset:2][length:2]

(defconstant +slot-size+ 4)

(defun slot-offset (slot-index)
  "Byte offset of slot directory entry from end of usable area."
  (- +page-usable-size+ (* (1+ slot-index) +slot-size+)))

;;; Page read/write from byte vector

(defun read-page-header (page)
  "Parse header from page data vector into page struct fields."
  (let ((data (page-data page)))
    (setf (page-number page)  (bytes-to-u32 data +hdr-offset-pagenum+))
    (setf (page-type page)    (aref data +hdr-offset-type+))
    (setf (page-flags page)   (aref data +hdr-offset-flags+))
    (setf (page-count page)   (bytes-to-u16 data +hdr-offset-count+))
    (setf (page-next page)    (bytes-to-u32 data +hdr-offset-next+))
    (setf (page-free-space page) (bytes-to-u16 data +hdr-offset-free-space+))
    page))

(defun write-page-header (page)
  "Write page struct fields into data vector header."
  (let ((data (page-data page)))
    (u32-to-bytes data +hdr-offset-pagenum+ (page-number page))
    (setf (aref data +hdr-offset-type+) (page-type page))
    (setf (aref data +hdr-offset-flags+) (page-flags page))
    (u16-to-bytes data +hdr-offset-count+ (page-count page))
    (u32-to-bytes data +hdr-offset-next+ (page-next page))
    (u16-to-bytes data +hdr-offset-free-space+ (page-free-space page))
    page))

;;; Meta page read/write

(defun read-meta (data)
  "Parse meta page from byte vector."
  (make-btree-meta
   :magic       (bytes-to-u32 data +meta-offset-magic+)
   :version     (bytes-to-u32 data +meta-offset-version+)
   :page-size   (bytes-to-u32 data +meta-offset-pagesize+)
   :type        (aref data +meta-offset-type+)
   :expiration  (bytes-to-i64 data +meta-offset-expiration+)
   :score-root  (bytes-to-u32 data +meta-offset-score-root+)
   :member-root (bytes-to-u32 data +meta-offset-member-root+)
   :page-count  (bytes-to-u32 data +meta-offset-page-count+)
   :entry-count (bytes-to-u32 data +meta-offset-entry-count+)
   :free-list   (bytes-to-u32 data +meta-offset-free-list+)
   :free-count  (bytes-to-u32 data +meta-offset-free-count+)))

(defun write-meta (meta data)
  "Write meta page to byte vector."
  (u32-to-bytes data +meta-offset-magic+ (meta-magic meta))
  (u32-to-bytes data +meta-offset-version+ (meta-version meta))
  (u32-to-bytes data +meta-offset-pagesize+ (meta-page-size meta))
  (setf (aref data +meta-offset-type+) (meta-type meta))
  (i64-to-bytes data +meta-offset-expiration+ (meta-expiration meta))
  (u32-to-bytes data +meta-offset-score-root+ (meta-score-root meta))
  (u32-to-bytes data +meta-offset-member-root+ (meta-member-root meta))
  (u32-to-bytes data +meta-offset-page-count+ (meta-page-count meta))
  (u32-to-bytes data +meta-offset-entry-count+ (meta-entry-count meta))
  (u32-to-bytes data +meta-offset-free-list+ (meta-free-list meta))
  (u32-to-bytes data +meta-offset-free-count+ (meta-free-count meta))
  meta)

;;; Byte conversion helpers

(defun bytes-to-u16 (data offset)
  "Read uint16 LE from byte vector."
  (logior (aref data offset)
          (ash (aref data (1+ offset)) 8)))

(defun u16-to-bytes (data offset value)
  "Write uint16 LE to byte vector."
  (setf (aref data offset) (logand value #xFF))
  (setf (aref data (1+ offset)) (logand (ash value -8) #xFF)))

(defun bytes-to-u32 (data offset)
  "Read uint32 LE from byte vector."
  (logior (aref data offset)
          (ash (aref data (+ offset 1)) 8)
          (ash (aref data (+ offset 2)) 16)
          (ash (aref data (+ offset 3)) 24)))

(defun u32-to-bytes (data offset value)
  "Write uint32 LE to byte vector."
  (setf (aref data offset) (logand value #xFF))
  (setf (aref data (+ offset 1)) (logand (ash value -8) #xFF))
  (setf (aref data (+ offset 2)) (logand (ash value -16) #xFF))
  (setf (aref data (+ offset 3)) (logand (ash value -24) #xFF)))

(defun bytes-to-i64 (data offset)
  "Read int64 LE from byte vector."
  (let ((value 0))
    (dotimes (i 8)
      (setf (ldb (byte 8 (* i 8)) value) (aref data (+ offset i))))
    (when (>= value (ash 1 63))
      (setf value (- value (ash 1 64))))
    value))

(defun i64-to-bytes (data offset value)
  "Write int64 LE to byte vector."
  (let ((v value))
    (dotimes (i 8)
      (setf (aref data (+ offset i)) (logand v #xFF))
      (setf v (ash v -8)))))

(defun bytes-to-double (data offset)
  "Read IEEE 754 double LE from byte vector."
  (ieee-floats:decode-float64 (bytes-to-i64 data offset)))

(defun double-to-bytes (data offset value)
  "Write IEEE 754 double LE to byte vector."
  (i64-to-bytes data offset (ieee-floats:encode-float64 (coerce value 'double-float))))

;;; Entry encoding within pages
;;;
;;; Entries are stored starting at offset +page-header-size+ (16).
;;; Slot directory grows backward from end of page.
;;; Slot entry: [entry-offset:2][entry-length:2]

(defun page-entry-start (page slot-index)
  "Get byte offset of entry at given slot index within page data."
  (let ((offset +page-header-size+))
    ;; Sum up lengths of all previous entries
    (dotimes (i slot-index)
      (incf offset (slot-directory-length page i)))
    offset))

(defun slot-directory-offset (page slot-index)
  "Get byte offset of slot directory entry."
  (- +page-usable-size+ (* (1+ slot-index) +slot-size+)))

(defun slot-directory-offset-of (page slot-index)
  "Get stored offset of entry from slot directory."
  (let ((soff (slot-directory-offset page slot-index)))
    (bytes-to-u16 (page-data page) soff)))

(defun slot-directory-length (page slot-index)
  "Get stored length of entry from slot directory."
  (let ((soff (slot-directory-offset page slot-index)))
    (bytes-to-u16 (page-data page) (+ soff 2))))

(defun slot-directory-set (page slot-index entry-offset entry-length)
  "Write slot directory entry."
  (let ((soff (slot-directory-offset page slot-index))
        (data (page-data page)))
    (u16-to-bytes data soff entry-offset)
    (u16-to-bytes data (+ soff 2) entry-length)))

(defun page-can-fit-p (page entry-length)
  "Check if an entry of given length can fit in the page.
The page layout: header (16 bytes) | entry data (grows forward) | gap | slot directory (grows backward, 4 bytes per slot).
Free gap = slot-directory-start - entry-data-end."
  (let ((count (page-count page)))
    ;; Compute where entry data ends
    (let ((entry-data-end +page-header-size+))
      (dotimes (i count)
        (incf entry-data-end (slot-directory-length page i)))
      ;; Slot directory for count+1 entries starts here
      (let ((slot-dir-start (- +page-usable-size+ (* (1+ count) +slot-size+))))
        (>= (- slot-dir-start entry-data-end) entry-length)))))

(defun page-insert-entry-at (page slot-index entry-bytes entry-length)
  "Insert entry at given slot index. Shifts subsequent entries right."
  (let ((data (page-data page))
        (count (page-count page)))
    ;; Step 1: Shift existing entries (from slot-index to end) to make room
    (when (< slot-index count)
      ;; Calculate total bytes of entries from slot-index to end
      (let ((bytes-to-shift 0))
        (loop for i from slot-index below count
              do (incf bytes-to-shift (slot-directory-length page i)))
        ;; Shift entry data right
        (let ((src-start (page-entry-start page slot-index))
              (dst-start (+ (page-entry-start page slot-index) entry-length)))
          (loop for i from (+ src-start bytes-to-shift -1) downto src-start
                do (setf (aref data (+ i entry-length)) (aref data i))))))
    ;; Step 2: Shift slot directory entries right by one position
    (loop for i from (1- count) downto slot-index
          do (let ((src-soff (slot-directory-offset page i))
                   (dst-soff (slot-directory-offset page (1+ i))))
               (setf (aref data dst-soff) (aref data src-soff))
               (setf (aref data (+ dst-soff 1)) (aref data (+ src-soff 1)))
               (setf (aref data (+ dst-soff 2)) (aref data (+ src-soff 2)))
               (setf (aref data (+ dst-soff 3)) (aref data (+ src-soff 3)))))
    ;; Step 3: Write new entry bytes
    (let ((entry-offset (page-entry-start page slot-index)))
      (replace data entry-bytes :start1 entry-offset :end1 (+ entry-offset entry-length)))
    ;; Step 4: Write slot directory entry for new slot
    (slot-directory-set page slot-index
                       (page-entry-start page slot-index)
                       entry-length)
    ;; Step 5: Update page metadata
    (setf (page-count page) (1+ count))
    (setf (page-free-space page) (page-can-fit-free-space page))
    page))

(defun page-can-fit-free-space (page)
  "Compute actual free space in page (gap between entry data and slot directory)."
  (let ((count (page-count page)))
    (let ((entry-data-end +page-header-size+))
      (dotimes (i count)
        (incf entry-data-end (slot-directory-length page i)))
      (let ((slot-dir-start (- +page-usable-size+ (* count +slot-size+))))
        (- slot-dir-start entry-data-end)))))

(defun slot-total-size-at (page slot-index)
  "Get total bytes consumed by entry at slot-index (entry-length)."
  (slot-directory-length page slot-index))

;;; Page allocation and free list
;;;
;;; Free list: each free page stores next free page number at offset 0.
;;; The meta page's free-list points to the head.

(defun allocate-page-number (meta)
  "Allocate a page number. Returns new page number and updated meta."
  (if (> (meta-free-count meta) 0)
      ;; Reuse from free list
      (let ((page-num (meta-free-list meta)))
        (setf (meta-free-list meta) page-num) ; will be read from page data on load
        (decf (meta-free-count meta))
        (values page-num meta))
      ;; Append new page
      (let ((page-num (meta-page-count meta)))
        (incf (meta-page-count meta))
        (values page-num meta))))

(defun free-page (meta page-num)
  "Add a page to the free list."
  ;; Store current free list head at offset 0 of the freed page
  ;; (This is done externally by writing to the page data)
  (setf (meta-free-list meta) page-num)
  (incf (meta-free-count meta))
  meta)

;;; Create empty page

(defun make-empty-page (number type)
  "Create a new empty page with given number and type."
  (let ((page (make-btree-page :number number :type type)))
    (setf (page-free-space page) +page-usable-size+)
    (write-page-header page)
    page))

;;; Entry encoding/decoding helpers for leaf entries
;;;
;;; Set entry: [status:1][member-len:4][member-data:N]
;;; Hash entry: [status:1][field-len:4][field-data:N][value-len:4][value-data:M]
;;; ZSet leaf (score-tree): [status:1][score:8][member-len:4][member-data:N]
;;; ZSet leaf (member-tree): [status:1][member-len:4][member-data:N][score:8]
;;; Branch entry: [child-page:4][key-len:4][key-data:N]

(defun encode-set-entry (status member-bytes)
  "Encode a set leaf entry."
  (let* ((mlen (length member-bytes))
         (result (make-array (+ 1 4 mlen) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) status)
    (u32-to-bytes result 1 mlen)
    (replace result member-bytes :start1 5)
    result))

(defun decode-set-entry (data offset)
  "Decode a set leaf entry. Returns (values status member-bytes next-offset)."
  (let ((status (aref data offset))
        (mlen (bytes-to-u32 data (+ offset 1))))
    (values status
            (subseq data (+ offset 5) (+ offset 5 mlen))
            (+ offset 1 4 mlen))))

(defun encode-hash-entry (status field-bytes value-bytes)
  "Encode a hash leaf entry."
  (let* ((flen (length field-bytes))
         (vlen (length value-bytes))
         (result (make-array (+ 1 4 flen 4 vlen) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) status)
    (u32-to-bytes result 1 flen)
    (replace result field-bytes :start1 5)
    (u32-to-bytes result (+ 5 flen) vlen)
    (replace result value-bytes :start1 (+ 5 flen 4))
    result))

(defun decode-hash-entry (data offset)
  "Decode a hash leaf entry. Returns (values status field-bytes value-bytes next-offset)."
  (let* ((status (aref data offset))
         (flen (bytes-to-u32 data (+ offset 1)))
         (field (subseq data (+ offset 5) (+ offset 5 flen)))
         (vlen (bytes-to-u32 data (+ offset 5 flen)))
         (value (subseq data (+ offset 5 flen 4) (+ offset 5 flen 4 vlen))))
    (values status field value (+ offset 1 4 flen 4 vlen))))

(defun encode-zset-leaf-entry (status score member-bytes)
  "Encode a sorted set leaf entry (score-tree)."
  (let* ((mlen (length member-bytes))
         (result (make-array (+ 1 8 4 mlen) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) status)
    (double-to-bytes result 1 score)
    (u32-to-bytes result 9 mlen)
    (replace result member-bytes :start1 13)
    result))

(defun decode-zset-leaf-entry (data offset)
  "Decode a sorted set leaf entry. Returns (values status score member-bytes next-offset)."
  (let* ((status (aref data offset))
         (score (bytes-to-double data (+ offset 1)))
         (mlen (bytes-to-u32 data (+ offset 9)))
         (member (subseq data (+ offset 13) (+ offset 13 mlen))))
    (values status score member (+ offset 1 8 4 mlen))))

(defun encode-zset-member-entry (status member-bytes score)
  "Encode a sorted set member-tree entry."
  (let* ((mlen (length member-bytes))
         (result (make-array (+ 1 4 mlen 8) :element-type '(unsigned-byte 8))))
    (setf (aref result 0) status)
    (u32-to-bytes result 1 mlen)
    (replace result member-bytes :start1 5)
    (double-to-bytes result (+ 5 mlen) score)
    result))

(defun decode-zset-member-entry (data offset)
  "Decode a sorted set member-tree entry. Returns (values status member-bytes score next-offset)."
  (let* ((status (aref data offset))
         (mlen (bytes-to-u32 data (+ offset 1)))
         (member (subseq data (+ offset 5) (+ offset 5 mlen)))
         (score (bytes-to-double data (+ offset 5 mlen))))
    (values status member score (+ offset 1 4 mlen 8))))

(defun encode-branch-entry (child-page key-bytes)
  "Encode a branch entry."
  (let* ((klen (length key-bytes))
         (result (make-array (+ 4 4 klen) :element-type '(unsigned-byte 8))))
    (u32-to-bytes result 0 child-page)
    (u32-to-bytes result 4 klen)
    (replace result key-bytes :start1 8)
    result))

(defun decode-branch-entry (data offset)
  "Decode a branch entry. Returns (values child-page key-bytes next-offset)."
  (let* ((child (bytes-to-u32 data offset))
         (klen (bytes-to-u32 data (+ offset 4)))
         (key (if (> klen 0) (subseq data (+ offset 8) (+ offset 8 klen)) #())))
    (values child key (+ offset 4 4 klen))))

;;; Entry length calculation

(defun set-entry-length (member-bytes)
  (+ 1 4 (length member-bytes)))

(defun hash-entry-length (field-bytes value-bytes)
  (+ 1 4 (length field-bytes) 4 (length value-bytes)))

(defun zset-leaf-entry-length (member-bytes)
  (+ 1 8 4 (length member-bytes)))

(defun zset-member-entry-length (member-bytes)
  (+ 1 4 (length member-bytes) 8))

(defun branch-entry-length (key-bytes)
  (+ 4 4 (length key-bytes)))

;;; Convert Lisp values to byte vectors for storage

(defun string-to-utf8-bytes (string)
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun symbol-to-utf8-bytes (symbol)
  (string-to-utf8-bytes (if (symbolp symbol) (symbol-name symbol) (format nil "~a" symbol))))

(defun value-to-key-bytes (value)
  "Convert a key value (member/field) to bytes for B+tree storage.
   Strings are stored as UTF-8. Symbols/keywords are stored as their name."
  (cond
    ((stringp value) (string-to-utf8-bytes value))
    ((symbolp value) (symbol-to-utf8-bytes value))
    (t (string-to-utf8-bytes (format nil "~a" value)))))

(defun key-bytes-compare (a b)
  "Compare two key byte vectors. Returns :less, :equal, or :greater."
  (let ((la (length a)) (lb (length b)))
    (cond
      ((and (= la 0) (= lb 0)) :equal)
      ((= la 0) :less)
      ((= lb 0) :greater)
      (t (let ((cmp (loop for i from 0 below (min la lb)
                          for ca = (aref a i)
                          for cb = (aref b i)
                          unless (= ca cb)
                            return (if (< ca cb) :less :greater))))
           (if cmp cmp
               (if (< la lb) :less
                   (if (> la lb) :greater :equal))))))))

(defun key-bytes-lessp (a b)
  "Returns T if key-bytes A < B."
  (eq (key-bytes-compare a b) :less))

(defun key-bytes-equalp (a b)
  "Returns T if key-bytes A = B."
  (eq (key-bytes-compare a b) :equal))
