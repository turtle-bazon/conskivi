;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; B+Tree core algorithms
;;;
;;; In-memory B+tree with disk persistence via mmap.
;;; Copy-on-write for crash safety.

;;; B+tree structure

(defstruct (btree (:conc-name btree-))
  (meta nil)                              ; btree-meta struct
  (page-cache                             ; hash-table: page-number -> btree-page
    (make-hash-table :test #'eql))
  (file-path nil)                         ; pathname to .ck file
  (dirty nil)                             ; has uncommitted changes
  ;; Callbacks for encoding/decoding entry keys
  (encode-key-fn #'value-to-key-bytes)    ; function: value -> bytes
  (decode-key-fn (lambda (b) b)))         ; function: bytes -> value

;;; Page cache operations

(defun btree-read-page (tree page-num)
  "Read a page from cache or disk."
  (or (gethash page-num (btree-page-cache tree))
      (error "Page ~d not in cache" page-num)))

(defun btree-cache-page (tree page)
  "Add a page to the cache."
  (setf (gethash (page-number page) (btree-page-cache tree)) page)
  page)

(defun btree-uncache-page (tree page-num)
  "Remove a page from the cache."
  (remhash page-num (btree-page-cache tree)))

;;; Page allocation (copy-on-write)

(defun btree-allocate-page (tree type)
  "Allocate a new page. Returns the new page."
  (let* ((meta (btree-meta tree))
         (page-num (meta-page-count meta)))
    (incf (meta-page-count meta))
    (let ((page (the btree-page (make-empty-page page-num type))))
      (btree-cache-page tree page)
      (setf (btree-dirty tree) t)
      page)))

(defun btree-copy-page (tree page)
  "Copy a page for copy-on-write. Returns new page with same data."
  (let ((new-page (btree-allocate-page tree (page-type page))))
    (replace (page-data new-page) (page-data page))
    (setf (page-count new-page) (page-count page))
    (setf (page-next new-page) (page-next page))
    (setf (page-free-space new-page) (page-free-space page))
    (setf (page-flags new-page) (page-flags page))
    new-page))

;;; B+Tree operations

(defun btree-init-meta (type expiration)
  "Create initial meta for a new B+tree."
  (make-btree-meta
   :type type
   :expiration (or expiration 0)
   :score-root 0
   :member-root 0
   :page-count 1    ; page 0 is meta
   :entry-count 0
   :free-list 0
   :free-count 0))

(defun btree-create (file-path type &optional expiration)
  "Create a new B+tree file with given collection type."
  (ensure-directories-exist file-path)
  (let* ((meta (btree-init-meta type expiration))
         (tree (make-btree :meta meta :file-path file-path)))
    tree))

(defun btree-open-from-meta (file-path meta-data)
  "Open an existing B+tree from file."
  (let* ((meta (read-meta meta-data))
         (tree (make-btree :meta meta :file-path file-path)))
    tree))

;;; Key comparison within a B+tree

(defun btree-key-compare (tree a-bytes b-bytes)
  "Compare two keys within a B+tree."
  (funcall #'key-bytes-compare a-bytes b-bytes))

(defun btree-key-lessp (tree a-bytes b-bytes)
  (eq (btree-key-compare tree a-bytes b-bytes) :less))

(defun btree-key-equalp (tree a-bytes b-bytes)
  (eq (btree-key-compare tree a-bytes b-bytes) :equal))

;;; Leaf operations

(defun btree-find-leaf (tree root-page-num key-bytes)
  "Traverse from root to leaf containing the given key.
   Returns the leaf page and the slot index where key should be."
  (when (= root-page-num 0)
    (return-from btree-find-leaf (values nil 0)))
  (let ((page (btree-read-page tree root-page-num)))
    (loop while (= (page-type page) +page-type-branch+)
          do (let ((slot (btree-find-slot-in-branch tree page key-bytes)))
               (let ((child-num (branch-child-at page slot)))
                 (setf page (btree-read-page tree child-num)))))
    ;; Now page is a leaf
    (let ((slot (btree-find-slot-in-leaf tree page key-bytes)))
      (values page slot))))

(defun btree-find-slot-in-branch (tree page key-bytes)
  "Find the child pointer index in a branch node for the given key.
   Returns the index of the child to descend into.
   Branch entry convention: entry i has (child_i, key_i).
   Entry 0 has key=#() (empty sentinel). Child i holds keys where
   key_{i-1} <= key < key_i (with key_{-1} = -infinity)."
  (let ((count (page-count page))
        (result 0))
    (loop for i from 0 below count
          for entry-offset = (page-entry-start page i)
          for (child key) = (multiple-value-list
                             (decode-branch-entry (page-data page) entry-offset))
          when (btree-key-lessp tree key-bytes key)
            do (return result)
          do (setf result i))
    result))

(defun branch-child-at (page slot-index)
  "Get child page number at given slot index in a branch node."
  (let ((offset (page-entry-start page slot-index)))
    (bytes-to-u32 (page-data page) offset)))

(defun branch-key-at (tree page slot-index)
  "Get key bytes at given slot index in a branch node."
  (let ((offset (page-entry-start page slot-index)))
    (multiple-value-bind (child key)
        (decode-branch-entry (page-data page) offset)
      (declare (ignore child))
      key)))

(defun btree-find-lowest-key (tree page-num)
  "Find the lowest key in the subtree rooted at page-num."
  (let ((page (btree-read-page tree page-num)))
    (if (= (page-type page) +page-type-branch+)
        ;; Branch - go to first child
        (btree-find-lowest-key tree (branch-child-at page 0))
        ;; Leaf - return first entry's key
        (when (> (page-count page) 0)
          (leaf-entry-key tree page (page-entry-start page 0))))))

(defun btree-branch-key-for-sort (tree entry-bytes)
  "Extract key from branch entry bytes for sorting purposes."
  (multiple-value-bind (child key)
      (decode-branch-entry entry-bytes 0)
    (declare (ignore child))
    (if key key #())))

(defun btree-find-slot-in-leaf (tree page key-bytes)
  "Find the slot index in a leaf node for the given key.
   Returns slot where key >= entries[slot] and key < entries[slot+1]."
  (let* ((count (page-count page))
         (result count))
    (loop for i from 0 below count
          for entry-offset = (page-entry-start page i)
          for entry-key = (leaf-entry-key tree page entry-offset)
          when (or (btree-key-equalp tree key-bytes entry-key)
                   (btree-key-lessp tree key-bytes entry-key))
            do (setf result i)
               (return))
    result))

(defun leaf-entry-key (tree page entry-offset)
  "Extract the key bytes from a leaf entry at given offset."
  (let ((data (page-data page))
        (type (meta-type (btree-meta tree))))
    (ecase type
      (10 ; set
       (multiple-value-bind (status member next)
           (decode-set-entry data entry-offset)
         (declare (ignore status next))
         member))
      (7 ; hash
       (multiple-value-bind (status field value next)
           (decode-hash-entry data entry-offset)
         (declare (ignore status value next))
         field))
      (8 ; zset (score-tree uses score+member as composite key)
       (multiple-value-bind (status score member next)
           (decode-zset-leaf-entry data entry-offset)
         (declare (ignore status score next))
         ;; Member is already stored as key bytes, return directly
         member)))))

(defun leaf-entry-at (tree page slot-index)
  "Read entry at given slot index in a leaf. Returns full entry bytes."
  (let ((offset (page-entry-start page slot-index))
        (len (slot-directory-length page slot-index)))
    (subseq (page-data page) offset (+ offset len))))

;;; Insert into leaf

(defun btree-insert-into-leaf (tree page key-bytes entry-bytes)
  "Insert an entry into a leaf page. Handles splitting if full."
  (let ((entry-len (length entry-bytes))
        (count-before (page-count page)))
    (let ((fit (page-can-fit-p page entry-len)))
      (cond
        ;; Page has room
        (fit
         (let ((slot (btree-find-slot-in-leaf tree page key-bytes)))
           (page-insert-entry-at page slot entry-bytes entry-len)
           (setf (btree-dirty tree) t)
           (values nil nil))) ; no split
        ;; Page is full - need to split
         (t
          (let ((result (multiple-value-list (btree-split-leaf tree page key-bytes entry-bytes))))
            (apply #'values result)))))))

(defun btree-split-leaf (tree page key-bytes entry-bytes)
  "Split a full leaf page. Returns (values new-key-bytes new-page) or NIL if no split needed."
  (let ((new-page (btree-allocate-page tree +page-type-leaf+))
        (count (page-count page)))
    ;; First, insert the new entry into the old page temporarily
    ;; by finding where it would go, then splitting
    (let ((slot (btree-find-slot-in-leaf tree page key-bytes)))
      ;; We'll do a combined approach: collect all entries, add new one, split
      (let ((all-entries nil))
        ;; Collect existing entries
        (loop for i from 0 below count
              do (push (leaf-entry-at tree page i) all-entries))
        ;; Add new entry
        (push entry-bytes all-entries)
        ;; Sort entries by key
        (setf all-entries
              (sort all-entries
                    (lambda (a b)
                      (let ((ka (leaf-entry-key-for-sort tree page a))
                            (kb (leaf-entry-key-for-sort tree page b)))
                        (btree-key-lessp tree ka kb)))))
        ;; Split in half
        (let* ((total (length all-entries))
               (mid (floor total 2)))
          ;; Clear old page and write first half
          (setf (page-count page) 0
                (page-free-space page) +page-usable-size+)
          (loop for entry in (subseq all-entries 0 mid)
                for slot from 0
                do (page-insert-entry-at page slot entry (length entry)))
          ;; Write second half to new page
          (loop for entry in (subseq all-entries mid)
                for slot from 0
                do (page-insert-entry-at new-page slot entry (length entry)))
          ;; Link leaf chain
          (setf (page-next new-page) (page-next page))
          (setf (page-next page) (page-number new-page))
          ;; Return the first key of new page as separator
          (values (leaf-entry-key-for-sort tree new-page (leaf-entry-at tree new-page 0))
                  new-page))))))

(defun leaf-entry-key-for-sort (tree page entry-bytes)
  "Extract key from entry bytes for sorting purposes."
  (let ((data entry-bytes)
        (type (meta-type (btree-meta tree))))
    (ecase type
      (10 ; set
       (multiple-value-bind (status member next)
           (decode-set-entry data 0)
         (declare (ignore status next))
         member))
      (7 ; hash
       (multiple-value-bind (status field value next)
           (decode-hash-entry data 0)
         (declare (ignore status value next))
         field))
      (8 ; zset score-tree
       ;; Key is the score encoded in bytes for comparison
       (let ((score (bytes-to-double data 1)))
         ;; Encode score as key bytes for comparison
         (let ((result (make-array 8 :element-type '(unsigned-byte 8))))
           (double-to-bytes result 0 score)
           result))))))

;;; Insert into B+tree (public API)

(defun btree-insert-entry (tree root-page-num key-bytes entry-bytes)
  "Insert an entry into the B+tree. Returns the new root page number."
  (cond
    ;; Empty tree
    ((= root-page-num 0)
     (let ((page (btree-allocate-page tree +page-type-leaf+)))
       (page-insert-entry-at page 0 entry-bytes (length entry-bytes))
       (page-number page)))
    ;; Non-empty tree
    (t
       (let ((root (btree-read-page tree root-page-num)))
        (let ((fit-p (page-can-fit-p root (length entry-bytes))))
          (if (not fit-p)
             ;; Root is full - need to split or descend
             (if (= (page-type root) +page-type-leaf+)
              ;; Leaf root is full - split it and create new branch root
                  (let ((new-root (btree-allocate-page tree +page-type-branch+)))
                    (multiple-value-bind (sep-key new-page)
                        (btree-insert-into-leaf tree root key-bytes entry-bytes)
                      (assert new-page)
                      (let ((old-root-bytes (encode-branch-entry (page-number root) #()))
                            (new-page-bytes (encode-branch-entry (page-number new-page) sep-key)))
                        (page-insert-entry-at new-root 0 old-root-bytes (length old-root-bytes))
                        (page-insert-entry-at new-root 1 new-page-bytes (length new-page-bytes)))
                      (page-number new-root)))
                 ;; Branch root is full - need to split branch
                 (error "Branch root split not yet implemented"))
             ;; Root has room
             (if (= (page-type root) +page-type-leaf+)
                 ;; Leaf root - insert directly
                 (progn
                   (btree-insert-into-leaf tree root key-bytes entry-bytes)
                   (page-number root))
                 ;; Branch root - descend to correct child
                 (let ((slot (btree-find-slot-in-branch tree root key-bytes)))
                   (let ((child-num (branch-child-at root slot)))
                     (let ((child-new-num (btree-insert-entry tree child-num key-bytes entry-bytes)))
                       ;; Check if child was split
                       (if (and child-new-num (/= child-new-num child-num))
                           ;; Child was split - insert new child pointer into branch
                           (let* ((sep-key (btree-find-lowest-key tree child-new-num))
                                  (new-entry (encode-branch-entry child-new-num sep-key)))
                             (if (page-can-fit-p root (length new-entry))
                                 ;; Branch has room
                                 (progn
                                   (page-insert-entry-at root (1+ slot) new-entry (length new-entry))
                                   (setf (btree-dirty tree) t)
                                   (page-number root))
                                 ;; Branch is full - need to split branch
                                 (error "Branch split not yet implemented")))
                           ;; Child was not split
                           (page-number root))))))))))))

;;; Lookup

(defun btree-lookup-entry (tree root-page-num key-bytes)
  "Look up an entry by key. Returns entry bytes or NIL."
  (when (= root-page-num 0)
    (return-from btree-lookup-entry nil))
  (multiple-value-bind (page slot)
      (btree-find-leaf tree root-page-num key-bytes)
    (when (and page (< slot (page-count page)))
      (let ((entry (leaf-entry-at tree page slot))
            (type (meta-type (btree-meta tree))))
        ;; Verify the key matches
        (let ((entry-key (leaf-entry-key tree page (page-entry-start page slot))))
          (when (btree-key-equalp tree key-bytes entry-key)
            entry))))))

;;; Delete from leaf

(defun btree-delete-from-leaf (tree page key-bytes)
  "Delete an entry from a leaf page. Returns T if found."
  (let ((count (page-count page))
        (found nil))
    (loop for i from 0 below count
          for entry-offset = (page-entry-start page i)
          for entry-key = (leaf-entry-key tree page entry-offset)
          when (btree-key-equalp tree key-bytes entry-key)
            do (setf found t)
               ;; Mark as deleted by shifting entries
               (let ((entry-len (slot-directory-length page i)))
                 (declare (ignore entry-len))
                 ;; Shift entries left
                 (loop for j from i below (1- count)
                       do (let ((src-offset (page-entry-start page (1+ j)))
                                (dst-offset (page-entry-start page j))
                                (src-len (slot-directory-length page (1+ j))))
                            ;; Copy entry data
                            (replace (page-data page) (page-data page)
                                     :start1 dst-offset :start2 src-offset
                                     :end2 (+ src-offset src-len))
                            ;; Update slot directory
                            (slot-directory-set page j dst-offset src-len)))
                 ;; Update slot directory count
                 (decf (page-count page))
                 (setf (page-free-space page)
                       (page-can-fit-free-space page))
                 ;; Mark tree dirty
                 (setf (btree-dirty tree) t)
                 (return)))
    found))

;;; Range scan

(defun btree-range-scan (tree root-page-num min-key-bytes max-key-bytes &optional limit)
  "Scan entries in range [min-key, max-key]. Returns list of entry byte vectors."
  (when (= root-page-num 0)
    (return-from btree-range-scan nil))
  (let ((result nil)
        (count 0))
    (multiple-value-bind (page slot)
        (btree-find-leaf tree root-page-num min-key-bytes)
      (when page
        ;; Scan from the found leaf forward
        (loop named scan-loop
              for current-page = page then (when (> (page-next current-page) 0)
                                             (btree-read-page tree (page-next current-page)))
              while current-page
              do (loop for i from 0 below (page-count current-page)
                       for entry-offset = (page-entry-start current-page i)
                       for entry-key = (leaf-entry-key tree current-page entry-offset)
                       when (and (or (null min-key-bytes)
                                     (btree-key-lessp tree min-key-bytes entry-key)
                                     (btree-key-equalp tree min-key-bytes entry-key))
                                 (or (null max-key-bytes)
                                     (btree-key-lessp tree entry-key max-key-bytes)
                                     (btree-key-equalp tree entry-key max-key-bytes)))
                          do (push (leaf-entry-at tree current-page i) result)
                            (incf count)
                            (when (and limit (>= count limit))
                              (return-from scan-loop (nreverse result)))
                       when (and max-key-bytes
                                 (btree-key-lessp tree max-key-bytes entry-key))
                         do (return-from scan-loop (nreverse result))))))
    (nreverse result)))

;;; Count

(defun btree-count-entries (tree root-page-num)
  "Count total entries in the B+tree."
  (meta-entry-count (btree-meta tree)))

;;; Tree statistics

(defun btree-tree-height (tree root-page-num)
  "Calculate the height of the B+tree."
  (when (= root-page-num 0)
    (return-from btree-tree-height 0))
  (let ((height 0)
        (page (btree-read-page tree root-page-num)))
    (loop while (= (page-type page) +page-type-branch+)
          do (incf height)
             (let ((child-num (branch-child-at page 0)))
               (setf page (btree-read-page tree child-num))))
    (1+ height)))

;;; Serialization to/from file

(defun btree-write-page-to-stream (page stream)
  "Write a page to a binary stream."
  (write-page-header page)
  (write-sequence (page-data page) stream))

(defun btree-read-page-from-stream (stream page-num)
  "Read a page from a binary stream."
  (let ((page (make-btree-page :number page-num)))
    (read-sequence (page-data page) stream)
    (read-page-header page)
    page))

(defun btree-flush-to-file (tree)
  "Write all dirty pages to file."
  (when (btree-file-path tree)
    (with-open-file (stream (btree-file-path tree)
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      ;; Write meta page first (page 0)
      (let ((meta-data (make-array +page-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
        (write-meta (btree-meta tree) meta-data)
        (write-sequence meta-data stream))
      ;; Write all cached pages
      (maphash (lambda (page-num page)
                 (declare (ignore page-num))
                 (let ((page-data (make-array +page-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
                    (write-page-header page)
                    (replace page-data (page-data page))
                    (file-position stream (* (page-number page) +page-size+))
                    (write-sequence page-data stream)))
               (btree-page-cache tree))
      ;; Update meta with correct page count
      (let ((meta-data (make-array +page-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
        (write-meta (btree-meta tree) meta-data)
        (file-position stream 0)
        (write-sequence meta-data stream))
      (setf (btree-dirty tree) nil))))

;;; Iterate all entries

(defun btree-map-entries (tree root-page-num function)
  "Call function with key-bytes and entry-bytes for each entry in order."
  (when (= root-page-num 0)
    (return-from btree-map-entries))
  ;; Find leftmost leaf
  (let ((page (btree-read-page tree root-page-num)))
    (loop while (= (page-type page) +page-type-branch+)
          do (setf page (btree-read-page tree (branch-child-at page 0))))
    ;; Scan leaf chain
    (loop while page
          do (loop for i from 0 below (page-count page)
                   for entry-offset = (page-entry-start page i)
                   for entry-key = (leaf-entry-key tree page entry-offset)
                   for entry = (leaf-entry-at tree page i)
                   do (funcall function entry-key entry))
              (setf page (when (> (page-next page) 0)
                           (btree-read-page tree (page-next page)))))))

