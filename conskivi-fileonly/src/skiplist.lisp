;;; -*- lisp -*-

(in-package #:conskivi-fileonly)

;;; Skiplist implementation for sorted set score ordering
;;;
;;; Max level 32, probability 0.25. Provides O(log N) insert/delete/range.

(defconstant +skiplist-max-level+ 32)
(defconstant +skiplist-probability+ 0.25)

(defstruct (skiplist-node (:conc-name snode-))
  key
  value
  (level 0 :type fixnum)
  (forward (make-array (1+ +skiplist-max-level+) :initial-element nil)))

(defstruct (skiplist (:conc-name sl-))
  (header (make-skiplist-node :level +skiplist-max-level+) :type skiplist-node)
  (tail nil)
  (level 0 :type fixnum)
  (count 0 :type integer)
  (test #'< :type function)
  (key-test #'equal :type function))

(defun make-skiplist-with-test (test key-test)
  "Create a skiplist with given comparison tests."
  (make-skiplist :test test :key-test key-test))

(defun random-level ()
  "Generate a random level for a new skiplist node."
  (let ((lvl 0))
    (loop while (and (< lvl (1- +skiplist-max-level+))
                     (< (random 1.0) +skiplist-probability+))
          do (incf lvl))
    lvl))

(defun skiplist-insert (sl key value)
  "Insert key-value pair into skiplist. Returns T if new, NIL if key existed (value updated)."
  (let ((update (make-array (1+ +skiplist-max-level+) :initial-element nil))
        (x (sl-header sl))
        (found nil))
    ;; Find insertion point at each level
    (loop for i from (sl-level sl) downto 0
          do (loop while (and (aref (snode-forward x) i)
                              (funcall (sl-test sl) (snode-key (aref (snode-forward x) i)) key))
                   do (setf x (aref (snode-forward x) i)))
             (setf (aref update i) x))
    ;; Check if key already exists
    (setf x (aref (snode-forward x) 0))
    (when (and x (funcall (sl-key-test sl) (snode-key x) key))
      (setf found t)
      (setf (snode-value x) value))
    (unless found
      ;; Create new node
      (let ((lvl (random-level)))
        (when (> lvl (sl-level sl))
          (loop for i from (1+ (sl-level sl)) to lvl
                do (setf (aref update i) (sl-header sl)))
          (setf (sl-level sl) lvl))
        (let ((new-node (make-skiplist-node :key key :value value :level lvl)))
          ;; Link at each level
          (loop for i from 0 to lvl
                 do (setf (aref (snode-forward new-node) i) (aref (snode-forward (aref update i)) i))
                   (setf (aref (snode-forward (aref update i)) i) new-node))
          (incf (sl-count sl)))))
    (not found)))

(defun skiplist-delete (sl key)
  "Delete entry by key. Returns T if found and deleted."
  (let ((update (make-array (1+ +skiplist-max-level+) :initial-element nil))
        (x (sl-header sl))
        (found nil))
    ;; Find the node to delete at each level
    (loop for i from (sl-level sl) downto 0
          do (loop while (and (aref (snode-forward x) i)
                              (funcall (sl-test sl) (snode-key (aref (snode-forward x) i)) key))
                   do (setf x (aref (snode-forward x) i)))
             (setf (aref update i) x))
    (setf x (aref (snode-forward x) 0))
    (when (and x (funcall (sl-key-test sl) (snode-key x) key))
      (setf found t)
      ;; Unlink from each level
      (loop for i from 0 to (sl-level sl)
            do (when (eq (aref (snode-forward (aref update i)) i) x)
                  (setf (aref (snode-forward (aref update i)) i) (aref (snode-forward x) i))))
      ;; Decrease level if top levels are empty
      (loop while (and (> (sl-level sl) 0)
                       (null (aref (snode-forward (sl-header sl)) (sl-level sl))))
            do (decf (sl-level sl)))
      (decf (sl-count sl)))
    found))

(defun skiplist-search (sl key)
  "Search for key in skiplist. Returns value or NIL."
  (let ((x (sl-header sl)))
    (loop for i from (sl-level sl) downto 0
          do (loop while (and (aref (snode-forward x) i)
                              (funcall (sl-test sl) (snode-key (aref (snode-forward x) i)) key))
                   do (setf x (aref (snode-forward x) i))))
    (setf x (aref (snode-forward x) 0))
    (when (and x (funcall (sl-key-test sl) (snode-key x) key))
      (snode-value x))))

(defun skiplist-range (sl min-key max-key &optional limit)
  "Return list of (key . value) pairs where min-key <= key <= max-key.
   If limit is provided, return at most that many results."
  (let ((result nil)
        (count 0))
    (let ((x (sl-header sl)))
      ;; Find starting point
      (loop for i from (sl-level sl) downto 0
            do (loop while (and (aref (snode-forward x) i)
                                (funcall (sl-test sl) (snode-key (aref (snode-forward x) i)) min-key))
                     do (setf x (aref (snode-forward x) i))))
      ;; Collect results
      (setf x (aref (snode-forward x) 0))
      (loop while (and x
                       (or (null max-key)
                           (funcall (sl-test sl) (snode-key x) max-key)
                           (funcall (sl-key-test sl) (snode-key x) max-key))
                       (or (null limit) (< count limit)))
            do (push (cons (snode-key x) (snode-value x)) result)
               (incf count)
               (setf x (aref (snode-forward x) 0))))
    (nreverse result)))

(defun skiplist-range-from (sl min-key &optional limit)
  "Return list of (key . value) pairs where key >= min-key."
  (skiplist-range sl min-key nil limit))

(defun skiplist-first (sl)
  "Return (key . value) of the first (smallest) entry, or NIL."
  (let ((x (aref (snode-forward (sl-header sl)) 0)))
    (when x
      (cons (snode-key x) (snode-value x)))))

(defun skiplist-last (sl)
  "Return (key . value) of the last (largest) entry, or NIL."
  (let ((x (sl-header sl)))
    (loop for i from (sl-level sl) downto 0
          do (loop while (aref (snode-forward x) i)
                   do (setf x (aref (snode-forward x) i))))
    (when (not (eq x (sl-header sl)))
      (cons (snode-key x) (snode-value x)))))

(defun skiplist-count (sl)
  "Return number of entries."
  (sl-count sl))

(defun skiplist-map (sl function)
  "Call function with (key . value) for each entry in order."
  (let ((x (aref (snode-forward (sl-header sl)) 0)))
    (loop while x
          do (funcall function (snode-key x) (snode-value x))
             (setf x (aref (snode-forward x) 0)))))

;;; Sorted-set specific skiplist helpers
;;;
;;; Score key is (score . member-bytes). Comparison:
;;; 1. Compare scores numerically
;;; 2. For equal scores, compare member bytes lexicographically

(defun make-zset-key (score member-bytes)
  "Create a composite sort key for sorted set."
  (cons score member-bytes))

(defun zset-key-compare (a b)
  "Compare two zset sort keys."
  (cond
    ((< (car a) (car b)) :less)
    ((> (car a) (car b)) :greater)
    (t (key-bytes-compare (cdr a) (cdr b)))))

(defun zset-key-lessp (a b)
  (eq (zset-key-compare a b) :less))

(defun zset-key-equalp (a b)
  (eq (zset-key-compare a b) :equal))

(defun make-zset-skiplist ()
  "Create a skiplist for sorted set score ordering."
  (make-skiplist :test #'zset-key-lessp :key-test #'zset-key-equalp))
