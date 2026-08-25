;;; -*- lisp -*-

(defsystem :conskivi-fileonly
  :name "conskivi-fileonly"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "GNU General Public License v3"
  :version "0.0.1.0"
  :description "conskivi key-value database - file-only backend"
  :depends-on (conskivi-core
               bordeaux-threads
               cl-ppcre
               flexi-streams
               ieee-floats
               cffi)
  :components ((:module src
                :components
                ((:file "package")
                 (:file "serialization"
                  :depends-on ("package"))
                 (:file "file-ops"
                  :depends-on ("package" "serialization"))
                 (:file "btree-page"
                  :depends-on ("package" "serialization"))
                 (:file "skiplist"
                  :depends-on ("package"))
                 (:file "btree-tree"
                  :depends-on ("package" "btree-page" "serialization"))
                  (:file "btree-wal"
                   :depends-on ("package" "btree-page"))
                  (:file "btree-index"
                   :depends-on ("package" "btree-page" "btree-tree" "skiplist" "serialization"))
                   (:file "conskivi-fileonly"
                   :depends-on ("package" "serialization" "file-ops"
                                "btree-page" "btree-tree" "btree-index" "skiplist")))))
  :in-order-to ((test-op (test-op conskivi-fileonly-tests)))
  :perform (test-op :after (op c)
             (funcall
              (intern (symbol-name '#:conskivi-run-all-tests)
                      :conskivi-fileonly-tests))))
