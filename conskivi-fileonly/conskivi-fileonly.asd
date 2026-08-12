;;; -*- lisp -*-

(defsystem :conskivi-fileonly
  :name "conskivi-fileonly"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "conskivi key-value database - file-only backend"
  :depends-on (conskivi-core
               bordeaux-threads
               cl-ppcre
               flexi-streams
               ieee-floats)
  :components ((:module src
                :components
                ((:file "package")
                 (:file "serialization"
                  :depends-on ("package"))
                 (:file "file-ops"
                  :depends-on ("package" "serialization"))
                 (:file "conskivi-fileonly"
                  :depends-on ("package" "serialization" "file-ops")))))
  :in-order-to ((test-op (test-op conskivi-fileonly-tests)))
  :perform (test-op :after (op c)
             (funcall
              (intern (symbol-name '#:conskivi-run-all-tests)
                      :conskivi-fileonly-tests))))
