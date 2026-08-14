;;;; -*- mode: lisp -*-

(defsystem :conskivi-core
  :name "conskivi-core"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "GNU General Public License v3"
  :version "0.0.1.0"
  :description "Cons Key Value Core"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "api"
                  :depends-on ("package")))))
  :in-order-to ((test-op (test-op conskivi-core-tests)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern (symbol-name '#:conskivi-core-tests)
                             :conskivi-core-tests))))
