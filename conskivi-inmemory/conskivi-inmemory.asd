;;;; -*- mode: lisp -*-

(defsystem :conskivi-inmemory
  :name "conskivi-inmemory"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Cons Key Value InMemory"
  :depends-on (bordeaux-threads
               conskivi-core
               iterate
               metabang-bind)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conskivi-inmemory"
                  :depends-on ("package")))))
  :in-order-to ((test-op (test-op conskivi-inmemory-tests)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern (symbol-name '#:conskivi-inmemory-tests)
                             :conskivi-inmemory-tests))))
