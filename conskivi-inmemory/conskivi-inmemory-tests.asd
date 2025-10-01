;;; -*- lisp -*-

(defsystem :conskivi-inmemory-tests
  :name "conskivi-inmemory-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Cons Key Value InMemory tests"
  :depends-on (:conskivi-inmemory
               :fiveam
               :iterate)
  :components ((:module tests
                        :components
			((:file "package")))))
