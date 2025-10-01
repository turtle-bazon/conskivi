;;; -*- lisp -*-

(defsystem :conskivi-core-tests
  :name "conskivi-core-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Cons Key Value Core tests"
  :depends-on (:conskivi-core
               :fiveam
               :iterate)
  :components ((:module tests
                        :components
			((:file "package")))))
