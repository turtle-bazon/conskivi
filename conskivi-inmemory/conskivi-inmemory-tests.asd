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
			((:file "package")
                         (:file "basic-operations"
                          :depends-on ("package"))
                         (:file "key-operations"
                          :depends-on ("package"))
                         (:file "string-operations"
                          :depends-on ("package"))
                         (:file "integer-operations"
                          :depends-on ("package"))
                         (:file "list-operations"
                          :depends-on ("package"))
                         (:file "set-operations"
                          :depends-on ("package"))
                         (:file "sorted-set-operations"
                          :depends-on ("package"))
                         (:file "hash-operations"
                          :depends-on ("package"))
                         (:file "pubsub-operations"
                          :depends-on ("package"))
                         (:file "transaction-operations"
                          :depends-on ("package"))))))