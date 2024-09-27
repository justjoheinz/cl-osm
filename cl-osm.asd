(asdf:defsystem cl-osm
  :author "Markus Klink"
  :description "Simple API support for OpenStreetMap. Currently only supports read requests."
  :version "0.6.0"
  :depends-on (:dexador
               :ciao
               :assoc-utils
               :cl-json)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :in-order-to ((test-op (test-op :cl-osm/tests))))

(asdf:defsystem cl-osm/tests
  :depends-on (:cl-osm
               :parachute)
  :serial t
  :components ((:module "t"
                :components ((:file "cl-osm-tests"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :cl-osm/tests)))
