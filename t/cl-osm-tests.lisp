(uiop:define-package cl-osm/tests
  (:use :cl :parachute)
  (:import-from :assoc-utils
                #:aget
                #:alist))

(in-package :cl-osm/tests)

(define-test "Load a node from osm"
  (let ((node (osm:node 1)))

    (of-type 'alist node)
    (of-type 'alist (osm:with-element node))
    (is string= "node" (osm:with-element node :type))
    (is string= "communication" (osm:with-element node :tags :|TOWER:TYPE|))
    )
  )


(test :cl-osm/tests)
