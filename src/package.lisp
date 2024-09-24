(uiop:define-package #:cl-osm
  (:documentation "A Common Lisp library for working with OpenStreetMap data.")
  (:use #:cl)
  (:nicknames #:osm)
  (:export
   #:*osm-api-version*
   #:*osm-api-base-url*
   #:api-versions
   #:api-capabilities
   #:map-bbox
   #:node
   #:way
   #:relation
   #:node-history
   #:way-history
   #:relation-history
   #:node-version
   #:way-version
   #:relation-version
   #:node-relations
   #:way-relations
   #:relation-relations
   #:node-ways
   #:way-full
   #:relation-full
   #:nodes
   #:ways
   #:relations
   #:api-permissions
   #:permissions))
