(in-package #:cl-osm)

(defvar *osm-api-version* "0.6"
  "The supported verion of the OpenStreetMap API")
(defvar *osm-api-base-url* "https://api.openstreetmap.org/api"
  "The base URL for the OpenStreetMap API")
(defvar *osm-dev-base-url* "https://master.apis.dev.openstreetmap.org")


(defun alist-get (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (loop :for key :in keys
        :do (if (typep key (and 'fixnum 'unsigned-byte))
                (setq alist (nth key alist))
                (setq alist (aget alist key))))
  alist)

(define-condition osm-error (error)
  ((status :initarg :status :reader osm-error-status)
   (message :initarg :message :reader osm-error-message)
   (url :initarg :url :reader osm-error-url))
  (:report (lambda (condition stream)
             (with-slots (status message url) condition
               (format stream "OpenStreetMap API error:~& ~A ~A~&The requested url was:~&~A" status message url)))))

(defun osm/get (url &key (oauth-obj *oauth-obj*))
  (let* ((base-url (format nil "~A/~A" *osm-api-base-url* *osm-api-version*))
         (full-url (concatenate 'string base-url url))
         (headers (append '(("accept" . "application/json"))
                          (when *oauth-obj*
                            (ciao:headers *oauth-obj*)))))
    (multiple-value-bind (stream status)
        (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
          (dex:get full-url :want-stream t :headers headers))
      (cond ((= status 200)
             (json:decode-json stream))
            ((= status 404)
             (error 'osm-error :status 404
                               :message "Not Found"
                               :url full-url))
            ((= status 401)
             (error 'osm-error :status 401
                               :message "Unauthorized"
                               :url full-url))
            ((= status 403)
             (error 'osm-error :status 403
                               :message "Forbidden"
                               :url full-url))
            (t (error 'osm-error :status status
                                 :message "Unknown error"
                                 :url full-url))))))

(defun api-versions ()
  "Get the supported versions of the OpenStreetMap API."
  (let ((*osm-api-version* ""))
    (osm/get (format nil "versions"))))

(defun api-capabilities ()
  "Get the capabilities of the OpenStreetMap API."
  (osm/get (format nil "/capabilities")))

(defun permissions ()
  "Get the permissions of the logged-in user."
  (osm/get (format nil "/permissions")))

(defun map-bbox (left bottom right top)
  "Get map data for a bounding box."
  (osm/get (format nil "/map?bbox=~A,~A,~A,~A" left bottom right top)))

;; GET /api/0.6/[node|way|relation]/#id

(defun node (id)
  "Details of a node."
  (osm/get (format nil "/node/~A" id)))

(defun way (id)
  "Details of a way."
  (osm/get (format nil "/way/~A" id)))

(defun relation (id)
  "Details of a relation."
  (osm/get (format nil "/relation/~A" id)))

;; GET /api/0.6/[node|way|relation]/#id/history

(defun node-history (id)
  "History of a node."
  (osm/get (format nil "/node/~A/history" id)))

(defun way-history (id)
  "History of a way."
  (osm/get (format nil "/way/~A/history" id)))

(defun relation-history (id)
  "History of a relation."
  (osm/get (format nil "/relation/~A/history" id)))

;; GET /api/0.6/[node|way|relation]/#id/#version

(defun node-version (id version)
  "Details of a node at a specific version."
  (osm/get (format nil "/node/~A/~A" id version)))

(defun way-version (id version)
  "Details of a way at a specific version."
  (osm/get (format nil "/way/~A/~A" id version)))

(defun relation-version (id version)
  "Details of a relation at a specific version."
  (osm/get (format nil "/relation/~A/~A" id version)))

;; Relations for element: GET /api/0.6/[node|way|relation]/#id/relations

(defun node-relations (id)
  "Relations that include a node."
  (osm/get (format nil "/node/~A/relations" id)))

(defun way-relations (id)
  "Relations that include a way."
  (osm/get (format nil "/way/~A/relations" id)))

(defun relation-relations (id)
  "Relations that include a relation."
  (osm/get (format nil "/relation/~A/relations" id)))

;; Ways for node: GET /api/0.6/node/#id/ways

(defun node-ways (id)
  "Ways that include a node."
  (osm/get (format nil "/node/~A/ways" id)))

;; Full: GET /api/0.6/[way|relation]/#id/full

(defun way-full (id)
  "Full details of a way."
  (osm/get (format nil "/way/~A/full" id)))

(defun relation-full (id)
  "Full details of a relation."
  (osm/get (format nil "/relation/~A/full" id)))

;; Multi fetch: GET /api/0.6/[nodes|ways|relations]?#parameters

(defun nodes (ids)
  "Details of multiple nodes."
  (osm/get (format nil "/nodes?nodes=~{~A~^,~}" ids)))

(defun ways (ids)
  "Details of multiple ways."
  (osm/get (format nil "/ways?ways=~{~A~^,~}" ids)))

(defun relations (ids)
  "Details of multiple relations."
  (osm/get (format nil "/relations?relations=~{~A~^,~}" ids)))

;; Changesets: GET /api/0.6/changeset/#id

(defun changeset (id &key (include-discussion nil))
  "Details of a changeset. If INCLUDE-DISCUSSION is true, include discussion comments."
  (if include-discussion
      (osm/get (format nil "/changeset/~A?include_discussion=true" id))
      (osm/get (format nil "/changeset/~A" id))))

;; Methods for user data

(defun user (id)
  "Details of a user."
  (osm/get (format nil "/user/~A" id)))

(defun users (ids)
  "Details of multiple users."
  (osm/get (format nil "/users?users=~{~A~^,~}" ids)))

(defun user-details ()
  "Details of the logged-in user."
  (osm/get "/user/details"))

(defun preferences ()
  "Preferences of the logged-in user."
  (osm/get "/preferences"))

;; create a parameter *oauth-ojb* and assign the return value
(defparameter *oauth-obj* nil)
(defparameter *osm-oauth-client-id* (uiop:getenv "OSM_OAUTH_CLIENT_ID"))
(defparameter *osm-oauth-secret* (uiop:getenv "OSM_OAUTH_SECRET"))

(defparameter *osm-auth-server* (make-instance 'ciao:oauth2-auth-server
                                               :auth-url "https://www.openstreetmap.org/oauth2/authorize"
                                               :token-url "https://www.openstreetmap.org/oauth2/token"))

;; assign the value of oauth2/request-auth-code/browser to *oauth-obj*
(defun get-auth-code (scopes)
  "Open the default browser to obatin the OAuth2 token.

Requires environment variables OSM_OAUTH_CLIENT_ID and OSM_OAUTH_SECRET to be set.
The scopes argument is a list of strings as per the OpenStreetMap API documentation."
  (setf *oauth-obj*
        (ciao:oauth2/request-auth-code/browser
         *osm-auth-server*
         (make-instance 'ciao:oauth2-client :secret *osm-oauth-secret*
                                            :id     *osm-oauth-client-id*)
         :scopes scopes)))

(defun with-element (osm-reply &rest args)
  (apply #'alist-get osm-reply (append '(:elements 0) args)))
