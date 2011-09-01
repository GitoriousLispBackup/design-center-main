;; Copyright (C) 2011 Rudolf Olah <rolah@goaugust.com>
(require 'quicklisp)
(ql:quickload '("imago"
		"hunchentoot"
		"html-template"
		"swank"))

(defpackage :design-center
  (:documentation "Design Center is a web application for painting a
picture of a room with new colours. Each picture is separated into
layers which can be colourized and the user can select new pictures,
set the colours of the layers, and see the resulting image.")
  (:use :common-lisp
	:imago
	:hunchentoot
	:html-template)
  (:export :start-server))
(in-package :design-center)

;; customize variables for HTML-TEMPLATE
(setq *template-start-marker* "{%"
      *template-end-marker* "%}"
      *default-template-pathname* #p"./templates/")

(defparameter *http-port* 4242
  "The port that Hunchentoot will be listening on.")

(defparameter *swank-enabled* nil
  "Whether or not remote interaction with SLIME is allowed.")

(defparameter *swank-port* 4444
  "The port used for remote interaction with SLIME.")

(defstruct site
  "A design center site has a base url where it is hosted, and a name.
``pictures'' is a list of pictures available for designing.
``picture-load-path'' is the path in which to search for pictures
available for designing."
  (base-url "http://localhost/" :type string)
  (name "Site" :type string)
  (pictures (list) :type (or nil list))
  (picture-load-path #p"./pictures/" :type pathname))

(defvar *sites*
  (list)
  "List of design center sites. Each site may have its own domain name and pictures. Each site shares any pictures that are placed into *PICTURES* (which are loaded from *PICTURE-LOAD-PATH*). Sessions are shared between sites.")

(defstruct picture
  thumbnail
  title
  description
  path
  (layers (list))
  base-image
  images-loaded)

(defvar *pictures*
  (list)
  "List of pictures available for designing on all sites.")

(defvar *picture-load-path*
  #p"./pictures/"
  "The path in which to search for pictures available for designing.")

(defun register-picture (title path &optional (description ""))
  "Registers a picture as available for designing. Stores the picture
in *PICTURES*"
  (setf *pictures* (cons (make-picture :title title
				       :description description
				       :path path
				       :images-load nil)
			 *pictures*)))

(defun load-pictures (site)
  "Loads the thumbnails, base image, and layer images for all registered pictures."
  (loop for p in (site-pictures site)
       when (not (picture-images-loaded p))
       do (setf (picture-base-image p) (read-image (picture-path p)))
       (setf (picture-layers p)
	     (loop for layer in (picture-layers p)
		collect (list layer (read-image (merge-pathnames (site-picture-load-path site)
								 layer)))))))

(defstruct site-session
  (started-on (get-universal-time))
  ip-address
  picture
  layer-colors)

(defun get-session (site id)
  (assoc id (site-sessions site)))

(defun start-session (site ip-address picture)
  (let ((id (random 300000)))
    (setf (site-sessions site)
	  (cons (list id (make-site-session :ip-address ip-address :picture picture))
		(site-sessions site)))
    id))

(defun cleanup-sessions (site)
  (setf (site-sessions site) (remove-if (lambda (x)
					  (> (- (get-universal-time) (session-started-on x))
					     (site-session-cleanup-time site)))
					(site-sessions site))))

(defun set-layer-color (session layer-index new-color)
  (setf (elt (session-layer-colors session) layer-index) new-color))

(defun generate-image (session)
  ;; run-program
  )

;; Request handlers (a.k.a. views)
(define-easy-handler (start-session :uri "/new") (pic)
  (setf (content-type*) "text/html")
  (fill-and-print-template #p"designer.html"))

(define-easy-handler (change-picture :uri "/choose") (session pic)
  "Handler for changing which picture (and layers) are associated with the given session."
  )

(define-easy-handler (set-color :uri "/set") (session layer color)
  "Handler for setting the color of a layer that's associated with the given session."
  )

(define-easy-handler (session-image :uri "/image") (session)
  "Handler for loading and writing the picture/layers being used by the given session."
  (setf (content-type*) "image/png")
  )

(define-easy-handler (thumbnail :uri "/thumbnail") (picture-id)
  "Handler that loads and resizes the base image of a picture."
  )

(defun start-server (&optional (config-file "config.lisp"))
  (load config-file :verbose t)
  (if *swank-enabled*
      (swank:create-server :port *swank-port* :dont-close t))
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *http-port*)))
