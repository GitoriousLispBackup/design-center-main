;; This file is part of Design Center.
;;
;; Design Center is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Design Center is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with Design Center.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Copyright (C) 2011 Rudolf Olah <rolah@goaugust.com>

(require 'quicklisp)
(ql:quickload '("imago"
		"hunchentoot"
		"md5"
		"swank"))

(defpackage :design-center
  (:documentation "Design Center is a web application for painting a
picture of a room with new colours. Each picture is separated into
layers which can be colourized and the user can select new pictures,
set the colours of the layers, and see the resulting image.")
  (:use :common-lisp
	:imago
	:hunchentoot)
  (:export :start-server))
(in-package :design-center)

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
  (picture-load-path (truename #p"./pictures/") :type pathname))

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
  (truename #p"./pictures/")
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

(defun set-layer-color (session layer-index new-color)
  (setf (elt (session-layer-colors session) layer-index) new-color))

(defun generate-image (session)
  ;; run-program
  )

;; Request handlers (a.k.a. views)
(define-easy-handler (new-design :uri "/") ()
  (start-session)
  (setf (session-value 'picture) nil)
  (setf (session-value 'layer-colors) (list))
  (setf (content-type*) "text/html"))

(define-easy-handler (test-design-center :uri "/test") ()
  (output-template #p"embedded-example.html" nil))

(define-easy-handler (change-picture :uri "/choose") (pic)
  "Handler for changing which picture (and layers) are associated with the given session."
  (start-session))

(define-easy-handler (set-color :uri "/set") (layer color)
  "Handler for setting the color of a layer that's associated with the given session."
  (start-session))

(define-easy-handler (generate-image :uri "/image") ()
  "Handler for loading and writing the picture/layers being used by the given session."
  (start-session)
  (setf (content-type*) "image/png")
)

(define-easy-handler (thumbnail :uri "/thumbnail") (picture-id)
  "Handler that loads and resizes the base image of a picture."
  )

(defun start-server (&optional (config-file "config.lisp"))
  (load config-file :verbose t)
  (if *swank-enabled*
      (swank:create-server :port *swank-port* :dont-close t))
  (reset-session-secret)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *http-port*)))
