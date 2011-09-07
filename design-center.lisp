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
		"cl-json"
		"swank"))

(defpackage :design-center
  (:documentation "Design Center is a web application for painting a
picture of a room with new colours. Each picture is separated into
layers which can be colourized and the user can select new pictures,
set the colours of the layers, and see the resulting image.")
  (:use :common-lisp
	:imago
	:json
	:hunchentoot)
  (:export :start-server))
(in-package :design-center)

(defparameter *http-port* 4242
  "The port that Hunchentoot will be listening on.")

(defparameter *swank-enabled* nil
  "Whether or not remote interaction with SLIME is allowed.")

(defparameter *swank-port* 4444
  "The port used for remote interaction with SLIME.")

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

(defun add-picture (title path &optional (description ""))
  "Registers a picture as available for designing. Stores the picture
in *PICTURES*"
  (pushnew (make-picture :title title
			 :description description
			 :path (truename (merge-pathnames *picture-load-path* path))
			 :images-loaded nil)
	   *pictures*
	   :test #'picture-path))

(defun load-pictures (site)
  "Loads the thumbnails, base image, and layer images for all registered pictures."
  (loop for p in (site-pictures site)
       when (not (picture-images-loaded p))
       do (setf (picture-base-image p) (read-image (picture-path p)))
       (setf (picture-layers p)
	     (loop for layer in (picture-layers p)
		collect (list layer (read-image (merge-pathnames (site-picture-load-path site)
								 layer)))))))

(defun color-transition (a b amount)
  "Transition an IMAGO:COLOR from ``a'' to ''b'' by the specified ``amount''."
  (flet ((transition (x y) (round (max 0 (min 255 (* amount (- y x)))))))
    (imago:make-color (transition (imago:color-red a) (imago:color-red b))
		      (transition (imago:color-green a) (imago:color-green b))
		      (transition (imago:color-blue a) (imago:color-blue b)))))

(defun colorize-image (image color amount)
  (imago:do-image-pixels (image old-color x y)
    (if (> (imago:color-alpha old-color) 0)
	(setf (imago:image-pixel image x y)
	      (color-transition old-color color amount)))))

(defun generate-image (image-input-filename image-output-filename &key (amount 1.0) (red 255) (green 255) (blue 255) (alpha 255))
  (let ((im (imago:read-png image-input-filename)))
    (colorize-image im (imago:make-color red green blue alpha) amount)
    (imago:write-png im image-output-filename)
    image-output-filename))

(defun test-colorize (input-filename output-filename-fmt red green blue)
  (loop for x in '(0.0 0.1 0.3 0.5 0.8 1.0 1.3 1.5 1.8 2.0)
     do (generate-image input-filename
			(format nil output-filename-fmt x)
			:red red :blue blue :green green :amount x)))

;; Request handlers (a.k.a. views)
(define-easy-handler (new-design :uri "/dc/test") ()
  (start-session)
  (setf (session-value 'picture) nil)
  (setf (session-value 'layer-colors) (list))
  (setf (content-type*) "text/html")
  (format nil "hello world"))

(defun generate-test-page ()
  (setf (content-type*) "text/html")
  (format nil "hello world"))

(define-easy-handler (change-picture :uri "/dc/choose") (pic)
  "Handler for changing which picture (and layers) are associated with the given session."
  (start-session))

(define-easy-handler (pictures-list :uri "/dc/picture/list") ()
  "Handler for listing pictures in JSON form."
  (start-session)
  (setf (content-type*) "application/json"))

(define-easy-handler (picture-info :uri "/dc/picture/info") ()
  "Handler for returning picture information in JSON form."
  (start-session)
  (setf (content-type*) "application/json")
  (encode-json-plist-to-string '(:title "Title" :description "Description")))

(define-easy-handler (picture-layers :uri "/dc/picture/layer/list") ()
  "Handler for returning picture layers information in JSON form."
  (start-session)
  (setf (content-type*) "application/json")
  (encode-json-alist-to-string '(("walls" . "Kitchen Walls"))))

(define-easy-handler (set-color :uri "/dc/picture/layer/set") (layer color)
  "Handler for setting the color of a layer that's associated with the given session."
  (start-session))

(define-easy-handler (generate-image :uri "/dc/picture/generate") ()
  "Handler for loading and writing the picture/layers being used by the given session. Saves the image to a random location and outputs the location as a JSON string."
  (start-session)
  (setf (content-type*) "application/json")
  ;(generate-image (session-value
)

(define-easy-handler (thumbnail :uri "/dc/picture/thumbnail") (picture-id)
  "Handler that loads and resizes the base image of a picture."
  )

(defun start-server (&optional (config-file "config.lisp"))
  (load config-file :verbose t)
  (if *swank-enabled*
      (swank:create-server :port *swank-port* :dont-close t))
  (reset-session-secret)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *http-port*)))
