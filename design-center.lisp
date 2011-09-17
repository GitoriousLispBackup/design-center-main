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
(ql:quickload '("opticl"
		"cl-colors"
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
	:opticl
	:cl-colors
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
  (id (random 123091238))
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
(defvar *generated-image-directory*
  "/tmp"
  "The directory where generated images are stored.")

(defvar *generated-image-url*
  "/dc/generated-images/"
  "The URL where generated images are located.")

  "Registers a picture as available for designing. Stores the picture
in *PICTURES*"
  (pushnew (make-picture :title title
			 :description description
			 :path (truename (merge-pathnames *picture-load-path* path))
			 :images-loaded nil)
	   *pictures*
	   :test #'picture-path))

(defun load-picture (picture)
  "Loads the base image, and layer images for the picture. The base
image is replaced with a list where the first element is the path and
the second element is the image data.

For each layer in the layers list, the loaded image data of the layer
is appended to that layer."
  (with-slots (base-image layers images-loaded) picture
    (setf base-image (cons base-image (read-png base-image)))
    (setf images-loaded t)
    picture))

(defun load-pictures ()
  "Loads the base images, and layer images for all registered pictures."
  (loop for p in *pictures*
     when (not (picture-images-loaded p))
     do (setf (picture-base-image p) (read-image (picture-path p)))
       (setf (picture-layers p)
	     (loop for layer in (picture-layers p)
		collect (list layer (read-image (merge-pathnames *picture-load-path* layer)))))))

(defun make-rgb (c)
  (make-instance 'cl-colors:rgb
		 :red (imago:color-red c)
		 :green (imago:color-green c)
		 :blue (imago:color-blue c)))

(defgeneric ->opticl (color)
  (:documentation "Converts a color to an OPTICL color (the color values consumed by OPTICL."))

(defmethod ->opticl ((color rgba))
  (values (round (red color))
	  (round (green color))
	  (round (blue color))
	  (round (alpha color))))

(defmethod ->opticl ((color hsv))
  (->opticl (->rgb color)))

(defun color-transition (a b)
  "Transition a CL-COLORS:RGB color from ``a'' to ''b'' by changing
the hue of the color on the HSV color space. Return type is CL-COLORS:HSV."
  (let ((hsv-a (->hsv a))
	(hsv-b (->hsv b)))
    (make-instance 'hsv
		   :hue (hue hsv-b)
		   :saturation (saturation hsv-a)
		   :value (value hsv-a))))

(defun colorize-image (image color)
  (with-image-bounds (height width) image
    (loop for i below height
       do (loop for j below width
	     do (setf (pixel image i j) (->opticl (color-transition (pixel image i j) color)))))))

(defun generate-image (image-input-filename image-output-filename &key (red 255) (green 255) (blue 255) (alpha 255))
  (let ((im (read-png-file image-input-filename)))
    (colorize-image im (make-instance 'rgba :red red :green green :blue blue :alpha alpha))
    (write-png-file im image-output-filename)
    image-output-filename))

;; Request handlers (a.k.a. views)
(define-easy-handler (new-design :uri "/dc/test") ()
  (start-session)
  (setf (session-value 'picture) nil)
  (setf (session-value 'layer-colors) (list))
  (setf (content-type*) "text/html")
  (format nil "hello world: ~a" (session-value 'id)))

(defun generate-test-page ()
  (setf (content-type*) "text/html")
  (format nil "hello world"))

(defmacro define-ajax (name vars uri documentation &body body)
  `(define-easy-handler (,name :uri ,uri) ,vars
     ,(concatenate 'string "AJAX handler. " documentation)
     (start-session)
     (setf (content-type*) "application/json")
     ,@body))

(define-ajax change-picture (pic)
  "/dc/choose"
  "Change which picture (and layers) are associated with the given session."
  (setf (session-value 'picture) nil))

(define-ajax pictures-list ()
  "/dc/picture/list"
  "Lists all pictures available for selection."
  nil)

(define-ajax picture-info ()
  "/dc/picture/info"
  "Returns a hash-table of picture information."
  (encode-json-plist-to-string '(:title "Title" :description "Description")))

(define-ajax picture-layers ()
  "/dc/picture/layer/list"
  "List all " ;; TODO: finishing converting define-easy-handler to define-ajax
  (encode-json-alist-to-string '(("walls" . "Kitchen Walls"))))

(define-ajax set-color (layer color)
  "/dc/picture/layer/set"
  "Set the color of a layer that's associated with the given session."
  nil)

(define-ajax generate-image ()
  "/dc/picture/generate"
  "Loads and writes the picture/layers being used by the given
session. Saves the image to a random location and outputs the location
as a JSON string."
  (encode-json-to-string (concatenate 'string *generated-image-url* (session-value 'whatever)))
  ;(generate-image (session-value
)

(define-easy-handler (thumbnail :uri "/dc/picture/thumbnail") (picture-id height)
  "Handler that loads and resizes the base image of a picture."
  (setf (content-type*) "image/png")
  (let* ((im (elt *pictures* picture-id))
	 (ratio (/ height (image-height im))))
    (with-output-to-string (stream)
      (write-png (resize im (round (* ratio (image-width im))) height) stream))))

(defun start-server (&optional (config-file "config.lisp"))
  (load config-file :verbose t)
  (if *swank-enabled*
      (swank:create-server :port *swank-port* :dont-close t))
  ;; change this to whatever session secret you like
  (setf hunchentoot:*session-secret* "{Rn2*%ZFw/'K+}-)7z@qW7mvb62S+h")
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *http-port*)))
