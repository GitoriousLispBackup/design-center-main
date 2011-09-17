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

(defgeneric ->imago (color)
  (:documentation "Converts a color to an IMAGO color (the color value produced by #'IMAGO:MAKE-COLOR)."))

(defmethod ->imago ((color cl-colors:rgb))
  (imago:make-color (round (cl-colors:red color))
		    (round (cl-colors:green color))
		    (round (cl-colors:blue color))))

(defmethod ->imago ((color cl-colors:hsv))
  (->imago (cl-colors:->rgb color)))

(defun color-transition (a b)
  "Transition a CL-COLORS:RGB color from ``a'' to ''b'' by changing
the hue of the color on the HSV color space. Return type is CL-COLORS:HSV."
  (let ((hsv-a (cl-colors:->hsv (make-rgb a)))
	(hsv-b (cl-colors:->hsv (make-rgb b))))
    (make-instance 'cl-colors:hsv
		   :hue (cl-colors:hue hsv-b)
		   :saturation (cl-colors:saturation hsv-a)
		   :value (cl-colors:value hsv-a))))

(defun colorize-image (image color)
  (imago:do-image-pixels (image old-color x y)
    (if (> (imago:color-alpha old-color) 0)
	(setf (imago:image-pixel image x y) (->imago (color-transition old-color color))))))

(defun generate-image (image-input-filename image-output-filename &key (red 255) (green 255) (blue 255) (alpha 255))
  (let ((im (imago:read-png image-input-filename)))
    (colorize-image im (imago:make-color red green blue alpha))
    (imago:write-png im image-output-filename)
    image-output-filename))

(defun test-colorize (input-filename output-filename red green blue)
  (generate-image input-filename
		  output-filename
		  :red red :blue blue :green green))

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

(define-easy-handler (set-color :uri "/dc/picture/layer/set") (layer color)
  "Handler for setting the color of a layer that's associated with the given session."
  (start-session))

(define-easy-handler (generate-image :uri "/dc/picture/generate") ()
  "Handler for loading and writing the picture/layers being used by the given session. Saves the image to a random location and outputs the location as a JSON string."
  (start-session)
  (setf (content-type*) "application/json")
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
