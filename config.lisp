;; Design Center Configuration File

;; This file is loaded by the START-SERVER function in the
;; DESIGN-CENTER package. It is loaded using the LOAD function so you
;; can use whatever Lisp you want here.

;; You can set/get or use any functions available in the DESIGN-CENTER
;; package.

(in-package :design-center)
(setf *http-port* 7888)
(setf *swank-enabled* t)
(setf *picture-load-path* (truename "./pictures"))
(format t "Swank starts on port: ~A~%" *swank-port*)

;; Add pictures
(add-picture "Kitchen Test" #p"kitchen-test" "This is a test image.")
