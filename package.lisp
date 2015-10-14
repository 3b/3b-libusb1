(defpackage %3b-libusb1
  (:use :cffi :cl)
  (:export
   #:init
   #:exit))


(defpackage 3b-libusb1
  (:use :cffi :cl)
  (:export
   #:init
   #:exit
   #:set-debug
   #:with-context
   #:*context*
   #:get-version))
