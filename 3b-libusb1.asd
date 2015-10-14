(defsystem 3b-libusb1
  :description "Common Lisp (cffi) bindings to libusb 1.0"
  :author "Bart Botta <00003b@gmail.com>"
  :license "MIT"
  :depends-on (cffi)
  :serial t
  :components ((:file "package")
               (:file "library")
               (:file "bindings")
               (:file "hidapi")))
