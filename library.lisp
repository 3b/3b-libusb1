(in-package #:%3b-libusb1)

(define-foreign-library libusb
  (:unix (:or "libusb-1.0.so"
              "libusb-1.0.so.0")))

(use-foreign-library libusb)


