(in-package #:3b-libusb1)

(defparameter *context* (cffi:null-pointer))

(defconstant +endpoint-in+ #x80)
(defconstant +endpoint-out+ #x00)


(defun set-debug (level &key (context *context*))
  (%3b-libusb1:set-debug context level))

(defun init-default ()
  (%3b-libusb1:init (cffi:null-pointer)))

(defun init ()
  (cffi:with-foreign-object (p '%3b-libusb1:context)
    (%3b-libusb1:init p)
    (cffi:mem-ref p '%3b-libusb1:context)))


(defun exit-default ()
  (%3b-libusb1:exit (cffi:null-pointer)))

(defun exit (context)
  (%3b-libusb1:exit (or context (cffi:null-pointer))))

(defmacro with-context ((&key (var '*context*) (debug nil)) &body body)
  `(let* ((,var (init)))
     ,@(when debug
         `((set-debug ,debug :context ,var)))
     (unwind-protect
          (progn ,@body)
       (exit ,var))))

(defun get-version ()
  (mem-ref (%3b-libusb1:get-version) '(:struct %3b-libusb1:version)))


(defun get-device-descriptor (device*)
  (with-foreign-object (desc '(:struct %3b-libusb1:device-descriptor))
    (loop for i below (foreign-type-size '(:struct %3b-libusb1:device-descriptor))
          do (setf (mem-aref desc :uint8 i)i))
    (%3b-libusb1:get-device-descriptor device* desc)
    (mem-ref desc '(:struct %3b-libusb1:device-descriptor))))

(defun map-device-list (thunk &key (context *context*) vendor product)
  "Call THUNK with all devices (optionally limited to specified VENDOR
and PRODUCT ids, collecting non-NIL results. Thunk is passed a DEVICE*
and a plist containing a device descriptor, the DEVICE* is only valid
for the duration of the call to MAP-DEVICE-LIST, but can be used to
open a device and return a DEVICE-HANDLE."
  (cffi:with-foreign-object (plist :pointer)
    (let* ((count (%3b-libusb1:get-device-list context plist))
           (list (cffi:mem-ref plist :pointer)))
      (unwind-protect
           (when (and (numberp count)
                      (> count 0))
             (loop for i below count
                   for dev = (mem-aref list :pointer i)
                   while (not (null-pointer-p dev))
                   when (let ((descriptor (get-device-descriptor dev)))
                          (block nil
                            (when (and vendor
                                       (not (= vendor
                                               (getf descriptor :vendor-id))))
                              (return nil))
                            (when (and product
                                       (not (= product
                                               (getf descriptor :product-id))))
                              (return nil))
                            (funcall thunk dev descriptor)))
                     collect it))
        (%3b-libusb1:free-device-list list 1)))))


(defun usb-open (device)
  (with-foreign-object (p '(:pointer %3b-libusb1:device-handle*))
    (%3b-libusb1:open device p)
    (mem-ref p '%3b-libusb1:device-handle*)))

(defun usb-close (device)
  (%3b-libusb1:close device))

(defun bulk-transfer-out (device-handle endpoint data
                          &key (timeout 0) count)
  ;; todo: add start, possibly rename COUNT to END to match start?
  "Send DATA to ENDPOINT on open DEVICE-HANDLE. Optionally timeout
after TIMEOUT seconds. Returns number of octets transferred if
successful.  If COUNT is provided, it specifies number of octets to
send from DATA, which can be a foreign pointer. If COUNT is NIL, DATA
must be a CL sequence, and entire sequence will be sent."
  (flet ((send (buf octets)
           (assert (not (logtest +endpoint-in+ endpoint)))
           (with-foreign-object (tx :int)
             (%3b-libusb1:bulk-transfer device-handle endpoint buf octets tx
                             (floor (or timeout 0) 1/1000))
             (mem-ref tx :int))))
    (if (pointerp data)
        (progn
          ;; must supply count if passing a foreign pointer directly
          (assert (numberp count))
          (send data count))
        (let ((count (or count (length data))))
          (with-foreign-object (b :uint8 count)
            (when count (assert (<= count (length data))))
            (typecase data
              ((simple-array (unsigned-byte 8) (*))
               ;; let compiler generate faster code for common case
               (loop for i below count
                     do (setf (mem-aref b :uint8 i) (aref data i))))

              ;; slower fallback code
              (cons
               (loop for i below count
                     for x in data
                     do (setf (mem-aref b :uint8 i) x)))
              (t
               (loop for i below count
                     do (setf (mem-aref b :uint8 i) (aref data i)))))
            (send b count))))))


(defun bulk-transfer-in (device-handle endpoint count &key timeout)
  ;; todo: allos providing a vector or foreign buffer for storing results?
  "Read up to COUNT octets from ENDPOINT on DEVICE-HANDLE, returning
results in a ub8 vector. Waits at most TIMEOUT seconds (unlimited if 0
or NIL)."
  (assert (logtest +endpoint-in+ endpoint))
  (with-foreign-objects ((buf :uint8 count)
                         (tx :int))
    (%3b-libusb1:bulk-transfer device-handle endpoint buf count tx
                               (floor (or timeout 0) 1/1000))
    (let* ((tx (mem-ref tx :int))
           (dest (make-array tx :element-type '(unsigned-byte 8)
                                :initial-element 0)))
      (loop for i below tx
            do (setf (aref dest i) (mem-aref buf :uint8 i)))
      dest)))
