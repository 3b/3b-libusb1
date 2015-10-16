#++ (asdf:load-systems 'cffi 'nibbles)
(in-package #:%3b-libusb1)

#++
(defctype ssize-t #.(ecase (foreign-type-size :pointer)
                      (4 ':int32)
                      (8 ':int64)))
(defcenum errors
  (:success 0)
  (:io -1)
  (:invalid-param -2)
  (:access -3)
  (:no-device -4)
  (:not-found -5)
  (:busy -6)
  (:timeout -7)
  (:overflow -8)
  (:pipe -9)
  (:interrupted -10)
  (:no-mem -11)
  (:not-supported -12)
  (:other -99))

(define-foreign-type checked-result ()
  ()
  (:actual-type :int)
  (:simple-parser checked-result))

(defmethod translate-from-foreign (v (type checked-result))
  ;; todo: expand-from-foreign/-dyn
  ;; todo: add option to ignore or continue from some errors?
  (if (zerop v) ;; optimize common case
      :success
      (let ((enum (foreign-enum-keyword 'errors v :errorp nil)))
        (when (< v 0)
          (error "libusb error ~s (~s)" v (or enum "??")))
        (or enum v))))

(defcstruct version
  (:major :uint16)
  (:minor :uint16)
  (:micro :uint16)
  (:nano :uint16)
  (:rc :string)
  (:describe :string))

(defctype context :pointer)

(defcfun ("libusb_set_debug" set-debug) :void
  (context context)
  (level :int))

(defcfun ("libusb_init" init) checked-result
  (context (:pointer context)))


(defcfun ("libusb_exit" exit) :void
  (context context))

(defcfun ("libusb_get_version" get-version) (:pointer (:struct version)))

(defctype device* :pointer)
(defctype device-handle* :pointer)

(defcenum speed
  (:unknown 0)
  (:low 1)
  (:full 2)
  (:high 3)
  (:super 4))


(defcfun ("libusb_get_device_list" get-device-list) checked-result
  (context context)
  (list (:pointer (:pointer device*))))

(defcfun ("libusb_free_device_list" free-device-list) :void
  (list (:pointer device*))
  (unref :int))

(defmacro with-device-list ((list-var &key (context *context*)) &body body)
  `(cffi:with-foreign-object (,list-var :pointer)
     (get-device-list ,context ,list-var)
     (unwind-protect
          (progn ,@body)
       (free-device-list))))

(defcenum (descriptor-type :uint8)
  (:device 1)
  (:config 2)
  (:string 3)
  (:interface 4)
  (:endpoint 5)
  (:hid #x21)
  (:report #x22)
  (:physical #x23)
  (:hub #x29))

(defcenum (class-code :uint8)
  (:per-interface 0)
  (:audio 1)
  (:communication-and-cdc-control 2)
  (:hid 3)
  (:physical 5)
  (:image 6)
  (:printer 7)
  (:mass-storage 8)
  (:hub 9)
  (:cdc-data #x0a)
  (:smart-card #x0b)
  (:content-security #x0d)
  (:video #x0e)
  (:personal-healthcare #x0f)
  (:audio/video #x10)
  (:billboard #x11)
  (:diagnostic-device #xdc)
  (:wireless #xe0)
  (:miscellaneous #xef)
  (:application-specific #xfe)
  (:vendor-specific #xff))

(defcstruct device-descriptor
  (:length :uint8)
  (:descriptor-type  descriptor-type)
  (:bcd-usb :uint16);; todo: translate this?
  (:device-class class-code)
  (:device-sub-class :uint8)
  (:device-protocol :uint8)
  (:max-packet-size-0 :uint8)
  (:vendor-id :uint16)
  (:product-id :uint16)
  (:bcd-device :uint16)
  (:manufacturer :uint8)
  (:product :uint8)
  (:serial-number :uint8)
  (:num-configurations :uint8))

(defcfun ("libusb_get_device_descriptor" get-device-descriptor) checked-result
  (device device*)
  (descriptor (:pointer (:struct device-descriptor))))

;; should these shadow cl:open/cl:close instead of having prefix?
(defcfun ("libusb_open" open) checked-result
  (device device*)
  (handle (:pointer device-handle*)))

(defcfun ("libusb_close" close) :void
  (handle device-handle*))

(defcfun ("libusb_clear_halt" clear-halt) checked-result
  (dev device-handle*)
  (endpoint :uint8))

(defcfun ("libusb_reset_device" reset-device) checked-result
  (dev device-handle*))


(defcfun ("libusb_claim_interface" claim-interface) checked-result
  (handle device-handle*)
  (interface-number :int))

(defcfun ("libusb_release_interface" release-interface) checked-result
  (handle device-handle*)
  (interface-number :int))


(defcfun ("libusb_bulk_transfer" bulk-transfer) checked-result
  (handle device-handle*)
  (endpoint :uint8)
  (data (:pointer :uint8))
  (length :int)
  (transferred (:pointer :int))
  (timeout :uint))



(defcfun ("libusb_kernel_driver_active" kernel-driver-active) checked-result
  (dev device-handle*)
  (interface :int))

(defcfun ("libusb_detach_kernel_driver" detach-kernel-driver) checked-result
  (dev device-handle*)
  (interface :int))

(defcfun ("libusb_attach_kernel_driver" attach-kernel-driver) checked-result
  (dev device-handle*)
  (interface :int))
