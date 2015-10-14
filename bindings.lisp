(in-package #:3b-libusb1)

(defparameter *context* (cffi:null-pointer))

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

(defcfun ("libusb_set_debug" %set-debug) :void
  (context context)
  (level :int))

(defun set-debug (level &key (context *context*))
  (%set-debug context level))


(defcfun ("libusb_init" %init) checked-result
  (context (:pointer context)))

(defun init-default ()
  (%init (cffi:null-pointer)))

(defun init ()
  (cffi:with-foreign-object (p 'context)
    (%init p)
    (cffi:mem-ref p 'context)))

(defcfun ("libusb_exit" %exit) :void
  (context context))

(defun exit-default ()
  (%exit (cffi:null-pointer)))

(defun exit (context)
  (%exit (or context (cffi:null-pointer))))

(defmacro with-context ((&key (var '*context*) (debug nil)) &body body)
  `(let* ((,var (init)))
     ,@(when debug
         `((set-debug ,debug :context ,var)))
     (unwind-protect
          (progn ,@body)
       (exit ,var))))

(defcfun ("libusb_get_version" %get-version) (:pointer (:struct version)))

(defun get-version ()
  (mem-ref (%get-version) '(:struct version)))

(defctype device :pointer)
(defctype device-handle :pointer)

(defcenum speed
  (:unknown 0)
  (:low 1)
  (:full 2)
  (:high 3)
  (:super 4))




#++
(with-context (:debug 3)
  (format t "context = ~s~%" *context*)
  (format t "version = ~s~%" (get-version))
)
