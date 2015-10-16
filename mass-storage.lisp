(in-package #:3b-libusb1)

(defconstant +cbw-size+ 31)
(defconstant +csw-size+ 13)

(defparameter *tag* 0)

(defun command-block-wrapper (length data &key (direction :in) (lun 0))
  (let ((buf (make-array +cbw-size+ :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
    (map-into buf 'char-code "USBC")
    (setf *tag* (ldb (byte 32 0) (1+ *tag*)))
    (setf (nibbles:ub32ref/le buf 4) *tag*)
    (setf (nibbles:ub32ref/le buf 8) length)
    (setf (aref buf 12) (ecase direction (:in #x80) (:out #x00)))
    (assert (<= 0 lun #xf))
    (setf (aref buf 13) lun)
    (assert (<= (length data) #x1f))
    (setf (aref buf 14) (max 1 (length data)))
    (replace buf data :start1 15)
    buf))

(defun command-status-wrapper (tag data-residue status)
  (let ((buf (make-array +csw-size+ :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
    (map-into buf 'char-code "USBS")
    (setf (nibbles:ub32ref/le buf 4) tag)
    (setf (nibbles:ub32ref/le buf 8) data-residue)
    (assert (<= 0 status 2))
    (setf (aref buf 12) status)
    buf))

(defun decode-cbw (buf)
  (assert (= (length buf) +cbw-size+))
  (assert (string= (map 'string 'code-char (subseq buf 0 4)) "USBC")
          nil "invalid signature in command block wrapper ~s? (expected 85 83 66 67 ...)" buf)
  (list :lun (aref buf 13)
        :length (nibbles:ub32ref/le buf 8)
        :flags (ecase (aref buf 12) (#x00 :out) (#x80 :in))
        :data (subseq buf 14)
        :tag (nibbles:ub32ref/le buf 4)))

(defun decode-csw (buf)
  (assert (= (length buf) +csw-size+))
  (assert (string= (map 'string 'code-char (subseq buf 0 4)) "USBS")
          nil "invalid signature in command status wrapper ~s? (expected 85 83 66 83 ...)" buf)
  (list :status (aref buf 12)
        :residue (nibbles:ub32ref/le buf 8)
        :tag (nibbles:ub32ref/le buf 4)))

(defun cbw-transfer-length (buf)
  (nibbles:ub32ref/le buf 8))

(defun (setf cbw-transfer-length) (new buf)
  (setf (nibbles:ub32ref/le buf 8) new))

(defun csw-status (buf)
  ;; 0 = passed/good, 1=failed, 2=phase error, 3/4=obsolete, 5+ reserved
  (aref buf 12))

(defun send-command (device-handle endpoint command-block-wrapper)
  (let ((in (logior endpoint +endpoint-in+))
        (ret nil))
    ;; send command
    (bulk-transfer-out device-handle endpoint command-block-wrapper
                       :timeout 2)
    ;; read response if one is expected
    (when (plusp (cbw-transfer-length command-block-wrapper))
      (setf ret (bulk-transfer-in device-handle in
                                  (cbw-transfer-length command-block-wrapper)
                                  :timeout 2)))

    ;; check status
    (let ((csw (bulk-transfer-in device-handle in 13 :timeout 2)))
      (assert (= (length csw) 13))
      ;; not sure which of these is more important or if both are
      ;; usually needed? possibly should be (values ret csw) instead?
      (list (decode-csw csw) ret))))
