(defpackage %3b-libusb1
  (:use :cffi :cl)
  (:shadow #:open #:close)
  (:export
   #:init
   #:exit
   #:set-debug
   #:get-version
   #:get-device-list
   #:free-device-list
   #:with-device-list
   #:get-device-descriptor
   #:open
   #:close
   #:claim-interface
   #:release-interface
   #:bulk-transfer
   #:device*
   #:device-handle*
   #:device-descriptor
   #:version
   #:context
   #:kernel-driver-active
   #:detach-kernel-driver
   #:attach-kernel-driver
   #:clear-halt
   #:reset-device))


(defpackage 3b-libusb1
  (:use :cffi :cl)
  (:import-from #:%3b-libusb1
                #:claim-interface
                #:release-interface
   )
  (:export
   #:init
   #:exit
   #:set-debug
   #:with-context
   #:*context*
   #:get-version
   #:usb-open
   #:usb-close
   #:claim-interface
   #:release-interface
   #:init-default
   #:exit-default
   #:get-device-descriptor
   #:map-device-list
   #:bulk-transfer-out
   #:bulk-transfer-in
   #:command-block-wrapper
   #:send-command
   #:csw-status))
