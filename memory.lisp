(in-package #:ivory-analyzer)

(defclass memory-block ()
  ((base-address :initarg :base-address)
   (size :initarg :size)
   (contents)))

(defclass prom-block (memory-block)
  ()
  (:default-initargs
   :size 262144
   :base-address #o37000000000))

(defmethod initialize-instance :after ((block memory-block) &key size base-address)
  (setf (slot-value block 'size) size)
  (setf (slot-value block 'base-address) base-address)
  (setf (slot-value block 'contents)
        (make-array (list size) :element-type 'ivory-word :initial-element #Xdeadbeef42)))

(defun vma=pma (address)
  (logior #o37000000000 address))
