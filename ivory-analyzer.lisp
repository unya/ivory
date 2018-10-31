;;;; ivory-analyzer.lisp

(in-package #:ivory-analyzer)


(defun read-into-block (stream block &optional read-size)
  "read file into memory block"
  (let ((read-size (if read-size read-size (1-  (slot-value block 'size)))))
    (with-slots (contents size) block
      (handler-case 
          (loop :for offset :from 0
                  :to read-size
                :do (setf (aref contents offset)
                          (read-word stream)))
        (end-of-file () nil)))))

(defun read-word (stream)
  (let ((temp (make-array '(5) :element-type '(unsigned-byte 8))))
    (read-sequence temp stream)
    (byte-array-to-word temp)))

(defun write-word (word stream)
  (write-sequence (word-to-byte-array word) stream))

(defun byte-array-to-word (array)
  (cl-intbytes:octets->uint array 5))

(defun word-to-byte-array (word)
  (cl-intbytes:int->octets word 5))

