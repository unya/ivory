(in-package #:ivory-analyzer)

(deftype ivory-word () '(unsigned-byte 40))

(defun word->pointer (word)
  (ldb (byte 32 0) word))

(defun %clamp-word (word)
  (deposit-field 0 (byte 24 40) word))

(defun tag (word)
  (ldb (byte 6 32) word))

(defun cdr-code (word)
  (ldb (byte 2 6) (tag word)))

(defun data-type (word)
  (ldb (byte 6 0) (tag word)))

(defun value (word)
  (ldb (byte 32 0) word))

(defenumerated *datatypes*         ;codes are written in octal
    (dtp-null                           ;00 Unbound var/function, unitialized storage
     dtp-monitor-forward                ;01 Cell is monitored
     dtp-header-p                       ;02 structure header with pointer
     dtp-header-i                       ;03 structure header with immediate bits
     dtp-external-value-cell-pointer    ;04 "invisible" used in bindings?
     dtp-one-q-forward                  ;05 "invisible" forwarding pointer - one cell
     dtp-header-forward                 ;06 "invisible" forwarding pointer - whole structure
     dtp-element-forward                ;07 "invisible" forwarding pointer - element in structure
     dtp-fixnum                         ;10 32bit fixnum
     dtp-small-ratio                    ;11 ratio with small numerator *and* denominator
     dtp-single-float                   ;12 single 32bit IEEE float immediate
     dtp-double-float                   ;13 double precision 64bit IEEE float
     dtp-bignum                         ;14 pointer to bignum
     dtp-big-ratio                      ;15 pointer to big rational
     dtp-complex                        ;16 pointer to complex number
     dtp-spare-number                   ;17 number used by hw trap
     dtp-instance                       ;20 pointer to instance
     dtp-list-instance                  ;21 pointer to instance acting as list
     dtp-array-instance                 ;22 pointer to instance acting as array
     dtp-string-instance                ;23 pointer to instance acting as string
     dtp-nil                            ;24 NIL
     dtp-list                           ;25 CONS cell
     dtp-array                          ;26 non-string array pointer
     dtp-string                         ;27 string pointer
     dtp-symbol                         ;30 symbol pointer other than NIL
     dtp-locative                       ;31 locative pointer
     dtp-lexical-closure                ;32
     dtp-dynamic-closure                ;33
     dtp-compiled-function              ;34 compiled code
     dtp-generic-function               ;35 generic function
     dtp-spare-pointer-1                ;36 spare
     dtp-spare-pointer-2                ;37 spare
     dtp-physical-address               ;40 physical address
     dtp-spare-immediate-1              ;41 spare - can contain Alpha instructions in random VLM stuff ???
     dtp-bound-location                 ;42 deep bound marker (TODO: figure wtf that is)
     dtp-character                      ;43 common lisp character
     dtp-logic-variable                 ;44 unbound logic variable (used by prolog?)
     dtp-gc-forward                     ;45 object moved flag for GC
     dtp-even-pc                        ;46 pointer to first instruction in a word
     dtp-odd-pc                         ;47 pointer to second instruction in a word
     dtp-call-compiled-even             ;50 start call, address is DTP-COMPILED-FUNCTION, start at even word
     dtp-call-compiled-odd              ;51 start call, address is DTP-COMPILED-FUNCTION, start at even word
     dtp-call-indirect                  ;52 start call, address is function cell of a symbol
     dtp-call-generic                   ;53 start call, address is DTP-GENERIC-FUNCTION
     dtp-call-compiled-even-prefetch    ;54 Prefetch variants of the above
     dtp-call-compiled-odd-prefetch     ;55
     dtp-call-indirect-prefetch         ;56
     dtp-call-generic-prefetch          ;57
     dtp-packed-instruction-60 dtp-packed-instruction-61 dtp-packed-instruction-62
     dtp-packed-instruction-63 dtp-packed-instruction-64 dtp-packed-instruction-65
     dtp-packed-instruction-66 dtp-packed-instruction-67 dtp-packed-instruction-70
     dtp-packed-instruction-71 dtp-packed-instruction-72 dtp-packed-instruction-73
     dtp-packed-instruction-74 dtp-packed-instruction-75 dtp-packed-instruction-76
     dtp-packed-instruction-77))

(defun tag-to-string (tag)
  (or (find tag *DATATYPES* :test (lambda (tag dtp) (= (symbol-value dtp) tag))) "BAD"))

(defenumerated *cdr-codes*
    (cdr-next                           ;00 CDR is in next word
     cdr-nil                            ;01 CDR is nil
     cdr-normal                         ;02 next memory word contains address of CDR
     cdr-illegal                        ;03 illegal
     ))

(defun cdr-code-to-string (cdr)
  (case cdr
    (cdr-next 'cdr-next)
    (cdr-nil 'cdr-nil)
    (cdr-normal 'cdr-normal)
    (cdr-illegal 'cdr-illegal)))

(defun print-word (word)
  (let ((cdr-code (cdr-code word))
        (dtp (data-type word))
        (immediate (value word)))
    (format t "#< ~S|~S|~S >~%"
            (cdr-code-to-string cdr-code)
            (tag-to-string dtp)
            immediate)))
