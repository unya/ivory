(in-package #:ivory-analyzer)

(defmacro defsysbyte (name length position)
  `(define-symbol-macro ,name (list byte ,length ,position)))

(eval-always
  (defmacro defenumerated (list-name code-list &optional (start 0) (increment 1) end)
    `(progn
       (defconstant ,list-name ',code-list)
       ,@(loop for code in code-list and prev = 0 then code
	             as value from start by increment
	             unless (eq code prev)		;kludge for data-types
	               collect `(defconstant ,code ,value)))))


