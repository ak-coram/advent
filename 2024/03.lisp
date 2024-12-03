(ql:quickload :cl-ppcre)

(defun day03 (is-part-two)
  (let* ((input (uiop:read-file-string #P"./03.txt"))
         (digits '(:greedy-repetition 1 nil :digit-class))
         (scanner (ppcre:create-scanner
                   `(:alternation (:sequence (:register "mul")
                                             #\( (:register ,digits)
                                             #\, (:register ,digits)
                                             #\))
                                  (:register "do()")
                                  (:register "don't()"))
                   :multi-line-mode t))
         (sum 0)
         (is-disabled nil))
    (ppcre:do-register-groups (mul (#'parse-integer x y) enable disable)
        (scanner input)
      (cond
        (enable (setf is-disabled nil))
        (disable (setf is-disabled t))
        (mul (unless (and is-part-two is-disabled)
               (incf sum (* x y))))))
    sum))

