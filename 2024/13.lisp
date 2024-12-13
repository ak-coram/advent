(ql:quickload :cl-ppcre)

(defun day13 (is-part-two)
  (let* ((digits '(:register (:greedy-repetition 1 nil :digit-class)))
         (scanner (ppcre:create-scanner
                   `(:sequence "Button A: X+" ,digits ", Y+" ,digits #\Newline
                               "Button B: X+" ,digits ", Y+" ,digits #\Newline
                               "Prize: X=" ,digits ", Y=" ,digits #\Newline)))
         (c 10000000000000)
         (token-count 0))
    (labels ((solve (ax ay bx by px py)
               (values (/ (- (* px by) (* bx py)) (- (* ax by) (* bx ay)))
                       (/ (- (* ax py) (* px ay)) (- (* ax by) (* bx ay))))))
      (ppcre:do-register-groups ((#'parse-integer ax ay bx by px py))
          (scanner (uiop:read-file-string #P"./13.txt"))
        (multiple-value-bind (a b) (solve ax ay bx by
                                          (if is-part-two (+ c px) px)
                                          (if is-part-two (+ c py) py))
          (when (and (integerp a) (integerp b))
            (incf token-count (+ (* a 3) b)))))
      token-count)))
