(ql:quickload :cl-ppcre)

(defun day18 (is-part-two)
  (loop :with moves
          := (loop :for line :in (uiop:read-file-lines #P"./18.txt")
                   :collect (ppcre:register-groups-bind (d1 n1 n2 d2)
                                ("^(.) (\\d+) \\(#(.....)(.)\\)" line)
                              (if is-part-two
                                  (list (case (parse-integer d2 :radix 16)
                                          (0 #\R)
                                          (1 #\D)
                                          (2 #\L)
                                          (3 #\U))
                                        (parse-integer n2 :radix 16))
                                  (list (char d1 0) (parse-integer n1)))))
        :and current := '(0 0)
        :for (d n) :in moves :for (dx dy) := (case d
                                               (#\L `(,(- n) 0))
                                               (#\R `(,n 0))
                                               (#\U `(0 ,(- n)))
                                               (#\D `(0 ,n)))
        :for (x1 y1) := current
        :for x2 := (+ x1 dx) :for y2 := (+ y1 dy)
        :sum (* (+ y1 y2) (- x1 x2)) :into sum :sum n :into total-moves
        :do (setf current (list x2 y2))
        :finally (return (+ (/ (abs sum) 2) (1+ (/ total-moves 2))))))
