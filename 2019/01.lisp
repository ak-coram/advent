
(defun day01 (is-part-two)
  (labels ((fuel (m) (- (floor m 3) 2))
           (total-fuel (m) (loop :for f := (fuel m) :then (fuel f)
                                 :while (plusp f) :sum f)))
    (loop :for line :in (uiop:read-file-lines #P"./01.txt")
          :for m := (parse-integer line)
          :sum (if is-part-two (total-fuel m) (fuel m)))))
