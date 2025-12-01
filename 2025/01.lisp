(defun day01 (is-part-two)
  (loop :with rotations := (uiop:read-file-lines #P"./01.txt")
        :with position := 50 :for rotation :in rotations
        :for direction := (aref rotation 0)
        :for amount := (parse-integer rotation :start 1)
        :for next-position := (mod (ecase direction
                                     (#\L (- position amount))
                                     (#\R (+ position amount)))
                                   100)
        :sum (if is-part-two
                 (ceiling (- amount (cond
                                      ((zerop position) 99)
                                      ((eql direction #\L) (1- position))
                                      ((eql direction #\R) (- 99 position))))
                          100)
                 (if (zerop next-position) 1 0))
        :do (setf position next-position)))
