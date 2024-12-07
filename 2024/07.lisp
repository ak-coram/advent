(defun day07 (is-part-two)
  (let ((equations 
          (loop :for line :in (uiop:read-file-lines #P"./07.txt")
                :for (test-input numbers-input)
                  := (uiop:split-string line :separator '(#\:))
                :collect
                (cons (parse-integer test-input)
                      (loop :for (x offset)
                              := (multiple-value-list
                                  (parse-integer numbers-input
                                                 :start (or offset 0)
                                                 :junk-allowed t))
                            :while x :collect x)))))
    (labels ((|| (x y) (+ (* (expt 10 (floor (1+ (log y 10)))) x) y))
             (possiblep (equation)
               (loop :with (test-value . numbers) := equation
                     :for x :in numbers
                     :for accs := (list x)
                       :then (loop :for acc :in accs
                                   :collect (* acc x) :collect (+ acc x)
                                   :when is-part-two
                                     :collect (|| acc x))
                     :finally (return (member test-value accs)))))
      (loop :for equation :in equations
            :when (possiblep equation)
              :sum (car equation)))))
