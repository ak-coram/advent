(defun day07 (is-part-two)
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
    (loop
      :for line :in (uiop:read-file-lines #P"./07.txt")
      :for equation
        := (loop :for s :in (uiop:split-string line :separator '(#\: #\Space))
                 :for n := (parse-integer s :junk-allowed t) :when n :collect n)
      :when (possiblep equation)
        :sum (car equation))))
