
(defun day04 (is-part-two)
  (let ((range (mapcar #'parse-integer
                       (uiop:split-string (uiop:read-file-line #P"./04.txt")
                                          :separator '(#\-)))))
    (labels ((digits (i)
               (loop :with result := nil
                     :for n := i :then next-n
                     :while (plusp n)
                     :for (next-n d) := (multiple-value-list (floor n 10))
                     :do (push d result)
                     :finally (return result)))
             (fitp (guess)
               (loop :with counts := nil :and count := 1
                     :for (d . (next)) :on (digits guess)
                     :when (and next (> d next))
                       :return nil
                     :if (eql d next)
                       :do (incf count)
                     :else
                       :do (push count counts) :and :do (setf count 1)
                     :finally (return counts))))
      (loop :for guess :from (car range) :upto (cadr range)
            :count (if is-part-two
                       (member 2 (fitp guess))
                       (find-if (lambda (n) (<= 2 n)) (fitp guess)))))))
