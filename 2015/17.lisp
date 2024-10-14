(ql:quickload :alexandria)

(defun day17 (is-part-two)
  (let ((bins (loop :for line :in (uiop:read-file-lines #P"./17.txt")
                    :collect (parse-integer line))))
    (labels ((count-combinations (length)
               (let ((result 0))
                 (alexandria:map-combinations
                  (lambda (combination)
                    (when (eql (apply #'+ combination) 150)
                      (incf result)))
                  bins
                  :length length)
                 result)))
      (loop :for i :from 1 :upto (length bins)
            :for count := (count-combinations i)
            :if (and is-part-two (plusp count))
              :return count
            :else
              :sum count))))
