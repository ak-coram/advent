(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day09 (is-part-two)
  (labels ((extrapolate (xs)
             (+ (alexandria:lastcar xs)
                (loop :with current := xs
                      :for deltas := (loop :for (x y) :on current
                                           :when y :collect (- y x))
                      :until (every #'zerop deltas)
                      :do (setf current deltas)
                      :sum (alexandria:lastcar deltas)))))
    (let ((input (mapcar (lambda (l)
                           (mapcar #'parse-integer (ppcre:split " " l)))
                         (uiop:read-file-lines "./09.txt"))))
      (loop :for xs :in input
            :sum (extrapolate (if is-part-two
                                  (reverse xs)
                                  xs))))))
