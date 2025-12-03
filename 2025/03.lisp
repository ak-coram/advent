(defun day03 (is-part-two)
  (labels ((recur (bank n)
             (if (zerop n)
                 0
                 (loop :with max := (cons 0 nil) :and next-n := (1- n)
                       :for i :from 0 :upto (- (length bank) n)
                       :for x :on bank
                       :when (< (car max) (car x))
                         :do (setf max x)
                       :finally (return (+ (* (car max) (expt 10 next-n))
                                           (recur (cdr max) next-n)))))))
    (loop :for line :in (uiop:read-file-lines #P"03.txt")
          :for bank := (map 'list #'digit-char-p line)
          :sum (recur bank (if is-part-two 12 2)))))
