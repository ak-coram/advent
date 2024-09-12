(defun day11 (is-part-two)
  (let ((expansion-factor (1- (if is-part-two 1000000 2)))
        (lines (uiop:read-file-lines #P"./11.txt"))
        (galaxy-positions nil)
        (empty-row-indices nil)
        (empty-column-indices nil))
    (loop :for line :in lines :for y :from 0
          :do (loop :with row-has-galaxy := nil
                    :for c :across line :for x :from 0
                    :do (when (char= c #\#)
                          (push (list x y) galaxy-positions)
                          (setf row-has-galaxy t))
                    :finally (unless row-has-galaxy
                               (push y empty-row-indices))))
    (loop :for x :from 0 :below (length (car lines))
          :unless (loop :for line :in lines
                        :when (char= (aref line x) #\#) :return t)
            :do (push x empty-column-indices))
    (labels ((adjust-for-expansion (position)
               (mapcar (lambda (n empty-indices)
                         (+ n (* expansion-factor
                                 (count-if (lambda (i) (< i n))
                                           empty-indices))))
                       position
                       (list empty-column-indices empty-row-indices))))
      (loop :with expanded-positions := (mapcar #'adjust-for-expansion
                                                galaxy-positions)
            :for (x1 y1) :in expanded-positions
            :sum (loop :for (x2 y2) :in expanded-positions
                       :sum (+ (abs (- x1 x2)) (abs (- y1 y2))))
              :into total-distance
            :finally (return (/ total-distance 2))))))
