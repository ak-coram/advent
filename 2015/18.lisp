(defun day18 (is-part-two)
  (labels
      ((make-grid () (make-array '(100 100) :element-type 'bit
                                            :initial-element 0))
       (get-neighbours (origin)
         (let ((x (car origin)) (y (cdr origin)))
           (list (cons (1- x) (1- y)) (cons (1- x) y) (cons (1- x) (1+ y))
                 (cons x (1- y)) (cons x (1+ y))
                 (cons (1+ x) (1- y)) (cons (1+ x) y) (cons (1+ x) (1+ y)))))
       (count-active (grid &optional positions)
         (if (eql positions :all)
             (loop :for x :below 100 :sum (loop :for y :below 100
                                                :sum (aref grid x y)))
             (loop :for (x . y) :in positions
                   :when (and (<= 0 x 99) (<= 0 y 99))
                     :sum (aref grid x y))))
       (animate (grid)
         (loop :with next-grid := (make-grid)
               :for x :below 100
               :do (loop :for y :below 100
                         :for prev-state := (aref grid x y)
                         :for n := (count-active grid
                                                 (get-neighbours (cons x y)))
                         :do (if (plusp prev-state)
                                 (when (<= 2 n 3)
                                   (setf (aref next-grid x y) 1))
                                 (when (eql n 3)
                                   (setf (aref next-grid x y) 1))))
               :finally (return next-grid)))
       (activate-corners (grid)
         (setf (aref grid 0 0) 1
               (aref grid 0 99) 1
               (aref grid 99 0) 1
               (aref grid 99 99) 1)))
    (loop :with initial-grid
            := (loop :with lights := (make-grid)
                     :for line :in (uiop:read-file-lines #P"./18.txt")
                     :for x :from 0
                     :do (loop :for light :across line
                               :for y :from 0
                               :when (char= light #\#)
                                 :do (setf (aref lights x y) 1))
                     :finally (return lights))
          :for i :upto 100
          :for grid := initial-grid :then (animate grid)
          :when is-part-two
            :do (activate-corners grid)
          :finally (return (count-active grid :all)))))
