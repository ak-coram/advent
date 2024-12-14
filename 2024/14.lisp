(ql:quickload '(:cl-ppcre :fset))

(defun day14 (is-part-two)
  (let* ((robots) (width 101) (height 103)
         (qx (floor width 2)) (qy (floor height 2)))
    (dolist (line (uiop:read-file-lines #P"./14.txt"))
      (ppcre:register-groups-bind ((#'parse-integer px py vx vy))
          ("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" line)
        (push (list (cons px py) (cons vx vy)) robots)))
    (labels ((move (robot n)
               (destructuring-bind (p v) robot
                 (let* ((x (car p)) (y (cdr p))
                        (dx (* n (car v))) (dy (* n (cdr v))))
                   (cons (mod (+ x dx) width)
                         (mod (+ y dy) height)))))
             (get-quadrant (p) (let ((x (car p)) (y (cdr p)))
                                 (cond ((and (< x qx) (< y qy)) 1)
                                       ((and (< qx x) (< y qy)) 2)
                                       ((and (< x qx) (< qy y)) 3)
                                       ((and (< qx x) (< qy y)) 4)))))
      (if is-part-two
          (loop :with l := (length robots) :for n :from 1
                :for positions
                  := (mapcar (lambda (robot) (move robot n)) robots)
                :when (eql l (length (remove-duplicates positions
                                                        :test #'equal)))
                  :do (return n))
          (loop :with quadrants := (fset:empty-bag) :and safety-factor := 1
                :for robot :in robots
                :for quadrant := (get-quadrant (move robot 100))
                :when quadrant
                  :do (fset:includef quadrants quadrant)
                :finally (return
                           (fset:do-bag-pairs (_ n quadrants safety-factor)
                             (declare (ignore _))
                             (setf safety-factor (* safety-factor n)))))))))
