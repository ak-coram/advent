(ql:quickload '(:cl-ppcre :fset))

(defun day15 ()
  (destructuring-bind (grid-input move-input)
      (ppcre:split "\\n\\n" (uiop:read-file-string #P"./15.txt"))
    (let ((initial-grid (fset:empty-map)) (initial-position)
          (moves (loop :for c :across move-input
                       :for d := (case c
                                   (#\< :left) (#\> :right)
                                   (#\^ :up) (#\v :down))
                       :when d :collect d)))
      (loop
        :for line :in (ppcre:split "\\n" grid-input)
        :for x :from 0
        :do (loop :for c :across line :for y :from 0
                  :for p := (cons x y)
                  :when (char= c #\@)
                    :do (setf initial-position p)
                  :do (fset:includef initial-grid p (case c
                                                      (#\@ :robot)
                                                      (#\. :space)
                                                      (#\O :box)
                                                      (#\# :wall)))))
      (labels ((get-next-position (position direction)
                 (let ((x (car position)) (y (cdr position)))
                   (case direction
                     (:left (cons x (1- y)))
                     (:right (cons x (1+ y)))
                     (:up (cons (1- x) y))
                     (:down (cons (1+ x) y)))))
               (step-state (grid position direction)
                 (let* ((thing (fset:lookup grid position))
                        (next (get-next-position position direction))
                        (next-thing (fset:lookup grid next)))
                   (case next-thing
                     (:space (values (fset:with (fset:with grid next thing)
                                                position :space)
                                     next))
                     (:box (multiple-value-bind (next-grid moved-position)
                               (step-state grid next direction)
                             (if (equal moved-position next)
                                 (values grid position)
                                 (step-state next-grid position direction))))
                     (:wall (values grid position))))))
        (loop :with robot := initial-position :and grid := initial-grid
              :and result := 0
              :for direction :in moves
              :for (next-grid next-robot)
                := (multiple-value-list (step-state grid robot direction))
              :do (setf grid next-grid robot next-robot)
              :finally (return (fset:do-map (position thing grid result)
                                 (when (eql thing :box)
                                   (setf result (+ result
                                                   (* (car position) 100)
                                                   (cdr position)))))))))))

