(ql:quickload :fset)

(defun day06 (is-part-two)
  (let ((original-grid (fset:empty-map))
        (original-start)
        (original-direction '(-1 . 0)))
    (loop :for line :in (uiop:read-file-lines #P"./06.txt")
          :for x :from 0
          :do (loop :for c :across line :for y :from 0
                    :for pos := (cons x y)
                    :when (char= c #\^)
                      :do (setf original-start pos)
                    :do (setf (fset:lookup original-grid pos)
                              (char= c #\#))))
    (labels
        ((register-visit (visits position direction)
           (fset:with visits position (cons direction
                                            (fset:lookup visits position))))
         (in-loop-p (visits position direction)
           (member direction (fset:lookup visits position) :test #'equal))
         (rotate (direction)
           (or (case (car direction)
                 (-1 '(0 .  1))         ; up -> right
                 ( 1 '(0 . -1))         ; down -> left
                 )
               (case (cdr direction)
                 (-1 '(-1 . 0))         ; left -> up
                 ( 1 '( 1 . 0))         ; right -> down
                 )))
         (walk (grid start direction)
           (loop :with visits
                   := (fset:with (fset:empty-map) start (list direction))
                 :and current := start
                 :for (x . y) := current
                 :for (dx . dy) := direction
                 :for next := (cons (+ x dx) (+ y dy))
                 :for (is-obstacle is-on-grid) := (multiple-value-list
                                                   (fset:lookup grid next))
                 :while (and is-on-grid (not (in-loop-p visits next direction)))
                 :if is-obstacle
                   :do (setf direction (rotate direction))
                 :else :do (setf current next
                                 visits (register-visit visits next direction))
                 :finally (return (values visits is-on-grid)))))
      (let ((visits (walk original-grid original-start original-direction))
            (count 0))
        (if is-part-two
            (fset:do-map (position directions (fset:less visits original-start))
              (let* ((x (car position)) (y (cdr position))
                     ;; Use direction of initial visit for starting
                     ;; next to the new obstacle:
                     (direction (car (last directions)))
                     (dx (car direction)) (dy (cdr direction)))
                (multiple-value-bind (_ is-loop)
                    (walk (fset:with original-grid position t)
                          (cons (- x dx) (- y dy))
                          direction)
                  (declare (ignore _))
                  (when is-loop (incf count)))))
            (setf count (fset:size visits)))
        count))))
