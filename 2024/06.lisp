(ql:quickload :fset)

(defun day06 (is-part-two)
  (let ((original-grid (fset:empty-map))
        (start)
        (initial-direction '(-1 . 0)))
    (loop :for line :in (uiop:read-file-lines #P"./06.txt")
          :for x :from 0
          :do (loop :for c :across line :for y :from 0
                    :for pos := (cons x y)
                    :when (char= c #\^)
                      :do (setf start pos)
                    :do (setf (fset:lookup original-grid pos)
                              (char= c #\#))))
    (labels
        ((register-visit (visits position direction)
           (fset:with visits (cons position direction)))
         (in-loop-p (visits position direction)
           (fset:lookup visits (cons position direction)))
         (get-visited-positions (visits)
           (let ((positions (fset:empty-set)))
             (fset:do-set (visit visits)
               (setf positions (fset:with positions (car visit))))
             positions))
         (rotate (direction)
           (or (case (car direction)
                 (-1 '(0 .  1))         ; up -> right
                 ( 1 '(0 . -1))         ; down -> left
                 )
               (case (cdr direction)
                 (-1 '(-1 . 0))         ; left -> up
                 ( 1 '( 1 . 0))         ; right -> down
                 )))
         (walk (grid)
           (loop :with visits
                   := (fset:with (fset:empty-set)
                                 (cons start initial-direction))
                 :and direction := initial-direction
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
                 :finally (return (values (get-visited-positions visits)
                                          is-on-grid)))))
      (if is-part-two
          (let ((visited-positions (walk original-grid))
                (count 0))
            (fset:do-set (position (fset:less visited-positions start))
              (multiple-value-bind (_ is-loop)
                  (walk (fset:with original-grid position t))
                (declare (ignore _))
                (when is-loop (incf count))))
            count)
          (fset:size (walk original-grid))))))
