(ql:quickload :alexandria)

(defun day10 (is-part-two)
  (let* ((directions '((:north . (0 -1))
                       (:south . (0 1))
                       (:east . (1 0))
                       (:west . (-1 0))))
         (pipes-to '((#\S . (:north :south :east :west))
                     (#\| . (:north :south))
                     (#\- . (:east :west))
                     (#\F . (:south :east))
                     (#\7 . (:south :west))
                     (#\L . (:north :east))
                     (#\J . (:north :west))))
         (pipes-from (loop :with results := nil
                           :for (pipe . ds) :in pipes-to
                           :do (loop :for d :in ds
                                     :do (push (case d
                                                 (:north :south)
                                                 (:south :north)
                                                 (:east :west)
                                                 (:west :east))
                                               (alexandria:assoc-value results
                                                                       pipe)))
                           :finally (return results)))
         (lines (uiop:read-file-lines #P"./10.txt"))
         (start-pos (loop :named outer
                          :for line :in lines :for y :from 0
                          :for x := (position #\S line)
                          :when x :return (list x y))))
    (labels ((lookup (pos)
               (destructuring-bind (x y) pos
                 (ignore-errors (aref (nth y lines) x))))
             (get-next (pos)
               (loop :with pipe := (lookup pos)
                     :for to-direction :in (alexandria:assoc-value pipes-to pipe)
                     :for (dx dy) := (alexandria:assoc-value directions to-direction)
                     :for next-pos := (list (+ (car pos) dx) (+ (cadr pos) dy))
                     :when (member to-direction
                                   (alexandria:assoc-value pipes-from
                                                           (lookup next-pos)))
                       :collect next-pos))
             (rotate (l) (append (cdr l) (list (car l)))))
      (let ((loop-positions
              (cons start-pos
                    (loop :with prev-pos := nil :and current-pos := start-pos
                          :for next-pos := (car (remove prev-pos (get-next current-pos)
                                                        :test #'equal))
                          :do (setf prev-pos current-pos
                                    current-pos next-pos)
                          :until (eql (lookup current-pos) #\S)
                          :collect current-pos))))
        (if is-part-two
            (let* ((area (/ (abs (loop :for (x1 y1) :in loop-positions
                                       :for (x2 y2) :in (rotate loop-positions)
                                       :sum (* (+ y1 y2) (- x1 x2))))
                            2)))
              (1+ (- area (/ (length loop-positions) 2))))
            (/ (length loop-positions) 2))))))
