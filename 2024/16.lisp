(ql:quickload :fset)

(defun day16 (is-part-two)
  (let ((walls (fset:empty-set)) (start) (end) (initial-orientation :east))
    (loop :for line :in (uiop:read-file-lines #P"./16.txt") :for x :from 0
          :do (loop :for c :across line :for y :from 0 :for p := (cons x y)
                    :do (case c
                          (#\S (setf start p)) (#\E (setf end p))
                          (#\# (fset:includef walls p)))))
    (labels ((turn (orientation direction)
               (if direction
                   (ecase direction
                     (:left (case orientation
                              (:east :south) (:south :west)
                              (:west :north) (:north :east)))
                     (:right (case orientation
                               (:east :north) (:north :west)
                               (:west :south) (:south :east))))
                   orientation))
             (move (score position orientation direction)
               (let* ((new-orientation (turn orientation direction))
                      (dx (case new-orientation (:north -1) (:south 1) (t 0)))
                      (dy (case new-orientation (:east -1) (:west 1) (t 0))))
                 (list (+ score (if direction 1001 1))
                       (cons (+ (car position) dx) (+ (cdr position) dy))
                       new-orientation))))
      (loop :with queue
              := (fset:seq `(0 ,(fset:seq start) ,initial-orientation))
            :and seats := (fset:empty-map (fset:empty-set))
            :and visited := (fset:empty-map most-positive-fixnum)
            :for (score positions orientation) := (fset:pop-first queue)
            :for last-position := (fset:last positions)
            :while score
            :if (equal last-position end)
              :do (fset:includef seats score
                                 (fset:union (fset:lookup seats score)
                                             (fset:range positions)))
            :else
              :do (loop :for direction :in '(nil :left :right)
                        :for (next-score . next)
                          := (move score last-position orientation direction)
                        :for (next-position next-orientation) := next
                        :when (and (<= next-score (fset:lookup visited next))
                                   (not (fset:member? next-position walls)))
                          :do (fset:push-last
                               queue
                               (list next-score
                                     (fset:with-last positions next-position)
                                     next-orientation))
                          :and :do (fset:includef visited next next-score))
            :finally (multiple-value-bind (score seats) (fset:least seats)
                       (return (if is-part-two (fset:size seats) score)))))))
