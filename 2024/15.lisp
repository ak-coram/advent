(ql:quickload '(:cl-ppcre :fset))

(defun day15 (is-part-two)
  (labels ((move (position direction)
             (when position
               (let ((x (car position)) (y (cdr position)))
                 (case direction
                   (:left (cons x (1- y))) (:right (cons x (1+ y)))
                   (:up (cons (1- x) y)) (:down (cons (1+ x) y))))))
           (get-thing (c)
             (case c (#\@ :robot) (#\. :space) (#\# :wall) (#\O  :box)))
           (sort-swaps (swaps direction)
             (case direction
               (:up (fset:sort swaps #'< :key #'caar))
               (:down (fset:sort swaps #'> :key #'caar))
               (:left (fset:sort swaps #'< :key #'cdar))
               (:right (fset:sort swaps #'> :key #'cdar))))
           (get-swaps (grid robot direction)
             (loop :with queue := (fset:seq robot)
                   :and swaps := (fset:empty-set)
                   :and visited := (fset:empty-set)
                   :for position := (fset:pop-first queue)
                   :while position
                   :for next-position := (move position direction)
                   :for (thing . sibling) := (fset:lookup grid next-position)
                   :for sibling-position := (move next-position sibling)
                   :when (eql thing :wall)
                     :return (fset:empty-seq)
                   :do (when (and sibling-position
                                  (not (fset:member? sibling-position visited)))
                         (fset:push-first queue sibling-position))
                   :do (fset:includef swaps (list position next-position))
                   :unless (eql thing :space)
                     :do (fset:push-last queue next-position)
                   :do (fset:includef visited next-position)
                   :finally (return (sort-swaps swaps direction))))
           (apply-swaps (grid swaps)
             (fset:do-seq (swap swaps :value grid)
               (let* ((p (car swap)) (q (cadr swap))
                      (a (fset:lookup grid p)) (b (fset:lookup grid q)))
                 (fset:includef grid p b) (fset:includef grid q a)))))
    (destructuring-bind (grid-input move-input)
        (ppcre:split "\\n\\n" (uiop:read-file-string #P"./15.txt"))
      (let* ((initial-grid (fset:empty-map)) (initial-position)
             (moves (loop :for c :across move-input
                          :for d := (case c
                                      (#\< :left) (#\> :right)
                                      (#\^ :up) (#\v :down))
                          :when d :collect d)))
        (loop :for line :in (ppcre:split "\\n" grid-input) :for x :from 0
              :do (if is-part-two
                      (loop :for c :across line :for y :from 0 :by 2
                            :for p1 := (cons x y) :for p2 := (cons x (1+ y))
                            :for thing := (get-thing c)
                            :when (char= c #\@)
                              :do (setf initial-position p1)
                            :do (fset:includef
                                 initial-grid p1
                                 (cons thing (when (eql thing :box) :right)))
                            :do (fset:includef
                                 initial-grid p2
                                 (cons (if (eql thing :robot) :space thing)
                                       (when (eql thing :box) :left))))
                      (loop :for c :across line :for y :from 0
                            :for p := (cons x y)
                            :when (char= c #\@)
                              :do (setf initial-position p)
                            :do (fset:includef initial-grid p
                                               (cons (get-thing c) nil)))))
        (loop :with grid := initial-grid :and position := initial-position
              :and result := 0
              :for direction :in moves
              :for swaps := (get-swaps grid position direction)
              :unless (fset:empty? swaps)
                :do (setf grid (apply-swaps grid swaps)
                          position (move position direction))
              :finally (return (fset:do-map (p thing grid result)
                                 (when (and (eql (car thing) :box)
                                            (or (not is-part-two)
                                                (eql (cdr thing) :right)))
                                   (incf result (+ (* (car p) 100)
                                                   (cdr p)))))))))))
