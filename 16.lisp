(defun day16 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./16.txt"))
         (height (length lines))
         (width (length (car lines))))
    (labels ((cast-light (cache position direction)
               (unless (member direction (gethash position cache))
                 (push direction (gethash position cache))
                 (loop :with (x y) := position
                       :with to-directions
                         := (case (char (nth y lines) x)
                              (#\. (list direction))
                              (#\| (case direction
                                     ((:up :down) (list direction))
                                     (t '(:up :down))))
                              (#\- (case direction
                                     ((:left :right) (list direction))
                                     (t '(:left :right))))
                              (#\/ (case direction
                                     (:up '(:right)) (:down '(:left))
                                     (:left '(:down)) (:right '(:up))))
                              (#\\ (case direction
                                     (:up '(:left)) (:down '(:right))
                                     (:left '(:up)) (:right '(:down)))))
                       :for to-direction :in to-directions
                       :for next-x := (case to-direction
                                        (:left (1- x)) (:right (1+ x)) (t x))
                       :for next-y := (case to-direction
                                        (:up (1- y)) (:down (1+ y)) (t y))
                       :when (and (< -1 next-x width) (< -1 next-y height))
                         :do (cast-light cache
                                         (list next-x next-y)
                                         to-direction))))
             (count-powered-tiles (start-position start-direction)
               (let ((cache (make-hash-table :test 'equal)))
                 (cast-light cache start-position start-direction)
                 (hash-table-count cache))))
      (if is-part-two
          (max (loop :for x :from 0 :below width
                     :maximize
                     (max (count-powered-tiles (list x (1- height)) :up)
                          (count-powered-tiles (list x 0) :down)))
               (loop :for y :from 0 :below height
                     :maximize
                     (max (count-powered-tiles (list (1- width) y) :left)
                          (count-powered-tiles (list 0 y) :right))))
          (count-powered-tiles '(0 0) :right)))))

