(ql:quickload :fset)

(defun day08 (is-part-two)
  (let* ((input (uiop:read-file-lines #P"./08.txt"))
         (antennas (fset:empty-map))
         (grid-height (length input))
         (grid-width (length (car input)))
         (antinodes (fset:empty-set)))
    (loop :for line :in input
          :for x :from 0
          :do (loop :for c :across line
                    :for y :from 0
                    :for positions := (or (fset:lookup antennas c)
                                          (fset:empty-set))
                    :unless (char= c #\.)
                      :do (setf (fset:lookup antennas c)
                                (fset:with positions (cons x y)))))
    (labels ((in-bounds-p (position)
               (and (< -1 (car position) grid-height)
                    (< -1 (cdr position) grid-width)))
             (collect-nodes (origin dx dy)
               (loop :with x := (car origin) :and y := (cdr origin)
                     :for position := (cons x y)
                     :while (in-bounds-p position)
                     :collect position
                     :do (incf x dx) :do (incf y dy)))
             (get-antinode-positions (a b)
               (let* ((x1 (car a)) (y1 (cdr a))
                      (x2 (car b)) (y2 (cdr b))
                      (dx (- x2 x1)) (dy (- y2 y1)))
                 (if is-part-two
                     (concatenate 'list
                                  (collect-nodes a (- dx) (- dy))
                                  (collect-nodes b dx dy))
                     (remove-if-not #'in-bounds-p
                                    (list (cons (- x1 dx) (- y1 dy))
                                          (cons (+ x2 dx) (+ y2 dy))))))))
      (fset:do-map (_ positions antennas)
        (declare (ignore _))
        (fset:do-set (p1 positions)
          (fset:do-set (p2 positions)
            (unless (equal p1 p2)
              (loop :for p :in (get-antinode-positions p1 p2)
                    :do (setf antinodes (fset:with antinodes p)))))))
      (fset:size antinodes))))
