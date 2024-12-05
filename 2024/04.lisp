(ql:quickload :fset)

(defun day04 (is-part-two)
  (let ((grid) (count 0))
    (loop :with m := (fset:empty-map)
          :for line :in (uiop:read-file-lines #P"./04.txt")
          :for x :from 0 :do (loop :for c :across line
                                   :for y :from 0
                                   :for pos := (list x y)
                                   :do (setf (fset:lookup m pos) c))
          :finally (setf grid m))
    (labels ((@= (c x y) (eql c (fset:lookup grid (list x y))))
             (count-xmas-occurences (start)
               (loop :for (dx dy) :in '((-1 -1) (1  1) (1 -1) (-1  1)
                                        (-1  0) (0 -1) (0  1) ( 1  0))
                     :count (loop :with (x y) := start
                                  :for c :across "XMAS"
                                  :unless (@= c x y)
                                    :return nil
                                  :do (incf x dx) :do (incf y dy)
                                  :finally (return t))))
             (x-mas-p (center) (destructuring-bind (x y) center
                                 (and (@= #\A x y)
                                      (or (and (@= #\M (1- x) (1- y))
                                               (@= #\S (1+ x) (1+ y)))
                                          (and (@= #\M (1+ x) (1+ y))
                                               (@= #\S (1- x) (1- y))))
                                      (or (and (@= #\M (1- x) (1+ y))
                                               (@= #\S (1+ x) (1- y)))
                                          (and (@= #\M (1+ x) (1- y))
                                               (@= #\S (1- x) (1+ y))))))))
      (if is-part-two
          (fset:do-map-domain (center grid)
            (when (x-mas-p center)
              (incf count)))
          (fset:do-map-domain (start grid)
            (incf count (count-xmas-occurences start))))
      count)))
