(ql:quickload :fset)

(defun day04 (is-part-two)
  (let ((initial-rolls (fset:empty-set)))
    (loop :for line :in (uiop:read-file-lines #P"./04.txt")
          :for y :from 0 :do (loop :for c :across line :for x :from 0
                                   :when (eql c #\@)
                                     :do (fset:includef initial-rolls
                                                        (cons x y))))
    (labels ((get-adjacent (pos)
               (let ((x (car pos)) (y (cdr pos)))
                 (fset:set (cons (1- x) (1- y)) (cons x (1- y))
                           (cons (1+ x) (1- y)) (cons (1+ x) y)
                           (cons (1+ x) (1+ y)) (cons x (1+ y))
                           (cons (1- x) (1+ y)) (cons (1- x) y))))
             (get-accessible (rolls)
               (let ((accessible-rolls (fset:empty-set)))
                 (fset:do-set (roll rolls accessible-rolls)
                   (when (< 4 (fset:size
                               (fset:set-difference (get-adjacent roll) rolls)))
                     (fset:includef accessible-rolls roll))))))
      (if is-part-two
          (loop :with rolls := initial-rolls
                :for removable-rolls := (get-accessible rolls)
                :until (zerop (fset:size removable-rolls))
                :sum (fset:size removable-rolls)
                :do (setf rolls (fset:set-difference rolls removable-rolls)))
          (fset:size (get-accessible initial-rolls))))))
