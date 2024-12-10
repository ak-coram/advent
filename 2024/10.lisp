(ql:quickload :fset)

(defun day10 (is-part-two)
  (let ((grid (fset:empty-map))
        (trailheads (fset:empty-set))
        (reachable-peaks (fset:empty-map))
        (count 0))
    (loop :for line :in (uiop:read-file-lines #P"./10.txt")
          :for x :from 0
          :do (loop :for c :across line :for y :from 0
                    :for pos := (cons x y)
                    :when (char= c #\0)
                      :do (setf trailheads (fset:with trailheads pos))
                    :do (setf (fset:lookup grid pos) (digit-char-p c))))
    (labels ((get-next (position)
               (let ((x (car position)) (y (cdr position))
                     (next-height (1+ (fset:lookup grid position))))
                 (loop :for (dx . dy) :in '((1 .  0) (-1 . 0)
                                            (0 . -1) ( 0 . 1))
                       :for next := (cons (+ x dx) (+ y dy))
                       :when (eql next-height (fset:lookup grid next))
                         :collect next)))
             (walk (trailhead
                    &optional (position trailhead) (visits (fset:empty-set)))
               (when (eql 9 (fset:lookup grid position))
                 (setf reachable-peaks (fset:update
                                        (lambda (peaks) (cons position peaks))
                                        reachable-peaks trailhead)))
               (loop :with visits := (fset:with visits position)
                     :for target :in (get-next position)
                     :unless (fset:member? target visits)
                       :do (walk trailhead target visits))))
      (fset:do-set (trailhead trailheads) (walk trailhead))
      (fset:do-map (_ peaks reachable-peaks)
        (declare (ignore _))
        (incf count (length (if is-part-two
                                peaks
                                (remove-duplicates peaks :test #'equal)))))
      count)))
