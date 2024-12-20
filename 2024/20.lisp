(ql:quickload :fset)

(defun day20 (is-part-two)
  (let ((walls (fset:empty-set)) (start) (end) (path (fset:empty-map)))
    (loop :for line :in (uiop:read-file-lines #P"./20.txt") :for x :from 0
          :do (loop :for c :across line :for y :from 0 :for p := (cons x y)
                    :do (case c
                          (#\S (setf start p)) (#\E (setf end p))
                          (#\# (fset:includef walls p)))))
    (loop :with current := start :and previous := nil
          :for n :from 1 :for (x . y) := current
          :until (equal current end)
          :do (loop :for (dx . dy) :in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                    :for next := (cons (+ x dx) (+ y dy))
                    :unless (or (equal next previous) (fset:member? next walls))
                      :do (fset:includef path next n)
                      :and :return (setf previous current current next))
          :finally (fset:includef path start 0))
    (labels ((distance (p q) (let ((px (car p)) (py (cdr p))
                                   (qx (car q)) (qy (cdr q)))
                               (+ (abs (- px qx)) (abs (- py qy))))))
      (let ((cheats (fset:bag)))
        (fset:do-map (p p-score path)
          (fset:do-map (q q-score path)
            (when (< p-score q-score)
              (let* ((d (distance p q)) (score (- q-score p-score d)))
                (when (and (<= d (if is-part-two 20 2)) (plusp score))
                  (fset:includef cheats score))))))
        (fset:size (fset:filter (lambda (n) (<= 100 n)) cheats))))))
