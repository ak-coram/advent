(ql:quickload :fset)

(defun day03 (is-part-two)
  (labels ((offset (p direction)
             (let ((x (car p)) (y (cdr p)))
               (case direction
                 (:L (cons (1- x) y)) (:R (cons (1+ x) y))
                 (:U (cons x (1- y))) (:D (cons x (1+ y))))))
           (get-wire-positions (wire-def)
             (loop :with p := (cons 0 0) :with wire := (fset:empty-set)
                   :for (direction . n) :in wire-def
                   :do (dotimes (_ n)
                         (setf p (offset p direction))
                         (fset:includef wire p))
                   :finally (return wire)))
           (count-steps (wire-def goals)
             (loop :with p := (cons 0 0) :and steps := 0
                   :and min-steps := (fset:empty-bag)
                   :for (direction . n) :in wire-def
                   :do (dotimes (_ n)
                         (setf p (offset p direction))
                         (incf steps)
                         (when (and (fset:member? p goals)
                                    (not (fset:member? p min-steps)))
                           (fset:includef min-steps p steps)))
                   :finally (return min-steps)))
           (distance (a b)
             (let ((ax (car a)) (ay (cdr a))
                   (bx (car b)) (by (cdr b)))
               (+ (- (abs ax) (abs bx)) (- (abs ay) (abs by))))))
    (let* ((wire-defs
             (loop :for line :in (uiop:read-file-lines #P"./03.txt")
                   :for instructions := (uiop:split-string line
                                                           :separator '(#\,))
                   :collect (loop :for i :in instructions
                                  :for direction := (intern (subseq i 0 1)
                                                            :keyword)
                                  :collect
                                  (cons direction
                                        (parse-integer (subseq i 1))))))
           (w1 (get-wire-positions (car wire-defs)))
           (w2 (get-wire-positions (cadr wire-defs)))
           (intersections (fset:intersection w1 w2)))
      (if is-part-two
          (let ((total-steps (fset:bag-sum (count-steps (car wire-defs)
                                                        intersections)
                                           (count-steps (cadr wire-defs)
                                                        intersections)))
                (min))
            (fset:do-bag-pairs (_ steps total-steps min)
              (declare (ignore _))
              (when (or (null min) (< steps min))
                (setf min steps))))
          (loop :with origin := (cons 0 0)
                :for p :in (fset:convert 'list intersections)
                :minimize (distance p origin))))))
