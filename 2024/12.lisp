(ql:quickload :fset)

(defun day12 (is-part-two)
  (let ((grid (fset:empty-map))
        (regions (fset:empty-map))
        (unprocessed-plots))
    (loop :for x :from 0 :for line :in (uiop:read-file-lines #P"./12.txt")
          :do (loop :for y :from 0 :for c :across line
                    :do (fset:includef grid (cons x y) c))
          :finally (setf unprocessed-plots (fset:domain grid)))
    (labels ((get-adjacent (position)
               (let ((x (car position)) (y (cdr position)))
                 (loop :for (dx . dy) :in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                       :collect (cons (+ x dx) (+ y dy)))))
             (walk (region-id position)
               (fset:removef unprocessed-plots position)
               (setf regions
                     (fset:update (lambda (region)
                                    (fset:with (or region (fset:empty-set))
                                               position))
                                  regions
                                  region-id))
               (loop :with type := (fset:lookup grid position)
                     :for adjacent :in (get-adjacent position)
                     :when (and (fset:member? adjacent unprocessed-plots)
                                (char= (fset:lookup grid adjacent) type))
                       :do (walk region-id adjacent)))
             (count-corners (position)
               (let ((x (car position)) (y (cdr position)))
                 (loop :for (dx . dy) :in '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1))
                       :for (a b c d)
                         := (mapcar (lambda (p) (fset:lookup grid p))
                                    (list position
                                          (cons (+ x dx) y) (cons x (+ y dy))
                                          (cons (+ x dx) (+ y dy))))
                       :count (or (and (eql a b) (eql a c) (not (eql a d)))
                                  (not (or (eql a b) (eql a c)))))))
             (get-perimiter (region)
               (let ((result 0))
                 (if is-part-two
                     (fset:do-set (position region result)
                       (incf result (count-corners position)))
                     (fset:do-set (position region result)
                       (loop :for adjacent :in (get-adjacent position)
                             :unless (fset:member? adjacent region)
                               :do (incf result)))))))
      (loop :for position := (fset:arb unprocessed-plots)
            :for i :from 0
            :for region-id := (cons i (fset:lookup grid position))
            :while position
            :do (walk region-id position))
      (let ((result 0))
        (fset:do-map (_ region regions result)
          (declare (ignore _))
          (incf result (* (fset:size region) (get-perimiter region))))))))
