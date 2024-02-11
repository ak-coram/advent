(defun day14 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./14.txt"))
         (height (length lines))
         (width (length (car lines)))
         (platform (make-array (list height width) :initial-contents lines))
         (cache (make-hash-table :test 'equalp)))
    (labels ((ref (direction y x)
               (case direction
                 (:north (list (1- y) x))
                 (:west (list y (1- x)))
                 (:south (list (1+ y) x))
                 (:east (list y (1+ x)))))
             (tilt-step (direction)
               (loop :for y :from 0 :below height
                     :sum (loop :for x :from 0 :below width
                                :for c := (aref platform y x)
                                :for (dy dx) := (ref direction y x)
                                :when (and (char= c #\O)
                                           (ignore-errors
                                            (char= (aref platform dy dx) #\.)))
                                  :count (setf (aref platform y x) #\.
                                               (aref platform dy dx) #\O))))
             (cycle ()
               (loop :for direction :in (if is-part-two
                                            '(:north :west :south :east)
                                            '(:north))
                     :do (loop :while (plusp (tilt-step direction))))))
      (loop :repeat (loop :with cycles := 1000000000
                          :for n :from 0
                          :for previous-n := (gethash platform cache)
                          :until previous-n
                          :do (setf (gethash platform cache) n)
                          :do (cycle)
                          :finally (return (mod (- cycles previous-n)
                                                (- n previous-n))))
            :do (cycle))
      (loop :for y :from 0 :below height
            :for factor := (- height y)
            :sum (* factor
                    (loop :for x :below width
                          :count (char= (aref platform y x) #\O)))))))
