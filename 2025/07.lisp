(ql:quickload :fset)

(defun day07 (is-part-two)
  (loop :with split-count := 0 :and timelines := (fset:empty-map 0)
        :for line :in (uiop:read-file-lines #P"./07.txt")
        :do (labels
                ((split-timeline (x)
                   (let* ((n (fset:lookup timelines x))
                          (left (1- x)) (right (1+ x)))
                     (setf timelines
                           (fset:with
                            (fset:with (fset:less timelines x)
                                       left (+ n (fset:lookup timelines left)))
                            right (+ n (fset:lookup timelines right)))))))
              (loop :for c :across line :for x :from 0
                    :do (case c
                          (#\S (progn
                                 (fset:includef timelines x 1)))
                          (#\^ (when (plusp (fset:lookup timelines x))
                                 (incf split-count)
                                 (split-timeline x))))))
        :finally (return (if is-part-two
                             (fset:reduce (lambda (acc _ n)
                                            (declare (ignore _))
                                            (+ acc n))
                                          timelines
                                          :initial-value 0)
                             split-count))))
