(ql:quickload :fset)

(defun day06 (is-part-two)
  (let ((primaries (fset:empty-map (fset:empty-set)))
        (secondaries (fset:empty-map)))
    (loop :for line :in (uiop:read-file-lines #P"./06.txt")
          :for (a b) := (mapcar #'intern
                                (uiop:split-string line
                                                   :separator '(#\))))
          :do (setf primaries
                    (fset:update (lambda (nodes) (fset:with nodes b))
                                 primaries
                                 a))
          :do (fset:includef secondaries b a))
    (labels ((count-orbits (node &optional (n 0))
               (let ((sum n))
                 (fset:do-set (orbiter (fset:lookup primaries node))
                   (incf sum (count-orbits orbiter (1+ n))))
                 sum))
             (path (node)
               (loop :with path := nil
                     :for current := node :then (fset:lookup secondaries
                                                             current)
                     :while current :do (push current path)
                     :finally (return path))))
      (if is-part-two
          (loop :for (a . rest-a) :on (path 'you)
                :for (b . rest-b) :on (path 'san)
                :while (eql a b)
                :finally (return (+ (length rest-a) (length rest-b))))
          (count-orbits 'com)))))
