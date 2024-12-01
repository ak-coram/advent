(ql:quickload :fset)

(defun day01 (is-part-two)
  (multiple-value-bind (l1 l2)
      (loop :for line :in (uiop:read-file-lines #P"./01.txt")
            :for (x offset) := (multiple-value-list
                                (parse-integer line :junk-allowed t))
            :for y := (parse-integer line :start offset)
            :collect x :into l1 :collect y :into l2
            :finally (return (values l1 l2)))
    (if is-part-two
        (loop :with counts := (fset:convert 'fset:bag l2)
              :for x :in l1
              :sum (* x (fset:count x counts)))
        (loop :for x :in (sort l1 #'<)
              :for y :in (sort l2 #'<)
              :for d := (abs (- x y))
              :sum d))))

