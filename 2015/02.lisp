(defun day02 (is-part-two)
  (let ((presents (mapcar (lambda (dimensions)
                            (mapcar #'parse-integer
                                    (uiop:split-string dimensions
                                                       :separator '(#\x))))
                          (uiop:read-file-lines #P"./02.txt"))))
    (if is-part-two
        (loop :for dimensions :in presents :for (l w h) := dimensions
              :for (x y) := (sort dimensions #'<)
              :sum (+ x x y y (* l w h)))
        (loop :for (l w h) :in presents
              :for a1 := (* l w) :for a2 := (* w h) :for a3 := (* h l)
              :sum (+ (* 2 a1) (* 2 a2) (* 2 a3) (min a1 a2 a3))))))
