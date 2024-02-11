(defun day15 (is-part-two)
  (let ((input (uiop:read-file-line #P"./15.txt")))
    (labels ((hash (s)
               (loop :with n := 0
                     :for c :across s
                     :do (setf n (rem (* 17 (+ n (char-code c))) 256))
                     :finally (return n))))
      (if is-part-two
          nil
          (loop :for step :in (ppcre:split "," input)
                :sum (hash step))))))
