(defun day10 (is-part-two)
  (labels ((look-and-say (digits)
             (loop :with results := nil :and prev := nil :and count := 0
                   :for d :in digits
                   :when (null prev) :do (setf prev d)
                     :do (if (eql prev d)
                             (incf count)
                             (progn
                               (push count results)
                               (push prev results)
                               (setf prev d count 1)))
                   :finally (return (progn
                                      (push count results)
                                      (push prev results)
                                      (nreverse results))))))
    (loop :with acc
            := (mapcar #'digit-char-p (coerce (uiop:read-file-line #P"./10.txt")
                                              'list))
          :for i :below (if is-part-two 50 40)
          :do (setf acc (look-and-say acc))
          :finally (return (length acc)))))
