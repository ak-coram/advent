(defun day02 (is-part-two)
  (labels ((validp (levels)
             (loop :with pred := nil
                   :for (current . (next)) :on levels
                   :while next :for d := (- current next)
                   :unless pred
                     :do (setf pred (if (plusp d) #'plusp #'minusp))
                   :unless (and (funcall pred d) (<= 1 (abs d) 3))
                     :return nil
                   :finally (return t)))
           (get-variations (levels)
             (loop :for i :below (length levels)
                   :collect (loop :for level :in levels
                                  :for j :from 0
                                  :unless (eql i j) :collect level))))
    (let ((reports (loop :for line :in (uiop:read-file-lines #P"./02.txt")
                         :collect
                         (loop :for (level offset)
                                 := (multiple-value-list
                                     (parse-integer line
                                                    :start (or offset 0)
                                                    :junk-allowed t))
                               :while level :collect level))))
      (if is-part-two
          (loop :for levels :in reports
                :count (or (validp levels)
                           (find-if #'validp (get-variations levels))))
          (count-if #'validp reports)))))
