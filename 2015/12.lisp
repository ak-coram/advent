(ql:quickload :alexandria)
(ql:quickload :com.inuoe.jzon)

(defun day12 (is-part-two)
  (let ((sum 0))
    (labels ((walk (v)
               (etypecase v
                 (simple-vector (loop :for x :across v :do (walk x)))
                 (hash-table (let ((values (alexandria:hash-table-values v)))
                               (unless (and is-part-two
                                           (member "red" values :test #'equal))
                                 (loop :for x :in values :do (walk x)))))
                 (integer (incf sum v))
                 (string nil))))
      (walk (com.inuoe.jzon:parse (uiop:read-file-string #P"./12.txt"))))
    sum))
