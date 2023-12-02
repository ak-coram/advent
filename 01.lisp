(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

(defun day01 (is-part-two)
  (let* ((digit-mapping (loop :for i :below 10
                              :collect (cons (format nil "~a" i) i)
                              :when is-part-two
                                :collect (cons (format nil "~r" i) i)))
         (digits `(:alternation ,@(loop :for (s nil) :in digit-mapping
                                        :collect `(:register ,s))))
         (first-scanner (ppcre:create-scanner
                         `(:sequence :start-anchor
                                     (:non-greedy-repetition 0 nil :everything)
                                     (:register ,digits))))
         (last-scanner (ppcre:create-scanner
                        `(:sequence (:greedy-repetition 0 nil :everything)
                                    (:register ,digits)
                                    (:non-greedy-repetition 0 nil :everything)
                                    :end-anchor))))
    (labels ((digit-to-integer (s)
               (alexandria:assoc-value digit-mapping s :test 'string=)))
      (loop :for line :in (uiop:read-file-lines #P"./01.txt")
            :sum (or (ppcre:register-groups-bind (first) (first-scanner line)
                       (ppcre:register-groups-bind (last) (last-scanner line)
                         (+ (* 10 (digit-to-integer first))
                            (digit-to-integer last))))
                     0)))))

