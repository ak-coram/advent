(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day14 (is-part-two)
  (labels
      ((parse-input ()
         (loop :for line :in (uiop:read-file-lines #P"./14.txt")
               :collect
               (ppcre:register-groups-bind (name speed duration rest-duration)
                   ("(\\w+) can fly (\\d+) km/s for (\\d+) .* rest for (\\d+)"
                    line)
                 (cons (intern name)
                       (mapcar #'parse-integer
                               (list speed duration rest-duration))))))
       (calculate-distance (total-duration speed duration rest-duration)
         (multiple-value-bind (full-cycle-count remaining-seconds)
             (floor total-duration (+ duration rest-duration))
           (+ (* full-cycle-count duration speed)
              (* (min duration remaining-seconds) speed))))
       (sort-alist (alist) (sort alist #'> :key #'cdr)))
    (if is-part-two
        (loop :with deers := (parse-input)
              :with totals := (mapcar (lambda (deer)
                                        (cons (car deer) 0))
                                      deers)
              :for i :from 1 :upto 2503
              :do (loop :for (name . stats) :in deers
                        :for distance := (apply #'calculate-distance i stats)
                        :collect (cons name distance) :into results
                        :finally (loop :with scores := (sort-alist results)
                                       :with max-distance := (cdar scores)
                                       :for (name . distance) :in scores
                                       :while (eql max-distance distance)
                                       :do (incf (alexandria:assoc-value totals
                                                                         name))))
              :finally (return (cdar (sort-alist totals))))
        (loop :for (name . stats) :in (parse-input)
              :maximize (apply #'calculate-distance 2503 stats)))))
