(ql:quickload :fset)

(defun day11 (is-part-two)
  (let ((cache (fset:empty-map)))
    (labels
        ((read-input ()
           (mapcar #'parse-integer (uiop:split-string
                                    (uiop:read-file-line #P"./11.txt"))))
         (lookup (key) (fset:lookup cache key))
         (save (key result)
           (fset:includef cache key result)
           result)
         (tally (depth stone)
           (if (zerop depth)
               1
               (let ((cache-key (cons depth stone)))
                 (or (lookup cache-key)
                     (save cache-key
                           (if (zerop stone)
                               (tally (1- depth) 1)
                               (let* ((digit-count (floor (1+ (log stone 10))))
                                      (m (expt 10 (floor digit-count 2))))
                                 (if (evenp digit-count)
                                     (+ (tally (1- depth) (floor stone m))
                                        (tally (1- depth) (mod stone m)))
                                     (tally (1- depth) (* stone 2024)))))))))))
      (loop :for stone :in (read-input)
            :sum (tally (if is-part-two 75 25) stone)))))
