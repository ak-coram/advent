(ql:quickload :fset)

(defun day22 (is-part-two)
  (macrolet ((shift-mix-prune (&rest xs)
               `(progn ,@(mapcar (lambda (x)
                                   `(setf n (mod (logxor n (ash n ,x))
                                                 16777216)))
                                 xs))))
    (labels ((evolve (n) (shift-mix-prune 6 -5 11))
             (banana-prices (n initial-secret)
               (loop :with changes := (fset:empty-seq)
                     :and prices := (fset:empty-bag)
                     :for i :below n
                     :for previous := initial-secret :then current
                     :for current := (evolve initial-secret)
                       :then (evolve current)
                     :for price := (mod current 10)
                     :do (fset:push-last changes
                                         (- price (mod previous 10)))
                     :when (< 2 i)
                       :do (unless (fset:member? changes prices)
                             (fset:includef prices changes price))
                       :and :do (fset:pop-first changes)
                     :finally (return prices))))
      (let ((initial-secrets (mapcar #'parse-integer
                                     (uiop:read-file-lines #P"./22.txt"))))
        (if is-part-two
            (fset:greatest
             (fset:range
              (fset:convert
               'fset:map
               (fset:reduce (lambda (acc secret)
                              (fset:bag-sum acc (banana-prices 2000 secret)))
                            initial-secrets
                            :initial-value (fset:empty-bag)))))
            (loop :for secret :in initial-secrets
                  :sum (dotimes (_ 2000 secret)
                         (setf secret (evolve secret)))))))))
