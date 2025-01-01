(ql:quickload :fset)

(defun day02 (is-part-two)
  (let ((program (fset:empty-seq)))
    (loop :for s :in (uiop:split-string (uiop:read-file-line #P"./02.txt")
                                        :separator '(#\,))
          :do (fset:push-last program (parse-integer s)))
    (macrolet ((@ (p) `(nth-value 0 (fset:lookup memory ,p)))
               (! (p v) `(cons (fset:with memory ,p ,v) (+ i 4)))
               (let-params (vars &rest body)
                 `(let (,@(loop :for var :in vars
                                :for j :from 1
                                :collect `(,var (@ (+ i ,j)))))
                    ,@body)))
      (labels ((init (memory noun verb)
                 (fset:with (fset:with memory 1 noun) 2 verb))
               (exec (memory i)
                 (case (fset:lookup memory i)
                   (1 (let-params (a b c) (! c (+ (@ a) (@ b)))))
                   (2 (let-params (a b c) (! c (* (@ a) (@ b)))))
                   (99 (cons memory nil))))
               (run (program)
                 (loop :for ip := 0 :then next-ip
                       :for memory := program :then next-memory
                       :while ip
                       :for (next-memory . next-ip) := (exec memory ip)
                       :finally (return (@ 0)))))
        (if is-part-two
            (loop :named outer
                  :for noun :from 0 :upto 99
                  :do (loop :for verb :from 0 :upto 99
                            :when (eql (run (init program noun verb)) 19690720)
                              :do (return-from outer (+ (* 100 noun) verb))))
            (run (init program 12 2)))))))
