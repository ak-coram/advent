(ql:quickload :cl-ppcre)

(defun day15 (is-part-two)
  (let* ((ingredients
           (loop
             :with property
               := `(:sequence
                    (:alternation #\: #\,) #\space
                    (:greedy-repetition 0 nil :word-char-class) #\space
                    (:register
                     (:sequence (:non-greedy-repetition 0 1 #\-)
                                (:greedy-repetition 1 nil :digit-class))))
             :with scanner
               := (ppcre:create-scanner
                   `(:sequence :start-anchor
                               (:greedy-repetition 0 nil :word-char-class)
                               ,@(loop :for i :below 5 :collect property)))
             :for line :in (uiop:read-file-lines #P"./15.txt")
             :collect (ppcre:register-groups-bind ((#'parse-integer capacity
                                                                    durability
                                                                    flavor
                                                                    texture
                                                                    calories))
                          (scanner line)
                        (list capacity durability flavor texture calories))))
         (properties (apply #'mapcar #'list ingredients)))
    (labels ((partition-integer (i n)
               (let ((result))
                 (labels ((f (i n &optional acc)
                            (cond
                              ((eql n 1) (push (cons i acc) result))
                              ((zerop i) (f 0 (1- n) (cons 0 acc)))
                              (t (loop :for j :upto i
                                       :do (f (- i j) (1- n) (cons j acc)))))
                            result))
                   (f i n))))
             (calculate-score (combination)
               (apply #'*
                      (loop :for constants :in (butlast properties)
                            :collect (max 0 (loop :for n :in combination
                                                  :for constant :in constants
                                                  :sum (* n constant))))))
             (calculate-calories (combination)
               (loop :for n :in combination
                     :for constant :in (car (last properties))
                     :sum (* n constant))))
      (loop :for combination :in (partition-integer 100 (length ingredients))
            :when (or (not is-part-two)
                      (eql (calculate-calories combination) 500))
              :maximize (calculate-score combination)))))
