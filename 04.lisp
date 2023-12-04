(ql:quickload :cl-ppcre)

(defun day04 (is-part-two)
  (let* ((everything `(:register (:greedy-repetition 0 nil :everything)))
         (whitespace `(:greedy-repetition 0 nil :whitespace-char-class))
         (integer `(:register (:greedy-repetition 1 nil :digit-class)))
         (scanner (ppcre:create-scanner
                   `(:sequence "Card" ,whitespace ,integer
                                ":" ,whitespace ,everything ,whitespace "|"
                               ,whitespace ,everything
                               :end-anchor)))
         (lines (uiop:read-file-lines "./04.txt"))
         (cards (labels ((parse-integers (s) (mapcar #'parse-integer
                                                     (ppcre:split "\\s+" s))))
                  (loop :for line :in lines
                        :collect
                        (ppcre:register-groups-bind (card-number
                                                     numbers
                                                     winning-numbers)
                            (scanner line)
                          (cons (parse-integer card-number)
                                (length (intersection
                                         (parse-integers numbers)
                                         (parse-integers winning-numbers)
                                         :test 'eql))))))))
    (if is-part-two
        (loop :with copies := cards
              :and new-copies := nil
              :while copies
              :do (loop :for (card-number . n) :in copies
                        :do (loop :for i :from (1+ card-number)
                                    :upto (+ card-number n)
                                  :for copy := (assoc i cards)
                                  :when copy :do (push copy new-copies)))
              :sum (length copies)
              :do (setf copies new-copies new-copies nil))
        (loop :for (card-number . n) :in cards
              :sum (if (zerop n) 0 (expt 2 (1- n)))))))
