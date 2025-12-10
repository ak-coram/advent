(ql:quickload '(:cl-ppcre :fset))

(defun day10 ()
  (labels ((bits (n) (make-array (list n) :element-type 'bit
                                          :initial-element 0))
           (zeros (n) (make-array (list n) :initial-element 0))
           (parse-goal (input)
             (loop :with result := (bits (length input))
                   :for i :from 0 :for c :across input
                   :when (char= c #\#)
                     :do (setf (aref result i) 1)
                   :finally (return result)))
           (parse-buttons (n input)
             (loop :with result := (fset:empty-set)
                   :for s :in (ppcre:split " " input)
                   :for indices
                     := (ppcre:regex-replace-all "[\\(\\)]" s "")
                   :do (loop :with button := (bits n)
                             :for index :in (ppcre:split "," indices)
                             :for i := (parse-integer index)
                             :do (setf (aref button i) 1)
                             :finally (fset:includef result button))
                   :finally (return result)))
           (parse-joltages-goal (input)
             (loop :with joltages := (ppcre:split "," input)
                   :with result := (zeros (length joltages))
                   :for joltage :in joltages
                   :for i :from 0
                   :do (setf (aref result i) (parse-integer joltage))
                   :finally (return result)))
           (get-combinations (buttons n)
             (loop :with old-combinations
                     := (fset:set (fset:empty-bag))
                   :for i :from 0 :below n
                   :for new-combinations := (fset:empty-set)
                   :do (fset:do-set (presses old-combinations)
                         (fset:do-set (button buttons)
                           (fset:includef new-combinations
                                          (fset:with presses button))))
                   :do (setf old-combinations new-combinations)
                   :finally (return old-combinations)))
           (apply-toggle-buttons (state presses)
             (fset:reduce #'bit-xor presses :initial-value state)))
    (let ((machines
            (loop :for line :in (uiop:read-file-lines #P"./10.txt")
                  :collect (ppcre:register-groups-bind
                               (goal buttons joltages)
                               ("\\[(.*)\\] (.*) {(.*)}" line)
                             (list (parse-goal goal)
                                   (parse-joltages-goal joltages)
                                   (parse-buttons (length goal) buttons))))))
      (loop :for (goal _ buttons) :in machines
            :sum (loop :with initial-state := (bits (length goal))
                       :for n :from 1
                       :for combinations := (get-combinations buttons n)
                       :when (fset:do-set (presses combinations)
                               (when (equalp (apply-toggle-buttons
                                              initial-state
                                              presses)
                                             goal)
                                 (return t)))
                         :return n)))))
