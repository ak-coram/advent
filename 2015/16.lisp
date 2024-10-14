(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day16 (is-part-two)
  (let ((candidates
          (loop :for line :in (uiop:read-file-lines #P"./16.txt")
                :for attributes := nil
                :collect (ppcre:register-groups-bind
                             ((#'parse-integer id) rest)
                             ("Sue (\\d+):(.*)" line)
                           (ppcre:do-register-groups
                               ((#'intern key) (#'parse-integer value))
                               ("[, ]*(\\w+): (\\d+)" rest)
                             (push (cons key value) attributes))
                           (cons id attributes))))
        (facts '((|children| . 3) (|cats| . 7)
                 (|samoyeds| . 2) (|pomeranians| . 3)
                 (|akitas| . 0) (|vizslas| . 0)
                 (|goldfish| . 5) (|trees| . 3)
                 (|cars| . 2) (|perfumes| . 1))))
    (loop :for (id . attributes) :in candidates
          :when (loop :for (key . value) :in facts
                      :for candidate-value
                        := (alexandria:assoc-value attributes key)
                      :for comparison-function
                        := (cond
                             ((not is-part-two) #'eql)
                             ((member key '(|cats| |trees|)) #'<)
                             ((member key '(|pomeranians| |goldfish|)) #'>)
                             (t #'eql))
                      :when (and candidate-value
                                 (not (funcall comparison-function
                                               value
                                               candidate-value)))
                        :return nil
                      :finally (return t))
            :return id)))
