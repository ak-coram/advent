(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day08 (is-part-two)
  (destructuring-bind (instructions nodes start-nodes end-nodes)
      (loop :named outer
            :with (instructions . network) := (uiop:read-file-lines "./08.txt")
            :for node-definition :in network
            :for node := (ppcre:register-groups-bind (a b c)
                             ("(\\w+) = \\((\\w+), (\\w+)\\)" node-definition)
                           (cons (intern a :keyword)
                                 (cons (intern b :keyword)
                                       (intern c :keyword))))
            :when node
              :collect node :into nodes
            :finally
               (loop :for node :in (mapcar #'car nodes)
                     :for last-char := (char (symbol-name node) 2)
                     :when (eql last-char #\A)
                       :collect node :into start-nodes
                     :when (eql last-char #\Z)
                       :collect node :into end-nodes
                     :finally (return-from outer
                                (list (loop :for i :across instructions
                                            :collect (intern (format nil "~C" i)
                                                             :keyword))
                                      nodes
                                      (if is-part-two
                                          start-nodes
                                          (list :AAA))
                                      (if is-part-two
                                          end-nodes
                                          (list :ZZZ))))))
    (loop :with infinite-instructions := (setf (cdr (last instructions)) instructions)
          :for start-node :in start-nodes
          :collect (loop :with node := start-node
                         :for i :in infinite-instructions
                         :for (l . r) := (alexandria:assoc-value nodes node)
                         :do (setf node (case i
                                          (:L l)
                                          (:R r)))
                         :count t :into step-count
                         :when (member node end-nodes)
                           :return step-count)
            :into cycles
          :finally (return (apply #'lcm cycles)))))

