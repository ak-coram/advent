(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day08 (is-part-two)
  (let* ((lines (uiop:read-file-lines "./08.txt"))
         (instructions (car lines))
         (nodes (mapcar (lambda (node-definition)
                          (ppcre:register-groups-bind (a b c)
                              ("(\\w+) = \\((\\w+), (\\w+)\\)" node-definition)
                            (cons (intern a :keyword)
                                  (cons (intern b :keyword)
                                        (intern c :keyword)))))
                        (cddr lines))))
    (destructuring-bind (instructions nodes start-nodes end-nodes)
        (loop :for (node . nil) :in nodes
              :for last-char := (char (symbol-name node) 2)
              :when (eql last-char #\A)
                :collect node :into start-nodes
              :when (eql last-char #\Z)
                :collect node :into end-nodes
              :finally (return (list (loop :for i :across instructions
                                           :collect (intern (format nil "~C" i)
                                                            :keyword))
                                     nodes
                                     (if is-part-two start-nodes (list :AAA))
                                     (if is-part-two end-nodes (list :ZZZ)))))
      (loop :for start-node :in start-nodes
            :collect (loop :with node := start-node
                           :for i :in (apply #'alexandria:circular-list
                                             instructions)
                           :for (l . r) := (alexandria:assoc-value nodes node)
                           :do (setf node (case i
                                            (:L l)
                                            (:R r)))
                           :count t :into step-count
                           :when (member node end-nodes)
                             :return step-count)
              :into cycles
            :finally (return (apply #'lcm cycles))))))
