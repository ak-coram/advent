(ql:quickload :fset)

(defun day23 (is-part-two)
  (let ((connections (fset:empty-map (fset:empty-set))))
    (loop :for line :in (uiop:read-file-lines #P"./23.txt")
          :for (a b) := (mapcar #'intern
                                (uiop:split-string line :separator '(#\-)))
          :for previous-a := (fset:lookup connections a)
          :for previous-b := (fset:lookup connections b)
          :do (fset:includef connections a (fset:with previous-a b))
          :do (fset:includef connections b (fset:with previous-b a)))
    (labels ((chiefp (node) (char= (aref (string node) 0) #\t))
             (clique (start-node)
               (loop :with clique := (fset:set start-node)
                     :and queue := (fset:seq start-node)
                     :for current := (fset:pop-first queue)
                     :while current
                     :do (fset:do-set (n (fset:lookup connections current))
                           (when (fset:subset? clique
                                               (fset:lookup connections n))
                             (unless (fset:member? n clique)
                               (fset:push-last queue n))
                             (fset:includef clique n)))
                     :finally (return clique)))
             (find-cliques ()
               (loop :with nodes := (fset:domain connections)
                     :and cliques := (fset:set)
                     :for visited
                       := (fset:reduce #'fset:union cliques
                                       :initial-value (fset:empty-set))
                     :for node := (fset:arb (fset:set-difference nodes visited))
                     :while node :do (fset:includef cliques (clique node))
                     :finally (return cliques))))
      (if is-part-two
          (format nil "~{~A~^,~}"
                  (fset:convert 'list
                                (fset:first (fset:sort (find-cliques) #'>
                                                       :key #'fset:size))))
          (let ((sets-of-three (fset:empty-set)))
            (fset:do-set (chief (fset:filter #'chiefp (fset:domain connections))
                                (fset:size sets-of-three))
              (fset:do-set (second-node (fset:lookup connections chief))
                (fset:do-set (third-node (fset:lookup connections second-node))
                  (when (fset:member? chief (fset:lookup connections
                                                         third-node))
                    (fset:includef sets-of-three (fset:set chief
                                                           second-node
                                                           third-node)))))))))))
