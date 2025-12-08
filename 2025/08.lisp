(ql:quickload '(:cl-ppcre :fset))

(defun day08 (is-part-two)
  (labels ((distance (p q) (destructuring-bind (px py pz) p
                             (destructuring-bind (qx qy qz) q
                               (sqrt (+ (expt (- px qx) 2)
                                        (expt (- py qy) 2)
                                        (expt (- pz qz) 2))))))
           (connect (clusters nodes)
             (let ((matches (fset:filter
                             (lambda (cluster)
                               (fset:nonempty? (fset:intersection cluster
                                                                  nodes)))
                             clusters)))
               (case (fset:size matches)
                 (0 (fset:with clusters nodes))
                 (1 (let ((cluster (fset:least matches)))
                      (unless (fset:subset? nodes cluster)
                        (fset:with (fset:less clusters cluster)
                                   (fset:union cluster nodes)))))
                 (2 (fset:with (fset:set-difference clusters matches)
                               (fset:reduce #'fset:union matches)))))))
    (let* ((coordinates
             (loop :for line :in (uiop:read-file-lines #P"./08.txt")
                   :collect (ppcre:register-groups-bind
                                ((#'parse-integer x y z))
                                ("(\\d+),(\\d+),(\\d+)" line)
                              (list x y z))))
           (distances
             (loop :with results := (fset:empty-map)
                   :for p :in coordinates
                   :do (loop :for q :in coordinates
                             :for pair := (fset:set p q)
                             :unless (or (equal p q)
                                         (fset:lookup results pair))
                               :do (fset:includef results pair (distance p q)))
                   :finally (return (fset:sort (fset:convert 'fset:seq results)
                                               #'<
                                               :key #'cdr)))))
      (if is-part-two
          (loop :with clusters := (fset:empty-set)
                :and total := (length coordinates)
                :for (pair . _) :in (fset:convert 'list distances)
                :for next-clusters := (connect clusters pair)
                :when (and (eql (fset:size next-clusters) 1)
                           (eql (fset:size (fset:least next-clusters)) total))
                  :return (fset:reduce #'* pair :key #'car)
                :when next-clusters :do (setf clusters next-clusters))
          (loop :with clusters := (fset:empty-set)
                :for (pair . _) :in (fset:convert 'list distances)
                :for i :below 1000
                :for next-clusters := (connect clusters pair)
                :when next-clusters
                  :do (setf clusters next-clusters)
                :finally (let ((sizes (sort (mapcar #'fset:size
                                                    (fset:convert 'list
                                                                  clusters))
                                            #'>)))
                           (return (reduce #'* (subseq sizes 0 3)))))))))
