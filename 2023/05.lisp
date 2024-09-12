(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day05 (is-part-two)
  (labels ((parse-integers (s) (mapcar #'parse-integer (ppcre:split "\\s+" s)))
           (merge-ranges (r1 r2)
             (loop :with (d1 s1 l1) := r1 :and (d2 s2 l2) := r2
                   :with de1 := (+ d1 l1 -1) :and se2 := (+ s2 l2 -1)
                   :with rs := (cond
                                 ;; ---
                                 ;; ===
                                 ((and (eql d1 s2) (eql l1 l2))
                                  `((nil ,d2 ,s1 ,l1)))
                                 ;;   ----
                                 ;; ========
                                 ((and (< s2 d1) (< de1 se2))
                                  `((nil ,(+ d2 (- d1 s2)) ,s1 ,l1)))
                                 ;; --------
                                 ;;    ===
                                 ((and (< d1 s2) (< se2 de1))
                                  `((t ,d1 ,s1 ,(- s2 d1))
                                    (nil ,d2 ,(+ s1 (- s2 d1)) ,l2)
                                    (t ,(+ d1 (- s2 d1) l2)
                                       ,(+ s1 (- s2 d1) l2)
                                       ,(+ (- s2 d1) l2))))
                                 ;;    -------
                                 ;; ======
                                 ((<= s2 d1 se2 de1)
                                  (let ((offset (- (+ s2 l2) d1)))
                                    `((nil ,(+ d2 (- d1 s2)) ,s1 ,offset)
                                      (t ,(+ d1 offset)
                                         ,(+ s1 offset)
                                         ,(- l1 offset)))))
                                 ;; -------
                                 ;;     ======
                                 ((<= d1 s2 de1 se2)
                                  (let ((offset (- s2 d1)))
                                    `((t ,d1 ,s1 ,offset)
                                      (nil ,d2
                                           ,(+ s1 offset)
                                           ,(- l1 offset))))))
                   :for r :in rs
                   :for (nil d s l) := r :when (plusp l) :collect r))
           (merge-mappings (r mappings)
             (loop :with old := (list r) :and new := nil
                   :for m :in mappings
                   :do (loop :for i :below (length old)
                             :for o := (nth i old)
                             :for merges := (when o (merge-ranges o m))
                             :when merges
                               :do (progn
                                     (setf (nth i old) nil)
                                     (loop :for (is-old . merge) :in merges
                                           :when merge
                                             :if is-old :do (push merge old)
                                               :else :do (push merge new))))
                   :finally (return (remove-if #'null (append old new))))))
    (let* ((input (loop :with sections
                          := (ppcre:split "\\n\\n+" (uiop:read-file-string "./05.txt"))
                        :for section :in sections
                        :for section-lines := (ppcre:split "\\n" section)
                        :for (section-header maybe-seed-ids)
                          := (ppcre:split ":\\s*" (car section-lines))
                        :collect (ppcre:register-groups-bind (nil from to)
                                     ("(seeds)|(\\w+)-to-(\\w+) map" section-header)
                                   (if maybe-seed-ids
                                       (cons :seeds (parse-integers maybe-seed-ids))
                                       (cons (cons (intern (string-upcase from) :keyword)
                                                   (intern (string-upcase to) :keyword))
                                             (mapcar #'parse-integers (cdr section-lines)))))))
           (seeds (if is-part-two
                      (loop :for (start length) :on (alexandria:assoc-value input
                                                                            :seeds)
                              :by #'cddr :while length
                            :collect (list start start length))
                      (loop :for seed-id :in (alexandria:assoc-value input
                                                                     :seeds)
                            :collect (list seed-id seed-id 1)))))
      (loop :with acc := seeds :and next-acc := nil
            :for map-id :in `((:seed . :soil)
                              (:soil . :fertilizer)
                              (:fertilizer . :water)
                              (:water . :light)
                              (:light . :temperature)
                              (:temperature . :humidity)
                              (:humidity . :location))
            :for m := (alexandria:assoc-value input map-id :test 'equalp)
            :do (loop :for r :in acc
                      :for ranges := (merge-mappings r m)
                      :when ranges
                        :do (setf next-acc (append next-acc ranges))
                      :finally (setf acc next-acc
                                     next-acc nil))
            :finally (return (apply #'min (mapcar #'car acc)))))))
