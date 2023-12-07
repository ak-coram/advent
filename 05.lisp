(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(labels ((parse-integers (s) (mapcar #'parse-integer (ppcre:split "\\s+" s))))
  (let ((input (loop :with sections
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
                                          (mapcar #'parse-integers (cdr section-lines))))))))
    (labels ((resolve-mapping (map-id id)
               (loop :with m := (alexandria:assoc-value input map-id :test 'equalp)
                     :for (destination-start source-start length) :in m
                     :when (<= source-start id (+ source-start length -1))
                       :return (+ destination-start (- id source-start))
                     :finally (return id))))
      (loop :for id :in (alexandria:assoc-value input :seeds)
            :minimize (loop :with current-id := id
                            :for map-id :in `((:seed . :soil)
                                              (:soil . :fertilizer)
                                              (:fertilizer . :water)
                                              (:water . :light)
                                              (:light . :temperature)
                                              (:temperature . :humidity)
                                              (:humidity . :location))
                            :do (setf current-id (resolve-mapping map-id current-id))
                            :finally (return current-id))))))
