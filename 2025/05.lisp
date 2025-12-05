(ql:quickload '(:cl-ppcre))

(defun day05 (is-part-two)
  (let* ((inputs (ppcre:split "\\n\\n" (uiop:read-file-string #P"./05.txt")))
         (ids (loop :for line :in (ppcre:split "\\n" (cadr inputs))
                    :collect (parse-integer line)))
         (ranges (loop :for line :in (ppcre:split "\\n" (car inputs))
                       :collect (ppcre:register-groups-bind
                                    ((#'parse-integer from to))
                                    ("(\\d+)-(\\d+)" line)
                                  (if (< to from)
                                      (cons to from) (cons from to)))
                         :into results
                       :finally (return (sort results #'< :key #'car))))
         (merged-ranges
           (loop :with result = (list (car ranges))
                 :for range :in (cdr ranges)
                 :for previous := (car result)
                 :if (<= (car range) (1+ (cdr previous)))
                   :do (setf (cdr previous) (max (cdr previous) (cdr range)))
                 :else
                   :do (push range result)
                 :finally (return (nreverse result)))))
    (if is-part-two
        (loop :for (from . to) :in merged-ranges :sum (1+ (- to from)))
        (loop :for id :in ids
              :count (loop :for (from . to) :in merged-ranges
                           :when (<= from id to) :return t)))))
