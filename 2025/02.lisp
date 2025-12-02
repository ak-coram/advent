(ql:quickload :cl-ppcre)

(defun day02 (is-part-two)
  (labels ((repeat (n s) (with-output-to-string (out)
                           (dotimes (_ n) (write-string s out))))
           (invalidp (id)
             (let* ((s (write-to-string id))
                    (l (length s))
                    (center (/ l 2)))
               (loop :for i :from (floor center)
                       :downto (if is-part-two 1 center)
                     :when (string= (repeat (floor l i) (subseq s 0 i)) s)
                       :return t))))
    (loop :for range :in (ppcre:split "," (uiop:read-file-string #P"./02.txt"))
          :sum (ppcre:register-groups-bind
                   ((#'parse-integer from to)) ("(\\d+)-(\\d+)" range)
                 (loop :for id :from from :upto to
                       :when (invalidp id) :sum id)))))
