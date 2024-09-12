(ql:quickload :cl-ppcre)

(defun day06 (is-part-two)
  (labels ((parse1 (s) (mapcar #'parse-integer
                               (ppcre:split "\\s+" (cadr (ppcre:split ":\\s*" s)))))
           (parse2 (s)
             (list (parse-integer (ppcre:regex-replace-all "\\s+"
                                                           (cadr (ppcre:split ":\\s*" s))
                                                           "")))))
    (loop :with acc := 1
          :with (times-line distances-line) := (uiop:read-file-lines "./06.txt")
          :with times := (funcall (if is-part-two #'parse2 #'parse1) times-line)
          :and distances := (funcall (if is-part-two #'parse2 #'parse1) distances-line)
          :for time :in times :for distance :in distances
          :do (setf acc (* acc (loop :for i :from 1 :below time
                                     :for r := (- time i)
                                     :for d := (* r i)
                                     :count (< distance d))))
          :finally (return acc))))
