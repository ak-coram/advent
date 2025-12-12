(ql:quickload :cl-ppcre)

(defun day12 ()
  (labels ((count-size (s) (loop :for c :across s :count (char= c #\#))))
    (let* ((input (uiop:read-file-string #P"./12.txt"))
           (chunks (ppcre:split "\\n\\n" input))
           (present-sizes (loop :for s :in (butlast chunks)
                                :for i := (parse-integer s :junk-allowed t)
                                :collect (cons i (count-size s))))
           (regions
             (loop :with results := nil
                   :for line :in (ppcre:split "\\n" (car (last chunks)))
                   :do (ppcre:register-groups-bind
                           ((#'parse-integer w h) ids)
                           ("(\\d+)x(\\d+): (.*)" line)
                         (push (list (cons w h)
                                     (loop :for i :from 0
                                           :for n :in (ppcre:split " " ids)
                                           :collect (cons i (parse-integer n))))
                               results))
                   :finally (return results))))
      (loop :for ((w . h) needed-presents) :in regions
            :for total-area := (* w h)
            :count (loop :for (id . n) :in needed-presents
                         :for (_ . size) := (assoc id present-sizes)
                         :sum n :into count :sum (* n size) :into area
                         :finally (return (cond
                                            ((<= (* count 3 3) total-area) t)
                                            ((> area total-area) nil)
                                            (t (error "oh no!")))))))))
