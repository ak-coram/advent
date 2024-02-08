(ql:quickload :cl-ppcre)

(defun day12 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./12.txt"))
         (rows (loop :for line :in lines
                     :for (springs groups) := (ppcre:split " " line)
                     :collect (cons springs
                                    (mapcar #'parse-integer
                                            (ppcre:split "," groups))))))
    (labels ((replace-char (s i c)
               (let ((s2 (copy-seq s)))
                 (setf (char s2 i) c)
                 s2))
             (variations (springs)
               (loop :with variations := (list springs)
                     :for c :across springs :for i :from 0
                     :when (char= c #\?)
                       :do (setf variations
                                 (loop :for v :in variations
                                       :collect (replace-char v i #\.)
                                       :collect (replace-char v i #\#)))
                     :finally (return variations)))
             (is-valid (variation groups)
               (equal groups
                      (loop :with n := 0 :and results := nil
                            :for c :across variation
                            :if (char= c #\#)
                              :do (incf n)
                            :else :when (plusp n)
                                    :do (push n results) :and :do (setf n 0)
                            :finally (return (nreverse (if (plusp n)
                                                           (cons n results)
                                                           results)))))))
      (if is-part-two
          nil
          (loop :for (springs . groups) :in rows
                :sum (count-if (lambda (v) (is-valid v groups))
                               (variations springs)))))))
