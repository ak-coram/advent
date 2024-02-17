(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day15 (is-part-two)
  (labels ((hash (s)
             (loop :with n := 0
                   :for c :across s
                   :do (setf n (rem (* 17 (+ n (char-code c))) 256))
                   :finally (return n)))
           (lens= (l1 l2) (string= (car l1) (car l2))))
    (let* ((input (uiop:read-file-line #P"./15.txt"))
           (instructions
             (loop :for s :in (ppcre:split "," input)
                   :collect
                   (ppcre:register-groups-bind (label op focal-length)
                       ("(\\w+)([-|=])(\\d*)" s)
                     (list (char op 0)
                           label
                           (hash label)
                           (parse-integer focal-length :junk-allowed t))))))
      (if is-part-two
          (loop :with boxes := (make-array '(256) :initial-element nil)
                :for (op label box-index focal-length) :in instructions
                :for op-lens := (cons label focal-length)
                :for box-contents := (aref boxes box-index)
                :if (char= op #\-)
                  :do (setf (aref boxes box-index)
                            (remove op-lens box-contents :test #'lens=))
                :else :do
                  (alexandria:if-let (i (position op-lens box-contents
                                                  :test #'lens=))
                    (setf (nth i (aref boxes box-index)) op-lens)
                    (setf (aref boxes box-index)
                          (cons op-lens box-contents)))
                :finally
                   (return (loop :for lenses :across boxes
                                 :for x :from 1
                                 :sum (loop :for (_ . focal-length)
                                              :in (reverse lenses)
                                            :for y :from 1
                                            :sum (* x y focal-length)))))
          (loop :for step :in (ppcre:split "," input) :sum (hash step))))))
