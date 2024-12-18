(ql:quickload :cl-ppcre)

(defun day17 (is-part-two)
  (let* ((input (ppcre:split "\\n\\n" (uiop:read-file-string #P"./17.txt")))
         (program (mapcar #'parse-integer
                          (cdr (ppcre:split " |," (cadr input)))))
         (instructions (loop :for (i . (op)) :on program :by #'cddr
                             :collect (cons (ecase i
                                              (0 :adv) (1 :bxl) (2 :bst)
                                              (3 :jnz) (4 :bxc) (5 :out)
                                              (6 :bdv) (7 :cdv))
                                            op)))
         (registers))
    (ppcre:do-register-groups (register (#'parse-integer value))
        ("Register (\\w): (\\d+)" (car input))
      (when register (push (cons (intern register :keyword) value) registers)))
    (labels
        ((reg (r) (cdr (assoc r registers)))
         (combo (op) (ecase op ((0 1 2 3) op) (4 'a) (5 'b) (6 'c)))
         (build ()
           `(lambda (a b c)
              (let ((results))
                (tagbody
                   ,@(loop :for n :from 0
                           :for (i . op) :in instructions
                           :collect n
                           :collect
                           (ecase i
                             (:adv `(setf a (truncate a (expt 2 ,(combo op)))))
                             (:bxl `(setf b (logxor b ,op)))
                             (:bst `(setf b (mod ,(combo op) 8)))
                             (:jnz `(unless (zerop a) (go ,(floor op 2))))
                             (:bxc `(setf b (logxor b c)))
                             (:out `(push (mod ,(combo op) 8) results))
                             (:bdv `(setf b (truncate a (expt 2 ,(combo op)))))
                             (:cdv `(setf c (truncate a (expt 2 ,(combo op))))))))
                (nreverse results)))))
      (let ((f (compile nil (build))))
        (if is-part-two
            (loop :with a := 0 :and program-length := (length program)
                  :for result := (funcall f a (reg :b) (reg :c))
                  :when (equal result program)
                    :return a
                  :if (equal result (nthcdr (- program-length (length result))
                                            program))
                    :do (setf a (* a 8))
                  :else :do (incf a))
            (format nil "~{~d~^,~}" (apply f (mapcar #'reg '(:a :b :c)))))))))
