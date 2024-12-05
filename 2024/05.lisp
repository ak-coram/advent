(ql:quickload '(:fset :cl-ppcre))

(defun day05 (is-part-two)
  (destructuring-bind (rule-input order-input)
      (ppcre:split "\\n\\n" (uiop:read-file-string #P"./05.txt"))
    (let ((rules (fset:empty-set))
          (manuals
            (loop :for line :in (uiop:split-string order-input
                                                   :separator '(#\Newline))
                  :for pages := (mapcar #'parse-integer
                                        (uiop:split-string line
                                                           :separator '(#\,)))
                  :when pages :collect pages)))
      (ppcre:do-register-groups ((#'parse-integer x y))
          ("(\\d+)\\|(\\d+)" rule-input)
        (setf rules (fset:with rules (cons x y))))
      (labels ((out-of-order-p (pages)
                 (loop :named outer
                       :for (current-page . subsequent-pages) :on pages
                       :do (loop :for subsequent-page :in subsequent-pages
                                 :when (fset:lookup rules (cons subsequent-page
                                                                current-page))
                                   :do (return-from outer t))))
               (get-middle-page-number (pages)
                 (nth (floor (length pages) 2) pages)))
        (if is-part-two
            (loop :for pages :in manuals
                  :when (out-of-order-p pages)
                    :sum (get-middle-page-number
                          (sort pages (lambda (x y)
                                        (fset:lookup rules (cons x y))))))
            (loop :for pages :in manuals
                  :unless (out-of-order-p pages)
                    :sum (get-middle-page-number pages)))))))
