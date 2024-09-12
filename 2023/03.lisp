(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun day03 (is-part-two)
  (let* ((rows (uiop:read-file-lines "03.txt"))
         (integers
           (loop :with results := nil
                 :and integer-scanner
                        := (ppcre:create-scanner
                            `(:register (:greedy-repetition 1 nil :digit-class)))
                 :for row :in rows :for row-index :from 0
                 :do (ppcre:do-scans (start end
                                      reg-starts reg-ends
                                      integer-scanner row)
                       (loop :with i-start := (aref reg-starts 0)
                             :and i-end := (aref reg-ends 0)
                             :with i := (parse-integer (subseq row i-start i-end))
                             :for column-index :from i-start :below i-end
                             :do (push `((,row-index . ,column-index) . ,i)
                                       results)))
                 :finally (return results))))
    (labels ((get-adjacent-integers (row-index column-index)
               (loop :for (x . y) :in '((1 . 0)   ; e
                                        (0 . 1)   ; s
                                        (-1 . 0)  ; w
                                        (0 . -1)  ; n
                                        (-1 . 1)  ; sw
                                        (1 . 1)   ; se
                                        (1 . -1)  ; ne
                                        (-1 . -1) ; nw
                                        )
                     :for i := (alexandria:assoc-value integers
                                                       (cons (+ row-index x)
                                                             (+ column-index y))
                                                       :test 'equalp)
                     :when (and i (not (find i results :test 'eq)))
                       :collect i :into results :finally (return results))))
      (loop :for row :in rows :for row-index :from 0
            :sum (loop :for c :across row :for column-index :from 0
                       :when (or (and is-part-two (eql c #\*))
                                 (not (or (eql c #\.)
                                          (digit-char-p c))))
                         :sum (let ((xs (get-adjacent-integers
                                         row-index
                                         column-index)))
                                (cond
                                  ((not is-part-two) (apply #'+ xs))
                                  ((eql (length xs) 2) (apply #'* xs))
                                  (t 0))))))))
