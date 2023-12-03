(ql:quickload :cl-ppcre)

(defun day03 ()
  (loop :with lines := (uiop:read-file-lines "03.txt")
        :and integer-scanner
               := (ppcre:create-scanner
                   `(:register (:greedy-repetition 1 nil :digit-class)))
        :and sum := 0
        :for line :in lines :for line-index :from 0
        :do (ppcre:do-scans (start end reg-starts reg-ends integer-scanner line)
              (loop :with i-start := (aref reg-starts 0)
                    :and i-end := (aref reg-ends 0)
                    :with i := (parse-integer (subseq line i-start i-end))
                    :for position :from i-start :below i-end
                    :when (loop :for (x . y) :in '((1 . 0)   ; e
                                                   (0 . 1)   ; s
                                                   (-1 . 0)  ; w
                                                   (0 . -1)  ; n
                                                   (-1 . 1)  ; sw
                                                   (1 . 1)   ; se
                                                   (1 . -1)  ; ne
                                                   (-1 . -1) ; nw
                                                   )
                                :for l := (if (zerop y)
                                              line
                                              (ignore-errors
                                               (nth (+ line-index y) lines)))
                                :for c := (ignore-errors (aref l (+ position x)))
                                :when (and c (not (eql c #\.)) (not (digit-char-p c)))
                                  :return t)
                      :do (progn (setf sum (+ sum i)) (return))))
        :finally (return sum)))
