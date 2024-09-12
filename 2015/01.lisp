(defun day01 (is-part-two)
  (let ((directions (uiop:read-file-string #P"./01.txt"))
        (move (lambda (c) (cond ((char= c #\() 1)
                                ((char= c #\)) -1)
                                (t 0)))))
    (if is-part-two
        (loop :for c :across directions :for i :from 1
              :sum (funcall move c) :into floor-number
              :when (minusp floor-number) :return i)
        (loop :for c :across directions :sum (funcall move c)))))
