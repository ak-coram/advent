(ql:quickload :cl-ppcre)

(defun day12 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./12.txt"))
         (rows (labels ((unfold (separator xs)
                          (apply #'concatenate 'string
                                 (butlast (mapcan (lambda (x)
                                                    (list x separator))
                                                  (loop :repeat 5
                                                        :collect xs))))))
                 (loop :for line :in lines
                       :for (springs groups) := (ppcre:split " " line)
                       :for unfolded-springs := (if is-part-two
                                                    (unfold "?" springs)
                                                    springs)
                       :for unfolded-groups := (if is-part-two
                                                   (unfold "," groups)
                                                   groups)
                       :collect
                       (cons (ppcre:regex-replace-all
                              "\\.+" (format nil "~A." unfolded-springs) ".")
                             (mapcar #'parse-integer
                                     (ppcre:split "," unfolded-groups))))))
         (cache (make-hash-table :test 'equal)))
    (labels ((valid-subseqs (springs length)
               (loop :with n := 0 :and first-broken-index := nil
                     :for c :across springs :for i :from 0
                     :for next := (ignore-errors (char springs (1+ i)))
                     :do (ecase c
                           (#\. (setf n 0))
                           (#\# (unless first-broken-index
                                  (setf first-broken-index i))
                            (incf n))
                           (#\? (incf n)))
                     :while (or (not first-broken-index)
                                (< (- i length) first-broken-index))
                     :when (and (<= length n) (member next '(#\. #\?)))
                       :collect (subseq springs
                                        (+ i 1 (if (char= next #\?) 1 0)))
                         :into results
                     :finally (return (setf (gethash (cons springs length)
                                                     cache)
                                            results))))
             (combinations (springs groups)
               (multiple-value-bind (c is-hit) (gethash (cons springs groups)
                                                        cache)
                 (if is-hit
                     c
                     (setf (gethash (cons springs groups) cache)
                           (if (cdr groups)
                               (loop :for v :in (valid-subseqs springs
                                                               (car groups))
                                     :sum (combinations v (cdr groups)))
                               (loop :for v :in (valid-subseqs springs
                                                               (car groups))
                                     :count (not (find #\# v)))))))))
      (loop :for (springs . groups) :in rows
            :sum (combinations springs groups)))))
