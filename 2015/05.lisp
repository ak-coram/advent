(defun day05 (is-part-two)
  (if is-part-two
      (labels ((has-pair-p (s)
                 (loop :with previous := nil :and l := (length s)
                       :for c :across s :for i :from 0
                       :when (search (list previous c) s
                                     :start2 (min  (1+ i) l))
                         :return t
                       :do (setf previous c)))
               (has-sibling-p (s)
                 (loop :with c1 := nil :and c2 := nil :for c3 :across s
                       :when (and c1 (char= c1 c3))
                         :return t
                       :do (setf c1 c2 c2 c3))))
        (let ((strings (uiop:read-file-lines #P"./05.txt")))
          (count-if (lambda (s) (and (has-pair-p s) (has-sibling-p s)))
                    strings)))
      (labels ((has-three-vowels-p (s)
                 (<= 3 (count-if (lambda (c) (member c '(#\a #\e #\i #\o #\u)))
                                 s)))
               (has-twice-in-a-row-p (s)
                 (loop :with previous := nil :for c :across s
                       :when (and previous (char= previous c))
                         :return t
                       :do (setf previous c)))
               (has-forbidden-p (s)
                 (some (lambda (x) (search x s)) '("ab" "cd" "pq" "xy"))))
        (let ((strings (uiop:read-file-lines #P"./05.txt")))
          (count-if (lambda (s) (and (has-three-vowels-p s)
                                     (has-twice-in-a-row-p s)
                                     (not (has-forbidden-p s))))
                    strings)))))
