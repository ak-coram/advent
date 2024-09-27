(defun day11 (is-part-two)
  (labels ((decode (s)
             (loop :for i :from 7 :downto 0
                   :for power := (expt 26 i)
                   :for v :in (mapcar #'char-code (coerce s 'list))
                   :sum (* (- v 97) power)))
           (encode (n)
             (format nil "~{~c~}"
                     (loop :with acc := n
                           :for i :from 7 :downto 0
                           :for power := (expt 26 i)
                           :for d := (floor acc power)
                           :do (setf acc (- acc (* d power)))
                           :collect (code-char (+ d 97)))))
           (validp (s)
             (and (loop :for c :across s
                        :when (case c ((#\i #\o #\l) t))
                          :return nil
                        :finally (return t))
                  (loop :for i :from 1 :below 7
                        :for c1 :across s
                        :for c2 := (aref s i)
                        :for c3 := (aref s (1+ i))
                        :when (= (+ 2 (char-code c1))
                                 (1+ (char-code c2))
                                 (char-code c3))
                          :return t)
                  (loop :with prev := nil
                        :for c :across s
                        :for i :from 0
                        :for is-match := (eql prev c)
                        :when is-match
                          :collect i :into results
                        :do (setf prev (unless is-match c))
                        :finally (return (< 1 (length results))))))
           (find-next (s)
             (loop :for p :from (1+ (decode s))
                   :for s := (encode p)
                   :when (validp s)
                     :return s)))
    (let ((next (find-next (uiop:read-file-line #P"./11.txt"))))
      (if is-part-two
          (find-next next)
          next))))
