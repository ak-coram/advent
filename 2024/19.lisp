(ql:quickload '(:cl-ppcre :fset))

(defun day19 (is-part-two)
  (destructuring-bind (towel-input design-input)
      (ppcre:split "\\n\\n" (uiop:read-file-string #P"./19.txt"))
    (let ((towels (ppcre:split ", " towel-input))
          (designs (ppcre:split "\\n" design-input))
          (cache (fset:map ("" 1))))
      (labels ((starts-with-p (design towel)
                 (loop :with l := (length design)
                       :for c :across towel :for i :from 0
                       :unless (and (< i l) (eql c (aref design i)))
                         :return nil
                       :finally (return t)))
               (combinations (design)
                 (or (fset:lookup cache design)
                     (loop :for towel :in towels
                           :when (starts-with-p design towel)
                             :sum (combinations (subseq design (length towel)))
                               :into count
                           :finally (progn (fset:includef cache design count)
                                           (return count))))))
        (loop :for design :in designs :for c := (combinations design)
              :sum c :into sum :count (plusp c) :into count
              :finally (return (if is-part-two sum count)))))))
