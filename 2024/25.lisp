(ql:quickload '(:cl-ppcre :fset))

(defun day25 ()
  (let ((locks (fset:empty-set)) (keys (fset:empty-set)))
    (labels ((count-height (i lines)
               (1- (loop :for line :in lines
                         :count (char= (char line i) #\#))))
             (parse (lines)
               (loop :with device := (fset:empty-bag)
                     :for c :across (car lines) :for i :from 0
                     :do (fset:includef device i (count-height i lines))
                     :finally (return device)))
             (fitp (lock key)
               (> 6 (fset:greatest
                     (fset:range
                      (fset:convert 'fset:map (fset:bag-sum lock key)))))))
      (loop :for device :in (ppcre:split "\\n\\n"
                                         (uiop:read-file-string #P"./25.txt"))
            :for lines := (ppcre:split "\\n" device)
            :for is-lock := (null (find #\. (car lines)))
            :if is-lock
              :do (fset:includef locks (parse lines))
            :else :do (fset:includef keys (parse lines)))
      (let ((count 0))
        (fset:do-set (lock locks count)
          (fset:do-set (key keys)
            (when (fitp lock key) (incf count))))))))
