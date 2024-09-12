(defun day03 (is-part-two)
  (labels ((get-positions (instructions)
             (loop :with position := (cons 0 0) :for i :in instructions
                   :do (case i
                         (#\v (incf (cdr position)))
                         (#\^ (decf (cdr position)))
                         (#\> (incf (car position)))
                         (#\< (decf (car position))))
                   :collect (copy-list position) :into positions
                   :finally (return (cons (cons 0 0) positions))))
           (count-distinct (positions)
             (length (remove-duplicates positions :test #'equal))))
    (let ((instructions (coerce (uiop:read-file-string #P"./03.txt") 'list)))
      (count-distinct
       (if is-part-two
           (let ((even-odd-instructions
                   (loop :for (x y) :on instructions :by #'cddr
                         :collect x :into even
                         :collect y :into odd
                         :finally (return (cons even odd)))))
             (concatenate 'list
                          (get-positions (car even-odd-instructions))
                          (get-positions (cdr even-odd-instructions))))
           (get-positions instructions))))))
