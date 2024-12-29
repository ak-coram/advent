(ql:quickload :fset)

(defun day21 (is-part-two)
  (labels
      ((get-paths (pad)
         (let ((paths (fset:empty-map (fset:empty-set))))
           (labels
               ((recur (origin goal current &optional
                                              (path (fset:empty-seq))
                                              (visited (fset:set current)))
                  (loop :with key := (cons origin goal)
                        :for (direction . next) :in (cdr (assoc current pad))
                        :for prev-paths := (fset:lookup paths key)
                        :when (equal current goal)
                          :do (fset:includef paths key
                                             (fset:with prev-paths path))
                        :unless (fset:member? next visited)
                          :do (recur origin goal next
                                     (fset:with-last path direction)
                                     (fset:with visited next)))))
             (loop :for (origin . _) :in pad
                   :do (loop :for (goal . _) :in pad
                             :do (recur origin goal origin))))
           (fset:image (lambda (key paths)
                         (let ((l (fset:least (fset:image #'fset:size paths))))
                           (values key (fset:filter (lambda (path)
                                                      (eql (fset:size path) l))
                                                    paths))))
                       paths)))
       (parse-output (output)
         (loop :with result := (fset:empty-seq)
               :for c :across output
               :do (fset:push-last result
                                   (or (digit-char-p c)
                                       (intern (format nil "~:@(~c~)" c))))
               :finally (return (cons (parse-integer output :junk-allowed t)
                                      result)))))
    (let* ((numpad (get-paths `((7         (> . 8) (v . 4)        )
                                (8 (< . 7) (> . 9) (v . 5)        )
                                (9 (< . 8)         (v . 6)        )
                                (4         (> . 5) (v . 1) (^ . 7))
                                (5 (< . 4) (> . 6) (v . 2) (^ . 8))
                                (6 (< . 5)         (v . 3) (^ . 9))
                                (1         (> . 2)         (^ . 4))
                                (2 (< . 1) (> . 3) (v . 0) (^ . 5))
                                (3 (< . 2)         (v . A) (^ . 6))
                                (0         (> . A)         (^ . 2))
                                (A (< . 0)                 (^ . 3)))))
           (dirpad (get-paths `((^         (> . A) (v . v)        )
                                (A (< . ^)         (v . >)        )
                                (<         (> . v)                )
                                (v (< . <) (> . >)         (^ . ^))
                                (> (< . v)                 (^ . A)))))
           (all-paths (fset:map-union numpad dirpad))
           (cache (fset:empty-map)))
      (macrolet ((with-cache (key &rest body)
                   `(or (fset:lookup cache ,key)
                        (let ((result (progn ,@body)))
                          (fset:includef cache ,key result)
                          result))))
        (labels
            ((count-keys (n output)
               (with-cache (cons n output)
                 (if (zerop n)
                     (fset:size output)
                     (loop :for (current . (next))
                             :on (cons 'A (fset:convert 'list output))
                           :while next
                           :for alternatives
                             := (fset:lookup all-paths (cons current next))
                           :sum (fset:least
                                 (fset:image (lambda (path)
                                               (count-keys (1- n)
                                                           (fset:with-last path
                                                             'A)))
                                             alternatives)))))))
          (loop :for line :in (uiop:read-file-lines #P"./21.txt")
                :for (x . output) := (parse-output line)
                :sum (* x (count-keys (if is-part-two 26 3) output))))))))
