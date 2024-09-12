(ql:quickload :queues.priority-queue)

(defun day17 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./17.txt"))
         (width (length (car lines))) (height (length lines))
         (min-steps (if is-part-two 4 0))
         (max-steps (if is-part-two 10 3)))
    (labels ((gethash-infinity (key ht) (gethash key ht most-positive-fixnum))
             (neighbours (pos)
               (destructuring-bind (x y d steps) pos
                 (loop :for (x y direction opposite-direction)
                         :in `((,(1- x) ,y :left :right)
                               (,(1+ x) ,y :right :left)
                               (,x ,(1- y) :up :down)
                               (,x ,(1+ y) :down :up))
                       :for is-same-direction := (eql d direction)
                       :when (and (not (eql d opposite-direction))
                                  (< -1 x width) (< -1 y height)
                                  (if (or (null d) is-same-direction)
                                      (< steps max-steps)
                                      (<= min-steps steps)))
                         :collect (list x y direction (if is-same-direction
                                                          (1+ steps)
                                                          1)))))
             (weight (pos) (digit-char-p (char (nth (cadr pos) lines)
                                               (car pos))))
             (min-heat-loss (start goal)
               (let* ((from (append start (list nil 0)))
                      (dist (make-hash-table :test 'equal))
                      (prev (make-hash-table :test 'equal))
                      (q (queues:make-queue
                          :priority-queue
                          :compare (lambda (a b)
                                     (< (gethash-infinity a dist)
                                        (gethash-infinity b dist))))))
                 (setf (gethash from dist) 0)
                 (queues:qpush q from)
                 (loop :for u := (queues:qpop q) :while u
                       :do (loop :for n :in (neighbours u)
                                 :for alt := (+ (gethash-infinity u dist)
                                                (weight n))
                                 :when (< alt (gethash-infinity n dist))
                                   :do (progn (setf (gethash n dist) alt
                                                    (gethash n prev) u)
                                              (queues:qpush q n))))
                 (loop :for k :being :the :hash-keys :of prev
                       :for (x y nil nil) := k
                       :when (equal (list x y) goal)
                         :minimize (gethash k dist)))))
      (min-heat-loss '(0 0) (list (1- width) (1- height))))))

