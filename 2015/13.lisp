(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

(defun day13 (is-part-two)
  (let* ((regexp "(\\w+) would (lose|gain) (\\d+) happiness units by sitting next to (\\w+)")
         (preferences
           (loop
             :with acc := nil
             :for line :in (uiop:read-file-lines #P"./13.txt")
             :do (ppcre:register-groups-bind (a sign happiness b) (regexp line)
                   (when (and a sign happiness b)
                     (let ((who (intern a)))
                       (setf (alexandria:assoc-value acc who)
                             (cons (cons (intern b)
                                         (funcall (if (string= sign "gain")
                                                      #'+
                                                      #'-)
                                                  (parse-integer happiness)))
                                   (alexandria:assoc-value acc who))))))
             :finally (return (if is-part-two (cons '(me) acc) acc))))
         (max-happiness nil)
         (seats (mapcar #'car preferences))
         (origin (car seats)))
    (labels ((lookup-happiness (a b)
               (or (alexandria:assoc-value
                    (alexandria:assoc-value preferences a)
                    b)
                   0)))
      (alexandria:map-permutations
       (lambda (path)
         (loop :for (a maybe-b) :on (cons origin path)
               :for b := (or maybe-b origin)
               :for a-happiness := (lookup-happiness a b)
               :for b-happiness := (lookup-happiness b a)
               :sum (+ a-happiness b-happiness) :into total-happiness
               :finally (when (or (not max-happiness)
                                  (> total-happiness max-happiness))
                          (setf max-happiness total-happiness))))
       (cdr seats)))
    max-happiness))
