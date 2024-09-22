(ql:quickload :alexandria)
(ql:quickload :esrap)
(use-package :esrap)

(defrule signal (+ (digit-char-p character))
  (:lambda (digits) (parse-integer (text digits))))

(defrule unary-operator "NOT" (:constant 'not))

(defrule binary-operator (or "AND" "OR" "LSHIFT" "RSHIFT")
  (:lambda (op) (intern (text op))))

(defrule wire (+ (alphanumericp character))
  (:lambda (w) (intern (text w))))

(defrule fixed-signal (and signal " -> " wire)
  (:destructure (s _ w) (declare (ignore _)) (list 'fix w s)))

(defrule negation (and unary-operator #\space wire " -> " wire)
  (:destructure (_1 _2 s _3 d) (declare (ignore _1 _2 _3)) (list 'not d s)))

(defrule operation (and (or signal wire)
                        #\space binary-operator #\space
                        (or signal wire) " -> " wire)
  (:destructure (s1 _1 op _2 s2 _3 d)
    (declare (ignore _1 _2 _3))
    (list op d s1 s2)))

(defrule assignment (and wire " -> " wire)
  (:destructure (s _ d) (declare (ignore _)) (list 'assign d s)))

(defrule instruction (or fixed-signal negation operation assignment))

(defconstant +max-value+ (1- (expt 2 16)))

(defun day07 (is-part-two)
  (labels ((iterate (state instructions)
             (loop :for instruction :in instructions
                   :for (op d s1 s2) := instruction
                   :unless (gethash d state)
                     :do (ecase op
                           (fix (setf (gethash d state) s1))
                           (assign (alexandria:when-let (v (gethash s1 state))
                                     (setf (gethash d state) v)))
                           (not (alexandria:when-let (v (gethash s1 state))
                                  (setf (gethash d state) (- +max-value+ v))))
                           ((and or)
                            (alexandria:when-let
                                ((a (if (integerp s1) s1 (gethash s1 state)))
                                 (b (if (integerp s2) s2 (gethash s2 state))))
                              (setf (gethash d state)
                                    (ecase op
                                      (and (logand a b))
                                      (or (logior a b))))))
                           ((rshift lshift)
                            (alexandria:when-let (v (gethash s1 state))
                              (setf (gethash d state)
                                    (mod (ash v (ecase op
                                                  (rshift (- s2))
                                                  (lshift s2)))
                                         +max-value+)))))
                     :and :collect instruction))
           (solve (instructions)
             (loop :with state := (make-hash-table)
                   :do (setf instructions (iterate state instructions))
                   :while instructions
                   :finally (return (gethash (intern "a") state)))))
    (let* ((instructions (loop :for line :in (uiop:read-file-lines #P"./07.txt")
                               :collect (parse 'instruction line)))
           (part-one-solution (solve instructions)))
      (if is-part-two
          (solve (cons (list 'fix (intern "b") part-one-solution) instructions))
          part-one-solution))))
