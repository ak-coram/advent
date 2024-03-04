(ql:quickload :alexandria)
(ql:quickload :esrap) (use-package :esrap)
(ql:quickload :let-plus) (use-package :let-plus)

(defrule integer (+ (digit-char-p character))
  (:lambda (digits) (parse-integer (text digits))))

(defrule end-of-line (or (and #\linefeed #\return)
                         (and #\return #\linefeed)
                         #\linefeed
                         #\return)
  (:constant nil))

(defrule word (+ (alphanumericp character))
  (:text t))

(defrule category (or #\x #\m #\a #\s)
  (:lambda (s) (intern (string-upcase s))))

(defrule relation-rule (and category (or #\< #\>) integer #\: word (? #\,))
  (:destructure (category relation x colon target comma)
    (declare (ignore colon comma))
    (list (list (intern relation) category x) (intern target))))

(defrule fixed-rule word
  (:lambda (w) (list t (intern w))))

(defrule workflow-rule (or relation-rule fixed-rule))

(defrule workflow (and word #\{ (+ workflow-rule) #\} (? end-of-line))
  (:destructure (name open-brace workflow-rules close-brace eol)
    (declare (ignore open-brace close-brace eol))
    (cons (intern name) workflow-rules)))

(defrule rating (and category #\= integer (? #\,))
  (:destructure (category equals value colon)
    (declare (ignore category equals colon))
    value))

(defrule part (and #\{ (+ rating) #\} (? end-of-line))
  (:function cadr))

(defrule input (and (+ workflow) end-of-line (+ part))
  (:destructure (workflows eol parts)
    (declare (ignore eol))
    (list workflows parts)))

(defun day19 (is-part-two)
  (if is-part-two
      (loop
        :with input := (parse 'input (uiop:read-file-string #P"./19.txt"))
        :with get-accepted-ranges :=
        `(lambda (ranges)
           (let ((branches (list ranges)) results current)
             (labels ((substitute-at (new n sequence)
                        (substitute-if new #'identity sequence :start n :count 1))
                      (split-range (relation range v)
                        (let ((from (car range)) (to (cdr range)))
                          (case relation
                            (< (cond ((< to v) (values range nil))
                                     ((>= from v) (values nil range))
                                     (t (values (cons from (1- v)) (cons v to)))))
                            (> (cond ((> from v) (values range nil))
                                     ((<= to v) (values nil range))
                                     (t (values (cons (1+ v) to) (cons from v))))))))
                      (branch (condition)
                        (let+ (((relation category v) condition)
                               (i (case category (x 0) (m 1) (a 2) (s 3)))
                               ((&values matching-range non-matching-range)
                                (split-range relation (nth i current) v))
                               (match (substitute-at matching-range i current))
                               (no-match (substitute-at non-matching-range i current)))
                          (cond ((and matching-range non-matching-range)
                                 (push match branches)
                                 (setf current no-match)
                                 nil)
                                (matching-range (setf current match) t)
                                (t (setf current no-match) nil)))))
               (tagbody
                start (alexandria:when-let (branch (pop branches))
                        (setf current branch)
                        (go |in|))
                  (go end)
                  ,@(loop :with wf := nil
                          :for (name . rules) :in (car input)
                          :do (push name wf)
                          :do (loop :for (condition target) :in rules
                                    :do (push (if (eql condition 't)
                                                  `(go ,target)
                                                  `(when (branch ',condition)
                                                     (go ,target)))
                                              wf))
                          :finally (return (nreverse wf)))
                A (push current results)
                R (go start)
                end nil)
               results)))
        :for ranges :in (funcall (compile nil get-accepted-ranges)
                                 (loop :repeat 4 :collect (cons 1 4000)))
        :sum (loop :with product := 1
                   :for (from . to) :in ranges
                   :do (setf product (* product (1+ (- to from))))
                   :finally (return product)))
      (loop :with input := (parse 'input (uiop:read-file-string #P"./19.txt"))
            :with accept-part-p
              := (compile
                  nil
                  `(lambda (part)
                     (destructuring-bind (x m a s) part
                       (declare (type fixnum x m a s))
                       (let ((result))
                         (tagbody
                            (go |in|)
                            ,@(loop :with wf := nil
                                    :for (name . rules) :in (car input)
                                    :do (push name wf)
                                    :do (loop :for (condition target)
                                                :in rules
                                              :do (push `(when ,condition
                                                           (go ,target))
                                                        wf))
                                    :finally (return (nreverse wf)))
                          A (setf result t)
                          R nil)
                         result))))
            :for part :in (cadr input)
            :when (funcall accept-part-p part)
              :sum (loop :for rating :in part :sum rating))))
