(ql:quickload :alexandria)
(ql:quickload :esrap)
(use-package :esrap)

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

(defun day19 ()
  (let* ((input (parse 'input (uiop:read-file-string #P"./19.txt")))
         (accept-part-p
           (compile
            nil `(lambda (part)
                   (destructuring-bind (x m a s) part
                     (declare (type fixnum x m a s))
                     (let ((result))
                       (tagbody
                          (go |in|)
                          ,@(loop :with wf := nil
                                  :for (name . rules) :in (car input)
                                  :do (push name wf)
                                  :do (loop :for (condition target) :in rules
                                            :do (push `(when ,condition
                                                         (go ,target))
                                                      wf))
                                  :finally (return (nreverse wf)))
                        A (setf result t)
                        R nil)
                       result))))))
    (loop :for part :in (cadr input)
          :when (funcall accept-part-p part)
            :sum (loop :for rating :in part :sum rating))))
