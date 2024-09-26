(ql:quickload :esrap)
(use-package :esrap)

(defrule unquoted-character (alphanumericp character))

(defrule quoted-backslash (and #\\ #\\)
  (:constant #\\))

(defrule quoted-double-quote (and #\\ #\")
  (:constant #\"))

(defrule quoted-character (and #\\ #\x
                               unquoted-character
                               unquoted-character)
  (:destructure (_1 _2 x y)
    (declare (ignore _1 _2))
    (parse-integer (format nil "~c~c" x y) :radix 16)))

(defrule string (and #\"
                     (* (or quoted-backslash
                            quoted-double-quote
                            quoted-character
                            unquoted-character))
                     #\")
  (:destructure (_1 contents _2)
    (declare (ignore _1 _2))
    contents))

(defun day08 (is-part-two)
  (labels ((decode (s) (parse 'string s))
           (double-encode (characters)
             (format nil "\"\\\"~{~A~}\\\"\""
                     (loop :for c :in characters
                           :collect (cond
                                      ((eql c #\\) "\\\\\\\\")
                                      ((eql c #\") "\\\\\\\"")
                                      ((integerp c)
                                       (format nil "\\\\x~(~2,'0x~)" c))
                                      (t c))))))
    (loop :for line :in (uiop:read-file-lines #P"./08.txt")
          :for decoded-string := (decode line)
          :sum (if is-part-two
                   (- (length (double-encode decoded-string)) (length line))
                   (- (length line) (length decoded-string))))))
