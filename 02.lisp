(ql:quickload :alexandria)
(ql:quickload :esrap)
(use-package :esrap)

(defrule digit (digit-char-p character))

(defrule integer (and (+ digit))
  (:destructure (digits)
    (parse-integer (text digits))))

(defrule end-of-line (or (and #\linefeed #\return)
                         (and #\return #\linefeed)
                         #\linefeed
                         #\return)
  (:constant nil))

(defrule color (or "red" "green" "blue"))

(defrule cube-count (and (? #\space) integer #\space color (? #\,))
  (:destructure (space1 n space2 color separator)
    (declare (ignore space1 space2 separator))
    (cons (intern (string-upcase color) 'keyword) n)))

(defrule reveal (+ (and (+ cube-count)
                        (or #\; end-of-line)))
  (:lambda (list)
    (mapcar #'car list)))

(defrule game (and "Game " integer #\: (+ reveal))
  (:destructure (s game-number colon reveals)
    (declare (ignore s colon))
    (cons game-number reveals)))

(defrule game-listing (+ game))

(defun day02 (is-part-two)
  (let ((games (parse 'game-listing (uiop:read-file-string "./02.txt"))))
    (if is-part-two
        (loop :for game :in games
              :sum (apply #'*
                          (loop :for color :in '(:red :green :blue)
                                :collect
                                (loop :for reveal :in (cadr game)
                                      :maximize
                                      (or (alexandria:assoc-value reveal color)
                                          0)))))
        (loop :with limits := `((:red . 12) (:green . 13) (:blue . 14))
              :for (game-number reveals) :in games
              :unless (loop :for reveal :in reveals
                            :when (loop :for (color . n) :in reveal
                                        :when (< (alexandria:assoc-value limits
                                                                         color)
                                                 n)
                                          :return t)
                              :return t)
                :sum game-number))))
