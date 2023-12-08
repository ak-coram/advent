(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :trivia)

(defun day07 (is-part-two)
  (labels ((occurences (hand)
             (loop :with acc := nil
                   :for card :in hand
                   :if (alexandria:assoc-value acc card)
                     :do (incf (alexandria:assoc-value acc card))
                   :else :do (push (cons card 1) acc)
                   :finally (return (sort acc #'> :key #'cdr))))
           (get-label-strength (hand)
             (let* ((occurences (occurences hand))
                    (joker-count (or (alexandria:assoc-value occurences #\J) 0)))
               (unless (or (not is-part-two) (eql joker-count 5))
                 (setf occurences (remove #\J occurences :key #'car))
                 (incf (cdar occurences) joker-count))
               (trivia:match (mapcar #'cdr occurences)
                 ((list 5) 7)           ; Five of a kind
                 ((list 4 1) 6)         ; Four of a kind
                 ((list 3 2) 5)         ; Full house
                 ((list 3 1 1) 4)       ; Three of a kind
                 ((list 2 2 1) 3)       ; Two pair
                 ((list 2 1 1 1) 2)     ; One pair
                 ((list 1 1 1 1 1) 1)   ; High card
                 )))
           (get-card-strength (card)
             (ecase card
               (#\A 14) (#\K 13) (#\Q 12) (#\J (if is-part-two 1 11)) (#\T 10)
               (#\9 9) (#\8 8) (#\7 7) (#\6 6)
               (#\5 5) (#\4 4) (#\3 3) (#\2 2))))
    (let* ((deals (loop :with lines := (uiop:read-file-lines "./07.txt")
                        :for line :in lines
                        :for (hand bid) := (ppcre:split " " line)
                        :collect (cons (loop :for card :across hand
                                             :collect card)
                                       (parse-integer bid))))
           (bid-strengths (loop :for (hand . bid) :in deals
                                :for strength
                                  := (+ (* (expt 10 30)
                                           (or (get-label-strength hand) 0))
                                        (loop :for card :in hand
                                              :for i :from 25 :downto 1 :by 3
                                              :sum (* (expt 10 i)
                                                      (get-card-strength card))))
                                :collect (cons bid strength))))
      (loop :for i :from 1
            :for (bid . nil) :in (sort bid-strengths #'< :key #'cdr)
            :sum (* i bid)))))
