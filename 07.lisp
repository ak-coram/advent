(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :trivia)

(labels ((occurences (hand)
           (loop :with acc := nil
                 :for card :in hand
                 :if (alexandria:assoc-value acc card)
                   :do (incf (alexandria:assoc-value acc card))
                 :else :do (push (cons card 1) acc)
                 :finally (return (sort acc #'> :key #'cdr))))
         (get-label-strength (hand)
           (trivia:match (mapcar #'cdr (occurences hand))
             ((list 5) 7)               ; Five of a kind
             ((list 4 1) 6)             ; Four of a kind
             ((list 3 2) 5)             ; Full house
             ((list 3 1 1) 4)           ; Three of a kind
             ((list 2 2 1) 3)           ; Two pair
             ((list 2 1 1 1) 2)         ; One pair
             ((list 1 1 1 1 1) 1)       ; High card
             ))
         (get-card-strength (card)
           (case card
             (#\A 13) (#\K 12) (#\Q 11) (#\J 10) (#\T 9)
             (#\9 8) (#\8 7) (#\7 6) (#\6 5)
             (#\5 4) (#\4 3) (#\3 2) (#\2 1))))
  (let* ((deals (loop :with lines := (uiop:read-file-lines "./07.txt")
                      :for line :in lines
                      :for (hand bid) := (ppcre:split " " line)
                      :collect (cons (loop :for card :across hand
                                           :collect card)
                                     (parse-integer bid))))
         (bid-strengths (loop :for (hand . bid) :in deals
                              :for strength
                                := (+ (* (expt 10 20) (get-label-strength hand))
                                      (loop :for card :in hand
                                            :for i :from 15 :downto 1 :by 2
                                            :sum (* (expt 10 i)
                                                    (get-card-strength card))))
                              :collect (cons bid strength))))
    (loop :for i :from 1
          :for (bid . nil) :in (sort bid-strengths #'< :key #'cdr)
          :sum (* i bid))))
