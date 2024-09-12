(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :queues.simple-queue)

(defun day20 (is-part-two)
  (let* ((lines (uiop:read-file-lines #P"./20.txt"))
         (module-config
           (loop :for line :in lines
                 :for (module destinations) := (ppcre:split " -> " line)
                 :collect (list (if (string= module "broadcaster")
                                    :broadcast
                                    (intern (subseq module 1)))
                                (char module 0)
                                (mapcar #'intern
                                        (ppcre:split ", " destinations)))))
         (modules (make-hash-table))
         (pulse-queue (queues:make-queue :simple-queue))
         (low-pulse-count 0) (high-pulse-count 0))
    (labels ((find-sources (module)
               (loop :for (source-module type destinations) :in module-config
                     :when (member module destinations)
                       :collect source-module))
             (send (source pulse-type destinations)
               (loop :for destination :in destinations
                     :do (case pulse-type
                           (:low (incf low-pulse-count))
                           (:high (incf high-pulse-count)))
                     :do (queues:qpush pulse-queue (list source
                                                         destination
                                                         pulse-type)))
               pulse-type)
             (make-broadcast (destinations)
               (lambda (pulse) (send :broadcast (caddr pulse) destinations)))
             (make-flip-flop (module destinations)
               (let ((state nil))
                 (lambda (pulse)
                   (when (eql (caddr pulse) :low)
                     (setf state (not state))
                     (send module (if state :high :low) destinations)))))
             (make-conjunction (module destinations)
               (let ((state (loop :for source :in (find-sources module)
                                  :collect (cons source :low))))
                 (lambda (pulse)
                   (setf (alexandria:assoc-value state (first pulse))
                         (third pulse))
                   (send module
                         (if (every (lambda (pulse-type)
                                      (eql pulse-type :high))
                                    (mapcar #'cdr state))
                             :low
                             :high)
                         destinations))))
             (press-button (&optional sources-to-track)
               (send :button :low '(:broadcast))
               (loop :for pulse := (queues:qpop pulse-queue)
                     :while pulse
                     :for target := (second pulse)
                     :for module := (gethash target modules)
                     :when (and module
                                (eql (funcall module pulse) :high)
                                (member target sources-to-track))
                       :collect target)))
      (loop :for (module type destinations) :in module-config
            :do (setf (gethash module modules)
                      (case type
                        (#\b (make-broadcast destinations))
                        (#\% (make-flip-flop module destinations))
                        (#\& (make-conjunction module destinations)))))
      (if is-part-two
          (loop :with conjunction-sources
                  := (find-sources (car (find-sources '|rx|)))
                :and loop-periods := nil
                :for n :from 1
                :do (loop :for low-pulse-source
                            :in (press-button conjunction-sources)
                          :do (setf (alexandria:assoc-value loop-periods
                                                            low-pulse-source)
                                    n))
                :when (eql (length conjunction-sources) (length loop-periods))
                  :return (apply #'lcm (mapcar #'cdr loop-periods)))
          (loop :for n :below 1000 :do (press-button)
                :finally (return (* low-pulse-count high-pulse-count)))))))
