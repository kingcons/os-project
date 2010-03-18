(in-package :os-project)

;Policy options: #'job-total-space, #'ins-count, #'data-count, #'priority  
(defun init ()
  (loader *data2*)
  (setf *job-order* (order-jobs :policy #'ins-count :comparison #'>))
  (long-scheduler)
  (short-scheduler *cpu1*))

(defun os-driver () ; or main/kernel
  (init)
  ; now what?
  )
