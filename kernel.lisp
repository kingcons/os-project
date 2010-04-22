(in-package :os-project)

;Policy options: #'job-total-space, #'ins-count, #'data-count, #'priority
(defun init ()
  (loader *data2*)
  (setf *job-order* (order-jobs :policy #'ins-count :comparison #'<))
  (long-scheduler)
  (short-scheduler *cpu1*))

(defun kernel ()
  (catch 'no-more-jobs
    (loop (fetch *cpu1*)
	  (decode *cpu1*))))

(defun os-driver ()
  (init)
  (kernel))

(defun debugging (setting)
  (ecase setting
    (:on
       (trace memory-read memory-write time-difference
	      reg-r reg-w fetch decode))
    (:off
       (untrace memory-read memory-write time-difference
		reg-r reg-w fetch decode))))

(defun reset ()
  (clear-all-data)
  (setf *ready-queue* (sb-queue:make-queue))
  (setf *cpu1* (make-instance 'cpu)))
