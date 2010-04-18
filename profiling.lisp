(in-package :os-project)

(defvar *profiling* nil)

(defun time-difference (start end)
  (coerce (/ (- end start) 1000) 'float))

(defmacro profile (&body body)
  `(if *profiling*
       (progn
	 (incf (car (job-io cpu)))
	 (incf (cdr (job-io cpu))
	       ;; obviously, this only gets us the real-time diff
	       (runtime ,@body)))
       ,@body))

(defmacro runtime (&body body)
  `(let ((pre-real ,(get-internal-real-time))
	 (pre-run ,(get-internal-run-time)))
     ,@body
     (values (time-difference pre-real (get-internal-real-time))
	     (time-difference pre-run (get-internal-run-time)))))
