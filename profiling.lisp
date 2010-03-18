(in-package :os-project)

(defvar *profile-io* nil)
(defvar *profile-waiting* nil)
(defvar *profile-completion* nil)

;; type options: :io, :waiting, :completion
(defmacro profile (job-id type &rest body)
  (let ((job (gethash job-id *pcb*))
	(runtime (runtime body)))
    `(incf ,(ecase type
	      (:io
		 (profile-io job))
	      (:waiting
		 (profile-waiting job))
	      (:completion
		 (profile-completion job)))
	   ,runtime)))

(defmacro runtime (&body body)
  `(let ((pre ,(get-internal-real-time)))
     ,@body
     (coerce (/ (- (get-internal-real-time) pre) 1000) 'float)))