(in-package :os-project)

(defvar *cpu-count* 4)
(defvar *cpu-list* nil)

;Policy options: #'job-total-space, #'ins-count, #'data-count, #'priority
(defun init ()
  (loader *data2*)
  (setf *job-order* (order-jobs :policy #'ins-count :comparison #'<))
  (long-scheduler)
  (loop for i from 1 to *cpu-count* do
    (let ((cpu (make-instance 'cpu)))
      (push cpu *cpu-list*)
      (short-scheduler cpu))))

;; Thanks to an old paste from lnostdal.
;; This ensures threads all share *standard-output*.
(defmacro with-thread (name &body body)
  "Defines a thread that executes `body'. Returns the thread-instance."
  (let ((st (gensym)))
    `(let ((,st *standard-output*))
       (sb-thread:make-thread
        (lambda ()
          (let ((*standard-output* ,st))
            ,@body))
        :name ,name))))

(defmethod kernel ((cpu cpu))
  (with-thread "CPU Kernel"
    (catch 'no-more-jobs
      (loop (fetch cpu)
	    (decode cpu)))))

(defun os-driver ()
  (init)
  (loop for cpu in *cpu-list* do
    (kernel cpu)))

;; There is a better way to write this...
;; i.e. switches for whether to debug memory/timing/cpu/etc
(defun debugging (setting)
  (ecase setting
    (:on
       (trace memory-read memory-write reg-r reg-w
	fetch decode registers-clear time-difference))
    (:off
       (untrace memory-read memory-write reg-r reg-w
	fetch decode registers-clear time-difference))))

(defun reset ()
  (clear-all-data)
  (setf *ready-queue* (sb-queue:make-queue))
  (setf *cpu-list* nil))
