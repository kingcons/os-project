(in-package :os-project)

;; just a collection of job ids, phase 1's long term scheduling will
;; just be FIFO
(defparameter *ready-queue* (make-instance 'cl-heap:priority-queue))

(defvar *job-order* nil)

(defun job-total-space (job)
  (+ (ins-count job)
     (data-buffer job)
     (data-count job)
     (scratchpad job)))

(defun order-jobs (&key (policy nil) (comparison nil))
  (sort (loop for job-id being the hash-keys in *pcb*
	      using (hash-value v)
	      collecting (cons job-id (funcall policy v)))
	comparison :key #'cdr))

(defun long-scheduler ()
  (let ((priority 1))
    (loop for job-id = (car (pop *job-order*)) while job-id
	  for job = (gethash job-id *pcb*)
	  until (> (job-total-space job) (memory-free *memory*))
	  do (load-job job)
	     (cl-heap:enqueue *ready-queue* job-id priority)
	     (incf priority))))

(defun load-job (job)
  (setf (start-ram job) (memory-index *memory*))
  (loop for i from (start-disk job) to (1- (+ (start-disk job)
					      (job-total-space job)))
	do (memory-push-end *memory* (memory-read *disk* i)))
  (setf (status job) :in-memory)
  (format t "~d words loaded into RAM.~%" (job-total-space job)))

(defun short-scheduler (cpu)
  (let* ((job-id (cl-heap:dequeue *ready-queue*))
	 (job (gethash job-id *pcb*)))
    (when nil ; *context-switch-p*
      (context-switch))
    (dispatcher job job-id cpu)))

(defun dispatcher (job job-id cpu)
  (registers-clear cpu)
  (setf (breg cpu) (start-ram job)
	(pc cpu) 0
	(ireg cpu) 0)
  (setf (status job) :running)
  (setf (job-id cpu) job-id))
