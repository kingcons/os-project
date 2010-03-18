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
	  do (move-job job :type :load)
	     (cl-heap:enqueue *ready-queue* job-id priority)
	     (incf priority))))

(defun move-job (job &key type)
  (let ((start-ram (start-ram job))
	(start-disk (start-disk job))
	(total-space (job-total-space job)))
    (ecase type
      (:load
	 (setf (start-ram job) (memory-index *memory*))
	 (loop for i from start-disk to (1- (+ start-disk total-space))
	       do (memory-push-end *memory* (memory-read *disk* i)))
	 (setf (status job) :in-memory)
	 (format t "~d words loaded into RAM.~%" total-space))
      (:save
	 (loop for i from start-disk to (1- (+ start-disk total-space))
	       for j from start-ram to (1- (+ start-ram total-space))
	       do (memory-write *disk* i (memory-read *memory* j)))
	 (setf (status job) :in-disk)
	 (format t "~d words saved to disk.~%" total-space)))))

(defun short-scheduler (cpu)
  (let* ((job-id (cl-heap:dequeue *ready-queue*))
	 (job (gethash job-id *pcb*)))
;    (when nil ; *context-switch-p*?
;      (context-switch))
    (dispatcher job job-id cpu)))

(defun dispatcher (job job-id cpu)
  (registers-clear cpu)
  (setf (breg cpu) (start-ram job)
	(pc cpu) 0
	(ireg cpu) 0)
  (setf (status job) :running)
  (setf (job-id cpu) job-id))
