(in-package :os-project)

(defparameter *ready-queue* (sb-concurrency:make-queue))
(defvar *job-order* nil)

(defstruct (running-jobs (:conc-name rj-))
  (count 0 :type sb-vm:word))
(defvar *job-count* (make-running-jobs)
  "Used to track the running job count with atomic-incf and decf.")
(defvar *ss-mutex* (sb-thread:make-mutex :name "Short Scheduler Mutex")
  "Ensures that only one CPU runs the Short Scheduler at a time.")

(defun job-total-space (job)
  (with-slots (ins-count data-buffer data-count scratchpad) job
    (+ ins-count data-buffer data-count scratchpad)))

(defun order-jobs (&key (policy nil) (comparison nil))
  (sort (loop for job-id being the hash-keys in *pcb*
	      using (hash-value v)
	      collecting (cons job-id (funcall policy v)))
	comparison :key #'cdr))

(defun long-scheduler ()
  (unless *job-order*
    (throw 'no-more-jobs nil))
  (loop for job-id = (caar *job-order*) while job-id
	for job = (gethash job-id *pcb*)
	until (> (job-total-space job) (memory-free *memory*))
	do (move-job job :type :load)
	   (sb-concurrency:enqueue job-id *ready-queue*)
	   ;; use get-internal-run-time here instead? both?
           (pop *job-order*)
	   (when *profiling*
	     (let ((now (get-internal-real-time)))
	       (setf (profile-waiting job) now
		     (profile-completion job) now)))))

(defun move-job (job &key type job-io)
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
	 (when *profiling*
	   (setf (profile-completion job)
		 (time-difference (profile-completion job)
				  (get-internal-real-time)))
	   (setf (profile-io job) job-io))
	 (setf (status job) :in-disk)
	 (setf (start-ram job) -1)
         (sb-ext:atomic-decf (rj-count *job-count*))
	 (format t "~d words saved to disk.~%" total-space)))))

(defun short-scheduler (cpu)
  "In the case where job-id is null, the ss-mutex will be held blocking
other threads from calling the short-scheduler. With the other threads
unable to load new jobs via the short-scheduler, we set up an event loop
to periodically check to see if all jobs have finished running. If they
have, we call reset *memory* and invoke the long scheduler, otherwise
we sleep a bit."
  (let* ((job-id (sb-concurrency:dequeue *ready-queue*))
	 (job (gethash job-id *pcb*)))
    (if job-id
	(dispatcher job job-id cpu)
	(progn
	  (loop until (not (sb-concurrency:queue-empty-p *ready-queue*)) do
            (if (zerop (rj-count *job-count*))
                (progn
                  (memory-reset *memory*)
                  (long-scheduler))
                (sleep 0.001)))
          (short-scheduler cpu)))))

(defun dispatcher (job job-id cpu)
  (registers-clear cpu)
  (setf (breg cpu) (start-ram job)
	(pc cpu) 0
	(ireg cpu) 0)
  (when *profiling*
    (setf (profile-waiting job)
	  (time-difference (profile-waiting job)
			   (get-internal-real-time))))
  (setf (status job) :running)
  (setf (job-id cpu) job-id)
  (sb-ext:atomic-incf (rj-count *job-count*)))
