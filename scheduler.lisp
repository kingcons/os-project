(in-package :os-project)

(defparameter *ready-queue* (sb-queue:make-queue))
(defvar *job-order* nil)

(defvar *ss-mutex*
  (sb-thread:make-mutex :name "Short Scheduler Mutex")
  "Ensures that only one CPU runs the Short Scheduler at a time.")
(defvar *rj-semaphore*
  (sb-thread:make-semaphore :name "Running Jobs Semaphore")
  "Helps ensure that memory-reset+long-scheduler aren't invoked
until all jobs are written to disk.")

(defmacro wait-until-zero ((semaphore) &body body)
"This macro has nutty indentation, accesses private sb-thread symbols all over the place and should under no circumstances ever be reused. It should behave properly for its given use case but would be better off replaced by a wait-thread construct of some kind by a person far smarter than I."
  (let ((count (gensym)))
    `(sb-thread::with-system-mutex ((sb-thread::semaphore-mutex ,semaphore)
				    :allow-with-interrupts t)
       (let ((,count (sb-thread::semaphore-%count ,semaphore)))
	 (if (zerop ,count)
	     (progn ,@body)
	     (unwind-protect
		  (progn
		    (incf (sb-thread::semaphore-waitcount ,semaphore))
		    (loop until (zerop
				  (setf ,count (sb-thread::semaphore-%count
						,semaphore)))
			  do (sb-thread:condition-wait
			      (sb-thread::semaphore-queue ,semaphore)
			      (sb-thread::semaphore-mutex ,semaphore)))
		    ,@body
		    (decf (sb-thread::semaphore-waitcount ,semaphore)))))))))

(defun job-total-space (job)
  (with-slots (ins-count data-buffer data-count scratchpad) job
    (+ ins-count data-buffer data-count scratchpad)))

(defun order-jobs (&key (policy nil) (comparison nil))
  (sort (loop for job-id being the hash-keys in *pcb*
	      using (hash-value v)
	      collecting (cons job-id (funcall policy v)))
	comparison :key #'cdr))

(defun long-scheduler ()
  (let ((priority 1))
    (unless *job-order*
      (throw 'no-more-jobs nil))
    (loop for job-id = (car (pop *job-order*)) while job-id
	  for job = (gethash job-id *pcb*)
	  until (> (job-total-space job) (memory-free *memory*))
	  do (move-job job :type :load)
	     (sb-queue:enqueue job-id *ready-queue*)
	     ;; use get-internal-run-time here instead? both?
	     (when *profiling*
	       (let ((now (get-internal-real-time)))
		 (setf (profile-waiting job) now
		       (profile-completion job) now)))
	     (incf priority))))

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
	 (sb-thread:signal-semaphore *rj-semaphore*)
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
	 (sb-thread:wait-on-semaphore *rj-semaphore*)
	 (format t "~d words saved to disk.~%" total-space)))))

(defun short-scheduler (cpu)
  "In the case where job-id is null, the ss-mutex will be held blocking other threads from calling the short-scheduler. With the other threads unable to load new jobs via the short-scheduler, we can wait for rj-semaphore to be zero to indicate that all jobs have been saved to disk."
  (let* ((job-id (sb-queue:dequeue *ready-queue*))
	 (job (gethash job-id *pcb*)))
;    (when nil ; *context-switch-p*?
;      (context-switch))
    (if job-id
	(dispatcher job job-id cpu)
	(wait-until-zero (*rj-semaphore*)
	  (memory-reset *memory*)
	  (long-scheduler)
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
  (setf (job-id cpu) job-id))
