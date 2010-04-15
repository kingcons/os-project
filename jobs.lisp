(in-package :os-project)

(defun list-identical-jobs ()
  (let ((result (make-hash-table))
	(jobs (loop for i from 1 to 30 collecting i)))
    (loop for i from 1 to 30 do
      (let* ((job (gethash i *pcb*))
	     (count (ins-count job))
	     (start (start-disk job)))
	(loop for j from (1+ i) to 30 do
	  (let* ((job2 (gethash j *pcb*))
		 (count2 (ins-count job2))
		 (start2 (start-disk job2)))
	    (when (and (= count count2)
		       (loop for ins-index from 0 to (1- count)
			     always (string= (memory-read *disk* (+ start ins-index))
					     (memory-read *disk* (+ start2 ins-index))))
		       (never-present j result))
	      (push j (gethash i result))
	      (mapcar #'(lambda (x) (setf jobs (remove x jobs))) (list i j)))))))
    (maphash #'(lambda (k v)
		 (format t "Job ~a has the following copies: ~a~%" k v)) result)
    (format t "And the following jobs are unique: ~a~%" jobs)))

(defun never-present (job-id hash-table)
  (loop for job-ids being the hash-values of hash-table
	never (member job-id job-ids)))
