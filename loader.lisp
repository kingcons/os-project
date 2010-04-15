(in-package :os-project)

(defvar *data1* "DataFile1.txt")
(defvar *data2* "DataFile2.txt")
(defvar *current-job* nil)

(defun loader (filepath)
  (with-open-file (input filepath)
    (loop for line = (read-line input nil) while line do
      (parse-line line)))
  (format t "All jobs in file ~a loaded.~%" filepath)
  (setf *current-job* nil)
  (find-identical-jobs))

(defun read-hex-from-string (string)
  (parse-integer string :radix 16))

(defun to-hex-string (integer)
  (write-to-string integer :base 16))
;  (format nil "~X" integer))

(defun parse-line (line)
  (cond ((and (> (length line) 1)
	      (string= "//" (subseq line 0 2)))
	 (parse-control line))
	((> (length line) 1)
	 (parse-data line))))

(defun parse-control (control-string)
  (let* ((control-list (split-sequence #\Space control-string))
	 (command (second control-list)))
    (cond ((string= command "JOB")
	   (setf *current-job* (read-hex-from-string (third control-list)))
	   (format t "Loading Job.~%")
	   (pcb-update control-list :type :job))
	  ((string= command "Data")
	   (format t "Loading Data.~%")
	   (pcb-update control-list :type :data)
	   (setf (status (gethash *current-job* *pcb*)) :in-disk)
	   (format t "Loaded job-data pair ~d.~%" *current-job*)))))

(defun pcb-update (metadata &key type)
  (cond ((eql type :job)
	 (setf (gethash (read-hex-from-string (third metadata)) *pcb*)
	       (make-instance 'process-state
			      :ins-count (read-hex-from-string
					  (fourth metadata))
			      :priority (read-hex-from-string
					 (fifth metadata))
			      ; Assumes order on disk doesn't change
			      ; and disk is only loaded once.
			      :start-disk (memory-index *disk*))))
	((eql type :data)
	 (let ((process-state (gethash *current-job* *pcb*)))
	   (setf (data-count process-state) (read-hex-from-string
					     (third metadata))
		 (data-buffer process-state) (read-hex-from-string
					      (fourth metadata))
		 (scratchpad process-state) (read-hex-from-string
					     (fifth metadata)))))
	 (t (format t "Something is quite wrong. We're embarassed.~%"))))

(defun parse-data (hex-line)
  (let ((line (subseq hex-line 2 10)))
    (memory-push-end *disk* line)))

(defun find-identical-jobs ()
  (let ((jobs (loop for i from 1 to 30 collecting i)))
    (flet ((instructs-equal (ins-count start1 start2)
	     (loop for idx from 0 to (1- ins-count)
		   always (string= (memory-read *disk* (+ start1 idx))
				   (memory-read *disk* (+ start2 idx))))))
      (loop for i in jobs do
	(let* ((job (gethash i *pcb*))
	       (count (ins-count job))
	       (start (start-disk job))
	       (idents (identical-jobs job)))
	  (loop for j in (remove i jobs) do
	    (let* ((job2 (gethash j *pcb*))
		   (count2 (ins-count job2))
		   (start2 (start-disk job2)))
	      (when (and (= count count2)
			 (instructs-equal count start start2))
		(setf (identical-jobs job) (push j idents))))))))))
