(in-package :os-project)

;; Professor provided "binaries", really text files filled with hex strings.
;; ... and Windows line endings.
(defvar *data1* "DataFile1.txt")
(defvar *data2* "DataFile2.txt")

;; Maybe use a special var here?
(defvar *current-job* nil)

(defun loader (filepath)
  (with-open-file (input filepath)
    (loop for line = (read-line input nil) while line do
      (parse-line line))))

;; And this just makes me uncomfortable for some reason.
(defun read-hex-from-string (string)
  (read-from-string (concatenate 'string "#x" string)))

(defun parse-line (line)
  (cond ((and (> (length line) 1)
	      (string= "//" (subseq line 0 2)))
	 (parse-control line))
	((> (length line) 1)
	 (parse-data line))
	(t nil)))

(defun parse-control (control-string)
  (let* ((control-list (split-sequence #\Space control-string))
	 (command (second control-list)))
    ;; TODO: Top 2 cases don't work due to (I'm pretty sure) Windows line endings. Gag me.
    (cond ((string= (first control-list) "//END")
	   (format t "All jobs have finished.~%"))
	  ((string= command "END")
	   (format t "Loaded job-data pair ~d.~%" *current-job*))
	  ((string= command "JOB")
	   (setf *current-job* (third control-list))
	   (format t "Loading Job.~%")
	   (pcb-update control-list :type :job))
	  ((string= command "Data")
	   (format t "Loading Data.~%")
	   (pcb-update control-list :type :data)))))

;; Also, there's probably a much better way to do this...
(defun pcb-update (metadata &key type)
  (cond ((eql type :job)
	 (setf (gethash (third metadata) *pcb*)
	       (make-instance 'process-state
			      ;; Not sure if it's cheating to store the converted hex value.
			      ;; Can change and convert at call sites if necessary.
			      :ins-count (read-hex-from-string (fourth metadata))
			      :priority (read-hex-from-string (fifth metadata))
			      :start-disk (memory-index *disk*))))
	((eql type :data)
	 (let* ((process-state (gethash *current-job* *pcb*))
		(count (read-hex-from-string (third metadata)))
		(buffer (read-hex-from-string (fourth metadata)))
		(scratch (read-hex-from-string (fifth metadata))))
	   ;; Use with-accessors here?
	   (setf (data-count process-state) count)
	   (setf (data-buffer process-state) buffer)
	   (setf (scratchpad process-state) scratch)))
	(t (format t "Something has gone quite wrong. We're embarassed.~%"))))

(defun parse-data (hex-line)
  (let ((line (subseq hex-line 2 10)))
    (memory-push-end *disk* line)))
