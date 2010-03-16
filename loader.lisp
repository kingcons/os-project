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

(defun read-hex-from-string (string)
  (parse-integer string :radix 16))

(defun to-hex-string (integer)
  (write-to-string integer :base 16))
;  (format nil "~X" integer))

#| Do I need these?
(defun read-binary-from-string (string)
  (parse-integer string :radix 2))
(defun to-binary-string (integer)
  (write-to-string integer :base 2))
|#

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
	   (setf (status (gethash *current-job* *pcb*)) :in-disk)
	   (format t "Loaded job-data pair 0x~d.~%" *current-job*))
	  ((string= command "JOB")
	   (setf *current-job* (third control-list))
	   (format t "Loading Job.~%")
	   (pcb-update control-list :type :job))
	  ((string= command "Data")
	   (format t "Loading Data.~%")
	   (pcb-update control-list :type :data)))))

(defun pcb-update (metadata &key type)
  (cond ((eql type :job)
	 (setf (gethash (third metadata) *pcb*)
	       (make-instance 'process-state
			      :ins-count (read-hex-from-string (fourth metadata))
			      :priority (read-hex-from-string (fifth metadata))
			      ; Assumes order on disk doesn't change and disk is only loaded once.
			      :start-disk (memory-index *disk*))))
	((eql type :data)
	 (let ((process-state (gethash *current-job* *pcb*)))
	   (setf (data-count process-state) (read-hex-from-string (third metadata))
		 (data-buffer process-state) (read-hex-from-string (fourth metadata))
		 (scratchpad process-state) (read-hex-from-string (fifth metadata)))))
	 (t (format t "Something has gone quite wrong. We're embarassed.~%"))))

(defun parse-data (hex-line)
  (let ((line (subseq hex-line 2 10)))
    (memory-push-end *disk* line)))
