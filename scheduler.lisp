(in-package :os-project)

;; just a collection of job ids, phase 1's long term scheduling will
;; just be FIFO
(defparameter *ready-queue* (make-array 1 :fill-pointer 0))

(defun rq-get-next ()
  (vector-pop *ready-queue*))

(defun rq-insert (job-id)
  (vector-push job-id *ready-queue*))

;; unloads job from RAM and stores in back to disk
;;(defun unload-job (job-id))

;; loads job from disk into RAM
;; since we're assuming 1 job at a time for phase 1,
;; this little hack always loads the code in ram at position 0
;; for phase 2, will need to find space and allocate accordingly.
;; maybe done at the memory management level
(defun load-job (job-id)
  (let* ((current-job (gethash (write-to-string job-id) *pcb*))
	 (num (+ (ins-count current-job)
		   (data-count current-job)
		   (scratchpad current-job)))
	 (start-pos (start-disk current-job)))
    (setf (start-ram current-job) 0)
       (do ((index start-pos (+ index 1))
	 (ram-place 0 (+ ram-place 1)))
	((equal index (+ start-pos num)) (format t "~D words loaded into RAM" num))
      (memory-write *memory* ram-place (memory-read *disk* index))))
)