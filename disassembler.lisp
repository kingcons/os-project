(in-package :os-project)

(defun disasm (job-id &key (show-separately nil) (extra nil))
  (let* ((job-metadata (gethash job-id *pcb*))
	 (job-start (start-disk job-metadata))
	 (job-end (+ job-start (1- (ins-count job-metadata)))))
    (format t "Disassembly for Job ~a:~%" job-id)
    (format t "~11a instr  reg1 reg2 reg3~%" "Hex")
    (loop for i from job-start to job-end do
      (let ((hex-string (memory-read *disk* i)))
	(cond (extra
	       (pretty-print-asm hex-string :ins-data t :op-data t))
	      (show-separately
	       (pretty-print-asm hex-string))
	      (t
	       (pretty-print-asm hex-string :raw t)))))))
  
(defun pretty-print-asm (hex-str &key (raw nil) (ins-data nil) (op-data nil) (job-id nil))
  (let* ((parsed-ins (parse-instruction hex-str :string-p t :ins-data ins-data))
	 (opcode (first parsed-ins))
	 (reg1 (second parsed-ins))
	 (reg2 (third parsed-ins))
	 (reg3 (fourth parsed-ins))
	 (ins-data (fifth parsed-ins))
	 (hexstr (sixth parsed-ins))
	 (ins-type (seventh parsed-ins))
	 (opcodes #("rd" "wr" "st" "lw" "mov" "add" "sub" "mul" "div" "and"
		    "or" "movi" "addi" "muli" "divi" "ldi" "slt" "slti"
		    "hlt" "nop" "jmp" "beq" "bneq" "bez" "bnz" "bgz" "blz")))
    (when op-data
      (ecase opcode
	(#x00 (setf op-data "Type: I/O. Reads contents of Input buffer into the Accumulator"))
	(#x01 (setf op-data "Type: I/O. Writes contents of Accumulator into the Output buffer"))
	(#x02 (setf op-data "Type: I. Stores contents of a Register to an Address"))
	(#x03 (setf op-data "Type: I. Loads contents of an Address into a Register"))
	(#x04 (setf op-data "Type: R. Transfers the contents of one Register into another"))
	(#x05 (setf op-data "Type: R. Adds contents of two s-regs and stores in d-reg"))
	(#x06 (setf op-data "Type: R. Subtracts contents of two s-regs and stores in d-reg"))
	(#x07 (setf op-data "Type: R. Multiplies contents of two s-regs and stores in d-reg"))
	(#x08 (setf op-data "Type: R. Divides contents of two s-regs and stores in d-reg"))
	(#x09 (setf op-data "Type: R. Logical And of two s-regs and stores in d-reg"))
	(#x0a (setf op-data "Type: R. Logical Or or two s-regs and stores in d-reg"))
	(#x0b (setf op-data "Type: I. Transfers Address/Data directly into a Register"))
	(#x0c (setf op-data "Type: I. Adds Data directly to a Register"))
	(#x0d (setf op-data "Type: I. Multiplies Data directly to a Register"))
	(#x0e (setf op-data "Type: I. Divides Data directly to a Register"))
	(#x0f (setf op-data "Type: I. Loads Data/Address directly into a Register"))
	(#x10 (setf op-data "Type: R. Sets d-reg to 1 if s-reg1 < s-reg2, 0 otherwise"))
	(#x11 (setf op-data "Type: I. Sets d-reg to 1 if s-reg1 < Data, 0 otherwise"))
	(#x12 (setf op-data "Type: J. Logical end of program"))
	(#x13 (setf op-data "Type: -. No-op. Does nothing and moves to next instruction"))
	(#x14 (setf op-data "Type: J. Jumps to a specified location."))
	(#x15 (setf op-data "Type: I. Jump to an Address when b-reg == d-reg"))
	(#x16 (setf op-data "Type: I. Jump to an Address when b-reg != d-reg"))
	(#x17 (setf op-data "Type: I. Jump to an Address when d-reg == 0"))
	(#x18 (setf op-data "Type: I. Jump to an Address when b-reg != 0"))
	(#x19 (setf op-data "Type: I. Jump to an Address when b-reg > 0"))
	(#x1a (setf op-data "Type: I. Jump to an Address when b-reg < 0"))))
    (when job-id
      (format t "Job ~a, " job-id))
    (if raw
	(format t "~a ~5a  ~4a ~4a ~4a~%" hexstr (aref opcodes opcode) reg1 reg2 reg3)
	(format t "~a~%ins-type opcode reg1 reg2 reg3~%~8a ~6a ~4a ~4a ~4a~%"
		hexstr ins-type (aref opcodes opcode) reg1 reg2 reg3))
    (when ins-data
      (format t "Instruction Format is ~a~%" ins-data))
    (when op-data
      (format t "Opcode is ~a~%" op-data))
    (unless raw
      (format t "~%"))))

(defun show-all-users (&key (opcode nil) (ins-type nil) (show-job nil))
  ;; What would be a better way to write this?
  (loop for result in (reverse
			(map-ins #'(lambda (ins)
				     (cond (opcode
					    (= opcode (ldb (byte 6 24) ins)))
					   (ins-type
					    (= ins-type (ldb (byte 2 30) ins)))))
			    :show-job show-job))
	do (if show-job
	       (pretty-print-asm (car result) :raw t :job-id (cdr result))
	       (pretty-print-asm result :raw t))))

(defun map-ins (condition &key (show-job nil))
  (let ((result nil))
    (maphash #'(lambda (job-id job)
		 (loop for i from (start-disk job) to (1- (+ (start-disk job) (ins-count job))) do
		   (let* ((instruction (memory-read *disk* i))
			  (ins (read-hex-from-string instruction)))
		     (cond ((and show-job (funcall condition ins))
			    (pushnew (cons instruction job-id) result
				     :test #'equal :key #'car))
			   ((funcall condition ins)
			    (pushnew instruction result :test #'equal)))))) *pcb*)
    result))

(defun show-all-pcb-slots (job-id)
  (let ((slots '(ins-count priority start-disk start-ram
		 data-count data-buffer scratchpad))) ;status)))
    (loop for slot in slots do
      ;; TODO: Fails if slot is unbound.
      (format t "~a ~a slot is: ~a~%" job-id slot (slot-value (gethash job-id *pcb*) slot)))))
