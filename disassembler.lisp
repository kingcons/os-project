(in-package :os-project)

(defun disasm (job-id &key (show-separately nil) (extra nil))
  (let* ((job-metadata (gethash (write-to-string job-id) *pcb*))
	 (job-start (start-disk job-metadata))
	 (job-end (+ job-start (ins-count job-metadata))))
    (format t "Disassembly for Job ~a:~%" job-id)
    (format t "Index  ~11a instr  reg1 reg2 reg3~%" "Hex")
    (loop for i from job-start to job-end do
      (let ((hex-string (memory-read *disk* i)))
	(format t "~5a  0x~8a: " i hex-string)
	(cond (extra
	       (pretty-print-asm hex-string :ins-data t :op-data t))
	      (show-separately
	       (pretty-print-asm hex-string))
	      (t
	       (pretty-print-asm hex-string :raw t)))))))
  
(defun pretty-print-asm (hex-str &key (raw nil) (ins-data nil) (op-data nil))
  (let* ((ins (read-hex-from-string hex-str))
	 (ins-type (ldb (byte 2 30) ins))
	 (opcode (ldb (byte 6 24) ins))
	 (reg1 0) (reg2 0) (reg3 0)
	 (opcodes #("rd" "wr" "st" "lw" "mov" "add" "sub" "mul" "div" "and"
		    "or" "movi" "addi" "muli" "divi" "ldi" "slt" "slti"
		    "hlt" "nop" "jmp" "beq" "bneq" "bez" "bnz" "bgz" "blz")))
    (flet ((bit-range (width position)
	     (ldb (byte width position) ins)))
      (ecase ins-type
	(#b00
	   (setf reg1 (bit-range 4 20))
	   (setf reg2 (bit-range 4 16))
	   (setf reg3 (bit-range 4 12))
	   (when ins-data
	     (setf ins-data (format nil "Arithmetic         IF: s-reg s-reg d-reg, ~2a ~2a ~2a" 4 4 4))))
	(#b01
	   (setf reg1 (bit-range 4 20))
	   (setf reg2 (bit-range 4 16))
	   (setf reg3 (bit-range 16 0))
	   (when ins-data
	     (setf ins-data (format nil "Cond and Immediate IF: b-reg d-reg addr, ~2a ~2a ~2a" 4 4 16))))
	(#b10
	   (setf reg1 (bit-range 24 0))
	   (when ins-data
	     (setf ins-data (format nil "Unconditional Jump IF: addr, ~2a" 24))))
	(#b11
	   (setf reg1 (bit-range 4 20))
	   (setf reg2 (bit-range 4 16))
	   (setf reg3 (bit-range 16 0))
	   (when ins-data
	     (setf ins-data (format nil "Input and Output   IF: reg1 reg2 addr, ~2a ~2a ~2a" 4 4 16))))))
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
    (if raw
	(format t "~5a  ~4a ~4a ~4a~%" (aref opcodes opcode) reg1 reg2 reg3)
	(format t "~%ins-type opcode reg1 reg2 reg3~%~8a ~6a ~4a ~4a ~4a~%"
		ins-type (aref opcodes opcode) reg1 reg2 reg3))
    (when ins-data
      (format t "Instruction Format is ~a~%" ins-data))
    (when op-data
      (format t "Opcode is ~a~%" op-data))
    (unless raw
      (format t "~%"))))
