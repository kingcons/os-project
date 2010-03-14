(in-package :os-project)

;; Programs are converted from hex strings to integers (binary)
;; on load from memory.

;; Register 0 is the accumulator, Register 1 is "true zero".
;; Does it make sense to hardcode this in some fashion?

;; Note, integers may have integer-length under 32 bits.
;; They still have top bits. Do not be fooled and try
;; to ldb only beneath their integer-length.

(deftype register ()
  '(unsigned-byte 32))

(defclass cpu ()
  ((program-counter
    :initform 0
    :accessor pc)
   (registers
    :initform (make-array 16 :element-type 'register)
    :accessor registers)
   (base-register
    :initform 0
    :accessor breg)
   (ins-register
    :initform 0
    :accessor ireg)))

(defvar *cpu1* (make-instance 'cpu))

(defmethod registers-clear ((cpu cpu))
  (setf (registers cpu)	(make-array 16 :element-type 'register)))

(defmethod register-write ((cpu cpu) index value)
  (setf (aref (registers cpu) index) value))

(defmethod register-read ((cpu cpu) index)
  (aref (registers cpu) index))

;; SERIOUS CONCERNS HERE
(defmethod address ((cpu cpu) type offset)
  (ecase type
    (:direct (+ (breg cpu) offset))
; TODO: Correct implementation once prof clarifies\corrects the spec.
    (:indirect (+ (breg cpu) (ireg cpu) offset))))

; TODO: Ensure implementation correctness after spec updates.
;; Probably should write the retval to ireg.
(defmethod fetch ((cpu cpu))
  (setf ireg (read-hex-from-string
	      (memory-read *memory* (address cpu (pc cpu) :direct)))))

(defun disasm (job-id &key (show-separately nil))
  (let* ((job-metadata (gethash (write-to-string job-id) *pcb*))
	 (job-start (start-disk job-metadata))
	 (job-end (+ job-start (ins-count job-metadata))))
    (format t "Disassemly for Job ~a:~%" job-id)
    (loop for i from job-start to job-end do
      (let ((hex-string (memory-read *disk* i)))
	(format t "~4a  0x~8a: " i hex-string)
	(if show-separately
	    (pretty-print-asm hex-string)
	    (pretty-print-asm hex-string :raw t))))))
  
(defun pretty-print-asm (hex-str &key (raw nil))
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
	   (setf reg3 (bit-range 4 12)))
	(#b01
	   (setf reg1 (bit-range 4 20))
	   (setf reg2 (bit-range 4 16))
	   (setf reg3 (bit-range 16 0)))
	(#b10
	   (setf reg1 (bit-range 24 0)))
	(#b11
	   (setf reg1 (bit-range 4 20))
	   (setf reg2 (bit-range 4 16))
	   (setf reg3 (bit-range 16 0)))))
    (if raw
	(format t "~5a  ~3a ~3a ~3a~%" (aref opcodes opcode) reg1 reg2 reg3)
	(format t "~%ins-type opcode reg1 reg2 reg3~%~8b ~6a ~4a ~4a ~4a~%~%"
		ins-type (aref opcodes opcode) reg1 reg2 reg3))))

;; (defmethod decode ((cpu cpu))
;;   (let* ((instruction (iref cpu))
;; 	 (ins-type (ldb (byte 2 30) instruction))
;; 	 (opcode (ldb (byte 6 24) instruction))
;; 	 (reg1 0) (reg2 0) (reg3 0))
;;     (flet ((bit-range (width position)
;; 	     (ldb (byte width position) instruction)))
;;       (ecase ins-type
;; 	(#b00
;; 	   (setf reg1 (bit-range 4 20))
;; 	   (setf reg2 (bit-range 4 16))
;; 	   (setf reg3 (bit-range 4 12)))
;; 	(#b01
;; 	   (setf reg1 (bit-range 4 20))
;; 	   (setf reg2 (bit-range 4 16))
;; 	   (setf reg3 (bit-range 16 0)))
;; 	(#b10
;; 	   (setf reg1 (bit-range 24 0)))
;; 	(#b11
;; 	   (setf reg1 (bit-range 4 20))
;; 	   (setf reg2 (bit-range 4 16))
;; 	   (setf reg3 (bit-range 16 0)))))
;;     (ecase opcode
;;       ;; TODO:
;;       ;; Right now this ecase executes instructions.
;;       ;; --We'll probably have to split that out.
;;       ;; Many of these opcodes are unwritten or pseudocode.
;;       ;; input-buf and output-buf need address resolution...
;;       ;; Finally, some macros to tidy this up might be nice.
;;       (#x00 ; RD
;; 	 `(register-write cpu 0
;; 			  (read-from-hex-string input-buf)))
;;       (#x01 ; WR
;; 	 `(setf output-buf (register-read cpu 0)))
;;       (#x02 ; ST
;; 	 `())
;;       (#x03 ; LW
;; 	 `())
;;       (#x04 ; MOV
;; 	 `())
;;       (#x05 ; ADD
;; 	 `(register-write cpu reg3
;; 			 (+ (register-read cpu reg1)
;; 			    (register-read cpu reg2))))
;;       (#x06 ; SUB
;; 	 `(register-write cpu reg3
;; 			  (- (register-read cpu reg1)
;; 			     (register-read cpu reg2))))
;;       (#x07 ; MUL
;; 	 `(register-write cpu reg3
;; 			  (* (register-read cpu reg1)
;; 			     (register-read cpu reg2))))
;;       (#x08 ; DIV
;; 	 `(register-write cpu reg3
;; 			  (/ (register-read cpu reg1)
;; 			     (register-read cpu reg2))))
;;       (#x09 ; AND
;; 	 `(register-write cpu reg3
;; 			  (logand (register-read cpu reg1)
;; 				  (register-read cpu reg2))))
;;       (#x0a ; OR
;; 	 `(register-write cpu reg3
;; 			  (logior (register-read cpu reg1)
;; 				  (register-read cpu reg2))))
;;       (#x0b ; MOVI
;; 	 `())
;;       (#x0c ; ADDI
;; 	 `())
;;       (#x0d ; MULI
;; 	 `())
;;       (#x0e ; DIVI
;; 	 `())
;;       (#x0f ; LDI
;; 	 `())
;;       (#x10 ; SLT
;; 	 `(if (< (register-read cpu reg1)
;; 		 (register-read cpu reg2))
;; 	      (register-write cpu reg3 1)
;; 	      (register-write cpu reg3 0)))
;;       (#x11 ; SLTI
;; 	 `())
;;       (#x12 ; HLT
;; 	 `())
;;       (#x13 ; NOP
;; 	 `())
;;       (#x14 ; JMP
;; 	 `())
;;       (#x15 ; BEQ
;; 	 `())
;;       (#x16 ; BNEQ
;; 	 `())
;;       (#x17 ; BEZ
;; 	 `())
;;       (#x18 ; BNZ
;; 	 `())
;;       (#x19 ; BGZ
;; 	 `())
;;       (#x1a ; BLZ
;; 	 `()))))