(in-package :os-project)

;; Programs are converted from hex strings to integers (binary)
;; on load from memory.

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
    :accessor ireg)
   (running-job
    :initform nil
    :accessor job-id)))

(defvar *cpu1* (make-instance 'cpu))

(defmethod registers-clear ((cpu cpu))
  (setf (registers cpu)	(make-array 16 :element-type 'register)))

(defmethod register-write ((cpu cpu) index value)
  (setf (aref (registers cpu) index) value))

(defmethod register-read ((cpu cpu) index)
  (aref (registers cpu) index))

(defmethod address ((cpu cpu) addr &key (with-base t))
  (if with-base
      (+ (breg cpu) (/ addr 4))
      (/ addr 4)))

(defmethod fetch ((cpu cpu))
  (setf (ireg cpu) (read-hex-from-string
		    (memory-read *memory* (+ (breg cpu) (pc cpu)))))
  (incf (pc cpu)))

(defmethod decode ((cpu cpu))
  (let* ((instruction (ireg cpu))
	 (ins-type (ldb (byte 2 30) instruction))
	 (opcode (ldb (byte 6 24) instruction))
	 (reg1 0) (reg2 0) (reg3 0))
    (flet ((bit-range (width position)
	     (ldb (byte width position) instruction)))
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
    (ecase opcode
      ;; I can't think of a good way to split out execution from decode.
      ;; Finally, some macros to tidy this up might be nice.
      (#x00 ; RD
	 (register-write cpu reg1
			 (read-hex-from-string
			  (memory-read *memory*
				       (address cpu (if (zerop reg2)
							reg3
							(register-read cpu reg2)))))))
      (#x01 ; WR
	 (memory-write *memory*
		       (address cpu (if (zerop reg2)
					reg3
					(register-read cpu reg2)))
		       (register-read cpu reg1)))
      (#x02 ; ST
	 (memory-write *memory*
		       (address cpu (register-read cpu reg2))
		       (register-read cpu reg1)))
      (#x03 ; LW
	 (register-write cpu reg2
			 (read-hex-from-string
			  (memory-read *memory*
				       (register-read cpu reg1)))))
      (#x04 ; MOV, INCOMPLETE, see peculiar use case in Job 7
	 (register-write cpu reg1 (register-read cpu reg2)))
      (#x05 ; ADD
	 (register-write cpu reg3
			 (+ (register-read cpu reg1)
			    (register-read cpu reg2))))
      (#x06 ; SUB, never used in provided asm
	 (register-write cpu reg3
			 (- (register-read cpu reg1)
			    (register-read cpu reg2))))
      (#x07 ; MUL, never used in provided asm
	 (register-write cpu reg3
			 (* (register-read cpu reg1)
			    (register-read cpu reg2))))
      (#x08 ; DIV
	 (register-write cpu reg3
			 (/ (register-read cpu reg1)
			    (register-read cpu reg2))))
      (#x09 ; AND, never used in provided asm
	 (register-write cpu reg3
			 (logand (register-read cpu reg1)
				 (register-read cpu reg2))))
      (#x0a ; OR, never used in provided asm
	 (register-write cpu reg3
			 (logior (register-read cpu reg1)
				 (register-read cpu reg2))))
      (#x0b ; MOVI
	 (register-write cpu reg2 reg3))
      (#x0c ; ADDI
	 (register-write cpu reg2 (+ reg3 (register-read cpu reg2))))
      (#x0d ; MULI, never used in provided asm
	 (register-write cpu reg2 (* reg3 (register-read cpu reg2))))
      (#x0e ; DIVI, never used in provided asm
	 (register-write cpu reg2 (/ reg3 (register-read cpu reg2))))
      (#x0f ; LDI
	 (register-write cpu reg2
			 (read-hex-from-string
			  (memory-read *memory* (address cpu reg3)))))
      (#x10 ; SLT
	 (if (< (register-read cpu reg1)
		(register-read cpu reg2))
	     (register-write cpu reg3 1)
	     (register-write cpu reg3 0)))
      (#x11 ; SLTI, never used in provided asm
	 (if (< (register-read cpu reg1) reg2)
	     (register-write cpu reg3 1)
	     (register-write cpu reg3 0)))
      (#x12 ; HLT
	 (short-scheduler cpu))
      (#x13 ; NOP, never used in provided asm
	 nil)
      (#x14 ; JMP, never used in provided asm
	 ())
      (#x15 ; BEQ
	 (when (= (register-read cpu reg1)
		  (register-read cpu reg2))
	   (setf (pc cpu) (address cpu reg3 :with-base nil))))
      (#x16 ; BNEQ
	 (unless (= (register-read cpu reg1)
		    (register-read cpu reg2))
	   (setf (pc cpu) (address cpu reg3 :with-base nil))))
      (#x17 ; BEZ, never used in provided asm
	 (when (zerop (register-read cpu reg2))
	   (setf (pc cpu) (address cpu reg3 :with-base nil))))
      (#x18 ; BNZ, never used in provided asm
	 (unless (zerop (register-read cpu reg1))
	   (setf (pc cpu) (address cpu reg3 :with-base nil))))
      (#x19 ; BGZ, never used in provided asm
	 (when (> (register-read cpu reg1) 0)
	   (setf (pc cpu) (address cpu reg3 :with-base nil))))
      (#x1a ; BLZ, never used in provided asm
	 (when (< (register-read cpu reg1) 0)
	   (setf (pc cpu) (address cpu reg3 :with-base nil)))))))

(defun reset ()
  (clear-all-data)
  (setf *cpu1* (make-instance 'cpu)))
