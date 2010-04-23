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
   (job-io
    :initform (cons 0 0)
    :accessor job-io)
   (running-job
    :initform nil
    :accessor job-id)))

(defmethod registers-clear ((cpu cpu))
  (setf (registers cpu)	(make-array 16 :element-type 'register)))

(defmethod reg-w ((cpu cpu) index value)
  (setf (aref (registers cpu) index) value))

(defmethod reg-r ((cpu cpu) index)
  (aref (registers cpu) index))

(defmethod address ((cpu cpu) addr &key (with-base t))
  (if with-base
      (+ (breg cpu) (/ addr 4))
      (/ addr 4)))

(defmethod fetch ((cpu cpu))
  (setf (ireg cpu) (read-hex-from-string
		    (memory-read *memory* (+ (breg cpu) (pc cpu)))))
  (incf (pc cpu)))

(defun parse-instruction (instruction &key (string-p nil) (ins-data nil))
  (let* ((ins (if string-p
		  (read-hex-from-string instruction)
		  instruction))
	 (ins-type (ldb (byte 2 30) ins))
	 (opcode (ldb (byte 6 24) ins))
	 (arg1 0) (arg2 0) (arg3 0)
	 (hexstr (when string-p (format nil "0x~8a: " instruction))))
    (flet ((bit-range (width position)
	     (ldb (byte width position) ins)))
      (ecase ins-type
	(#b00
	   (setf arg1 (bit-range 4 20))
	   (setf arg2 (bit-range 4 16))
	   (setf arg3 (bit-range 4 12))
	   (when ins-data
	     (setf ins-data (format nil "Arithmetic         IF: s-reg s-reg d-reg, ~2a ~2a ~2a"
				    4 4 4))))
	(#b01
	   (setf arg1 (bit-range 4 20))
	   (setf arg2 (bit-range 4 16))
	   (setf arg3 (bit-range 16 0))
	   (when ins-data
	     (setf ins-data (format nil "Cond and Immediate IF: b-reg d-reg addr, ~2a ~2a ~2a"
				    4 4 16))))
	(#b10
	   (setf arg1 (bit-range 24 0))
	   (when ins-data
	     (setf ins-data (format nil "Unconditional Jump IF: addr, ~2a" 24))))
	(#b11
	   (setf arg1 (bit-range 4 20))
	   (setf arg2 (bit-range 4 16))
	   (setf arg3 (bit-range 16 0))
	   (when ins-data
	     (setf ins-data (format nil "Input and Output   IF: reg1 reg2 addr, ~2a ~2a ~2a"
				    4 4 16))))))
    (list opcode arg1 arg2 arg3 ins-data hexstr ins-type)))

(defmethod decode ((cpu cpu))
  (let* ((parsed-ins (parse-instruction (ireg cpu)))
	 (opcode (first parsed-ins))
	 (arg1 (second parsed-ins))
	 (arg2 (third parsed-ins))
	 (arg3 (fourth parsed-ins)))
    (ecase opcode
      ;; Finally, some macros to tidy this up might be nice.
      (#x00 ; RD
	 (profile
	   (reg-w cpu arg1
		  (read-hex-from-string
		   (memory-read *memory* (address cpu (if (zerop arg2)
							  arg3
							  (reg-r cpu arg2))))))))
      (#x01 ; WR
	 (profile
	   (memory-write *memory* (address cpu (if (zerop arg2)
						   arg3
						   (reg-r cpu arg2)))
			 (to-hex-string (reg-r cpu arg1)))))
      (#x02 ; ST
	 (profile
	   (memory-write *memory*
			 (address cpu (reg-r cpu arg2))
			 (to-hex-string (reg-r cpu arg1)))))
      (#x03 ; LW
	 (profile
	   (reg-w cpu arg2
			   (read-hex-from-string
			    (memory-read *memory*
					 (address cpu (reg-r cpu arg1)))))))
      (#x04 ; MOV, INCOMPLETE? see peculiar use case in Job 7
	 (reg-w cpu arg1 (reg-r cpu arg2)))
      (#x05 ; ADD
	 (reg-w cpu arg3 (+ (reg-r cpu arg1) (reg-r cpu arg2))))
      (#x06 ; SUB, never used in provided asm
	 (reg-w cpu arg3 (- (reg-r cpu arg1) (reg-r cpu arg2))))
      (#x07 ; MUL, never used in provided asm
	 (reg-w cpu arg3 (* (reg-r cpu arg1) (reg-r cpu arg2))))
      (#x08 ; DIV
	 (reg-w cpu arg3 (round (/ (reg-r cpu arg1)
				   (reg-r cpu arg2)))))
      (#x09 ; AND, never used in provided asm
	 (reg-w cpu arg3 (logand (reg-r cpu arg1)
				 (reg-r cpu arg2))))
      (#x0a ; OR, never used in provided asm
	 (reg-w cpu arg3 (logior (reg-r cpu arg1)
				 (reg-r cpu arg2))))
      (#x0b ; MOVI
	 (reg-w cpu arg2 arg3))
      (#x0c ; ADDI
	 (reg-w cpu arg2 (+ arg3 (reg-r cpu arg2))))
      (#x0d ; MULI, never used in provided asm
	 (reg-w cpu arg2 (* arg3 (reg-r cpu arg2))))
      (#x0e ; DIVI, never used in provided asm
	 (reg-w cpu arg2 (round (/ arg3 (reg-r cpu arg2)))))
      (#x0f ; LDI
	 (reg-w cpu arg2 arg3))
      (#x10 ; SLT
	 (if (< (reg-r cpu arg1) (reg-r cpu arg2))
	     (reg-w cpu arg3 1)
	     (reg-w cpu arg3 0)))
      (#x11 ; SLTI, never used in provided asm
	 (if (< (reg-r cpu arg1) arg2)
	     (reg-w cpu arg3 1)
	     (reg-w cpu arg3 0)))
      (#x12 ; HLT
	 (if *profiling*
	     (move-job (gethash (job-id cpu) *pcb*) :type :save
		                                    :job-io (job-io cpu))
	     (move-job (gethash (job-id cpu) *pcb*) :type :save))
	 (setf (job-io cpu) (cons 0 0))
	 (setf (job-id cpu) nil)
	 (sb-thread:with-mutex (*ss-mutex*)
	   (short-scheduler cpu)))
      (#x13 ; NOP, never used in provided asm
	 nil)
      (#x14 ; JMP, never used in provided asm
	 ()) ;; TODO: should reset PC and set barg to 0,
             ;; resetting barg after the next fetch
      ;; Better idea: Use a kwarg to bypass barg...on the next fetch.
      (#x15 ; BEQ
	 (when (= (reg-r cpu arg1) (reg-r cpu arg2))
	   (setf (pc cpu) (address cpu arg3 :with-base nil))))
      (#x16 ; BNEQ
	 (unless (= (reg-r cpu arg1) (reg-r cpu arg2))
	   (setf (pc cpu) (address cpu arg3 :with-base nil))))
      (#x17 ; BEZ, never used in provided asm
	 (when (zerop (reg-r cpu arg2))
	   (setf (pc cpu) (address cpu arg3 :with-base nil))))
      (#x18 ; BNZ, never used in provided asm
	 (unless (zerop (reg-r cpu arg1))
	   (setf (pc cpu) (address cpu arg3 :with-base nil))))
      (#x19 ; BGZ, never used in provided asm
	 (when (> (reg-r cpu arg1) 0)
	   (setf (pc cpu) (address cpu arg3 :with-base nil))))
      (#x1a ; BLZ, never used in provided asm
	 (when (< (reg-r cpu arg1) 0)
	   (setf (pc cpu) (address cpu arg3 :with-base nil)))))))
