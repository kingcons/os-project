(in-package :os-project)

;; The VM machine word is a hex string
(deftype machine-word ()
  '(simple-array character (8)))

;; Process Control Block, a process metadata store
(defvar *pcb* (make-hash-table :test #'equal))

;; Storage abstraction for memory/disk management
(defclass storage-device ()
  ((storage
    :initform (error "Must supply a size.")
    :initarg :size)))

(defmethod initialize-instance :after ((device storage-device) &key)
  (let ((size (slot-value device 'storage)))
    (setf (slot-value device 'storage)
	  (make-array size :fill-pointer 0 :element-type 'machine-word))))

;; Storage Devices
(defvar *memory* (make-instance 'storage-device :size 1024))
(defvar *disk* (make-instance 'storage-device :size 2048))

;; Temporary hack until Phase 2
(defmethod memory-read ((device storage-device) index)
  (aref (slot-value device 'storage) index))

(defmethod memory-write ((device storage-device) index value)
  (setf (aref (slot-value device 'storage) index) value))

(defmethod memory-push-end ((device storage-device) value)
  (vector-push value (slot-value device 'storage)))

(defmethod memory-index ((device storage-device))
  (fill-pointer (slot-value device 'storage)))

;; Process state object to be stored in the PCB.
;; Potentially also store parent process, child process list.
(defclass process-state ()
  ((ins-count
    :initarg :ins-count
    :initform (error "Must supply instruction count.")
    :reader ins-count)
   (priority
    :initarg :priority
    :initform (error "Must supply priority.")
    :reader priority)
   (start-disk
    :initarg :start-disk
    :initform (error "Must supply disk start index.")
    :reader start-disk)
   (start-ram :accessor start-ram)
   (data-count :accessor data-count)
   (data-buffer :accessor data-buffer)
   (scratchpad :accessor scratchpad)
   (status :accessor status)))
