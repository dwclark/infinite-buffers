(in-package :infinite-buffers)

(defgeneric io-position (this &optional pos))
(defgeneric io-in (this ary &key start end))
(defgeneric io-out (this ary &key start end))
(defgeneric io-close (this))

(defclass file-sequence-io ()
  ((filename :initarg :filename :accessor filename)
   (filestream :reader filestream)))

(defmethod initialize-instance :after ((this file-sequence-io) &key)
  (let ((name (slot-value this 'filename)))
    (setf (slot-value this 'filestream)
	  (open name :direction :io :element-type 'octet :if-exists :overwrite))))

(defmethod io-position ((this file-sequence-io) &optional (pos -1))
  (if (<= 0 pos)
      (file-position (slot-value this 'filestream) pos)
      (file-position (slot-value this 'filestream))))

(defmethod io-in ((this file-sequence-io) ary &key (start 0) (end (array-dimension ary 0)))
  (declare (type octet-array ary))
  (read-sequence ary (slot-value this 'filestream) :start start :end end))

(defmethod io-out ((this file-sequence-io) ary &key (start 0) (end (array-dimension ary 0)))
  (declare (type octet-array ary))
  (write-sequence ary (slot-value this 'filestream) :start start :end end))

(defmethod io-close ((this file-sequence-io))
  (close (slot-value this 'filestream))
  (setf (slot-value this 'filestream) nil))

(defclass array-io ()
  ((type-or-size :initarg :type-or-size :accessor type-or-size)
   (backing-array :reader backing-array)
   (current-position :reader current-position)))

(defmethod initialize-instance :after ((this array-io) &key)
  (setf (slot-value this 'backing-array) (make-octet-array (slot-value this 'type-or-size))
	(slot-value this 'position) 0))

(defmethod io-position ((this array-io) &optional (pos -1))
  (if (<= 0 pos)
      (setf (slot-value this 'current-position) pos)
      (slot-value this 'current-position)))

;;todo add type declarations
(defmethod io-in ((this file-sequence-io) ary &key (start 0) (end (array-dimension ary 0)))
  (declare (type octet-array ary))
  (with-slots (current-position backing-array) this 
    (loop with num-bytes = (min (- end start) (- (array-dimension backing-array 0) current-position))
	  with ary-position = start
	  for index from current-position below (+ current-position num-bytes)
	  do (setf (aref backing-array index) (aref ary (post++ ary-position)))
	  finally (setf current-position index))))

#|(defmethod io-out ((this file-sequence-io) ary &key (start 0) (end (array-dimension ary 0)))
  (declare (type octet-array ary))
  (with-slots (current-position backing-array count) this 

|#
