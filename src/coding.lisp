(in-package :infinite-buffers)

(declaim (inline max-uint min-uint max-sint min-sint sint->uint uint->sint
		 sint->zigzag zigzag->sint boolean->octet octet->boolean))

(defun max-uint (size)
  (declare (type size int-size))
  (1- (expt 2 (ash size 3))))

(defun min-uint (size) 0)

(defun max-sint (size)
  (declare (type size int-size))
  (1- (expt 2 (1- (ash size 3)))))

(defun min-sint (size)
  (declare (type size int-size))
  (- (expt 2 (1- (ash size 3)))))

(defun sint->uint (size val)
  (declare (type size int-size)
	   (type integer val)
	   (optimize (speed 3)))
  (mask-field (byte (ash size 3) 0) val))

(defun uint->sint (size val)
  (declare (type size int-size)
	   (type integer val)
	   (optimize speed 3))
  (if (logbitp (1- (ash size 3)) val)
      (+ (min-sint size) (deposit-field 0 (byte (ash size 3) (1- (ash size 3))) val))
      val))

(defun sint->zigzag (val)
  (declare (type integer val)
	   (optimize speed 3))
  (if (<= 0 val)
      (ash val 1)
      (1- (ash (- val) 1))))

(defun zigzag->sint (val)
  (declare (type integer val)
	   (optimize speed 3))
  (if (evenp val)
      (ash val -1)
      (- (ash (1+ val) -1))))

(defun boolean->octet (val)
  (declare (type boolean val)
	   (optimize speed 3))
  (if val 1 0))

(defun octet->boolean (val)
  (declare (type octet val)
	   (optimize speed 3))
  (if (= 1 val) t nil))

(defun %int->writer-fixed (encoding size writer-sym target-sym at-sym val-sym)
  (declare (type int-size size))
  (loop with bits = (ash size 3)
	with loads = (loop for bit-index from 0 below bits by 8
			   collecting `(ldb (byte ,bits ,bit-index) ,val-sym))
	for load in (if (eq encoding :little-endian) loads (reverse loads))
	for index from 0 below size
	collecting `(,writer-sym ,target-sym ,@(if at-sym `((+ ,index ,at-sym)) nil) ,load) into actions
	finally (return `(progn ,@actions ,size))))

(defun %int->writer-variable (encoding writer-sym target-sym at-sym val-sym)
  (let ((mask (lognot #x7f)))
    (once-only ((num `,val-sym)
		(index 0))
      `(loop while (not (zerop (logand ,num ,mask)))
	     do (,writer-sym ,target-sym ,@(if at-sym `((+ ,index ,at-sym)) nil) (logior (logand ,num #x7f) #x80))
		(setf ,num (ash ,num -7))
		(incf ,index)
	     finally (,writer-sym ,target-sym ,@(if at-sym `((+ ,index ,at-sym)) nil) ,num)
		     (return (1+ ,index))))))

(defmacro int->writer (encoding size writer target at val)
  (declare (type byte-encoding encoding))
  
  (once-only (writer target val)
    (ecase encoding
      (:little-endian :big-endian
       (if (null at)
	   (%int->writer-fixed encoding size writer target nil val)
	   (once-only (at)
	     (%int->writer-fixed encoding size writer target at val))))
      (:variable-length
       (if (null at)
	   (%int->writer-variable encoding writer target nil val)
	   (once-only (at)
	     (%int->writer-variable encoding writer target at val)))))))

(defmacro octet->writer (writer target at val)
  `(progn (,writer ,target ,@(if at `(,at) nil) ,val)
	  1))

(defmacro octets->writer (writer target at src &key (start 0) (end '(length src)))
  (with-unique-names (src-index)
    (once-only (writer target src start)
      (if at
	  (with-unique-names (at-index)
	    `(loop for ,src-index from ,start below ,end
		   for ,at-index = ,at then (1+ ,at-index)
		   do (,writer ,target ,at-index (aref ,src ,src-index))
		   finally (return (- ,src-index ,start))))
	  `(loop for ,src-index from ,start below ,end
		 do (,writer ,target (aref ,src ,src-index))
		 finally (return (- ,src-index ,start)))))))

(defmacro str->prefix-utf8->writer (encoding prefix-size writer target at str)
  )
