(in-package :infinite-buffers)

(defgeneric update-segment (manager segment requested-addr type))
(defgeneric create-segment (manager &optional requested-addr))

(defstruct struct-buffer
  (base-address 0 :type legal-base-address)
  (last-address 0 :type legal-base-address)
  (store *default-buffer* :type power-of-two-octet-array)
  (at 0 :type fixnum))

;;within striking distance of raw arrays, necessary since
;;passing around raw arrays and tracking offsets via macro
;;magic is going to be error prone to implement and use.
;;bulk operations may make things better. For example, we
;;can offer int operations that do the correct encoding as a parameter.
;;to simplify things, only offer unsigned versions and offer
;;signed->unsigned and unsigned->signed conversions for the first/final conversions.
;;also offer signed->zigzag and zigzag->signed for the first/final conversions
(defun copy-struct-buffer (sb index val)
  (declare (type fixnum index)
	   (type octet val)
	   (optimize (speed 3)))
  (let* ((ary (struct-buffer-store sb))
	 (base (struct-buffer-base-address sb))
	 (last (struct-buffer-last-address sb))
	 (write-at (the fixnum (- index base))))
    (if (< write-at last)
	(setf (aref ary index) val)
	(error "out of bounds"))
    sb))

(defclass class-buffer ()
  ((base-address :initarg :base-address :reader base-address)
   (last-address :initarg :last-address :reader last-address)
   (store :initarg :store :reader store)
   (at :initarg :at :reader at)))

;; this gets really bad when you have to start accessing
;; all of the slots. Looks like really need the struct to organize
;; the fast computation, while the class stuff should only be for
;; course grained policy control/bulk operations
(defun copy-class-buffer (cb index val)
  (declare (type fixnum index)
	   (type octet val)
	   (optimize (speed 3)))
  (let* ((ary (the octet-array (slot-value cb 'store)))
	 (base (the fixnum (slot-value cb 'base-address)))
	 (last (the fixnum (slot-value cb 'last-address)))
	 (write-at (the fixnum (- index base))))
    (if (< write-at last)
	(setf (aref ary index) val)
	(error "out of bounds"))
    cb))

(defun copy-standard-buffer (ary ary-max index val)
  (declare (type octet-array ary)
	   (type fixnum ary-max index)
	   (type octet val)
	   (optimize (speed 3)))
  (if (< index ary-max)
      (setf (aref ary index) val)
      (error "out of bounds"))
  ary)

;; well that's kind of good. struct is basically same as raw when everything is optimized
;; class seems to be 2x slower no matter what. Really need to amortize slot access.
(defun show-timings ()
  (declare (optimize (speed 3)))
  (let* ((num-runs 10000000)
	 (size 64)
	 (to-copy (make-octet-array size))
	 (sb (make-struct-buffer :base-address 0 :last-address size :store (make-octet-array size) :at 0))
	 (cb (make-instance 'class-buffer :base-address 0 :last-address size :store (make-octet-array size) :at 0))
	 (ary (make-octet-array 64)))
    (declare (type struct-buffer sb)
	     (type class-buffer cb)
	     (type octet-array ary))
    
    (dotimes (var size)
      (setf (aref to-copy var) var))

    (format t "Timings for struct buffer~%")
    (time (loop for i from 0 below num-runs
		do (loop for idx from 0 below size
			 do (setf sb (copy-struct-buffer sb idx (aref to-copy idx))))))

    (format t "Timings for class buffer~%")
    (time (loop for i from 0 below num-runs
		do (loop for idx from 0 below size
			 do (setf cb (copy-class-buffer cb idx (aref to-copy idx))))))

    (format t "Timings for standard buffer~%")
    (time (loop for i from 0 below num-runs
		do (loop for idx from 0 below size
			 do (setf ary (copy-standard-buffer ary size idx (aref to-copy idx))))))


    ))


(defmacro stupid (method)
  (let ((foo method))
    `(,foo :blah 1 1.45f)))

(defparameter *config* '(1 2 3 4))
