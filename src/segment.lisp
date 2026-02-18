(in-package :infinite-buffers)

(defvar *default-buffer* (make-octet-array 0))

(deftype legal-base-address () '(and fixnum (or (satisfies zerop) (satisfies power-of-two-p))))

(declaim (inline copy-struct-buffer copy-class-buffer copy-standard-buffer
		 struct-buffer-store struct-buffer-base-address struct-buffer-last-address))
  
(defstruct segment
  (manager nil :read-only t)
  (target nil)
  (base 0 :type legal-base-address)
  (last 0 :type legal-base-address)
  (position 0 :type fixnum))

(defun within-bounds (seg requested-addr)
  (declare (type segment seg)
	   (fixnum requested-addr)
	   (optimize (speed 3)))
  (and (<= (segment-base-addr seg) requested-addr)
       (<= requested-addr (segment-last-addr seg))))

(defun target-and-address (seg requested-addr)
  (declare (type segment seg)
	   (fixnum requested-addr)
	   (optimize (speed 3)))
  (if (within-bounds seg requested-addr)
      (values (segment-target seg) (- requested-addr (segment-base-addr seg)))
      (progn
	(update-segment (segment-manager seg) seg requested-addr :absolute)
	(values (segment-target seg) (- requested-addr (segment-base-addr seg))))))

(defun next-target-and-address (seg)
  (declare (type segment seq)
	   (optimize (speed 3)))
  (if (within-bounds seq (segment-relative-addr seg))
      (values (segment-target seg) (post++ (segment-relative-addr seg)))
      (progn
	(update-segment (segment-manager seg) seg (segment-relative-addr seg) :relative)
	(values (sement-target seg) (post++ (segment-relative-addr seg))))))

