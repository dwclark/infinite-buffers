(in-package :infinite-buffers)

(defconstant +standard-size+ (expt 2 16))
(defconstant +medium-size+ (expt 2 20))
(defconstant +large-size+ (expt 2 24))
(defconstant +jumbo-size+ (expt 2 28))

(declaim (inline power-of-two-p power-of-two-array-p))

(declaim (ftype (function (fixnum) boolean) power-of-two-p power-of-two-1-p))
(defun power-of-two-p (num)
  (declare (type fixnum num))
  (and (< 1 num) (= 1 (logcount num))))

(defun power-of-two-array-p (ary)
  (power-of-two-p (array-dimension ary 0)))

(deftype octet () '(unsigned-byte 8))
(deftype octet-array () '(simple-array octet *))
(deftype fixed-octet-array (size) `(simple-array octet (,size)))
(deftype standard-octet-array () `(fixed-octet-array ,+standard-size+))
(deftype medium-octet-array () `(fixed-octet-array ,+medium-size+))
(deftype large-octet-array () `(fixed-octet-array ,+large-size+))
(deftype jumbo-octet-array () `(fixed-octet-array ,+jumbo-size+))
(deftype adjustable-string () '(and (array character *) (satisfies adjustable-array-p)))
(deftype adjustable-octet-array () '(and (array octet *) (satisfies adjustable-array-p)))
(deftype power-of-two-octet-array () '(and octet-array (satisfies power-of-two-array-p)))
(deftype int-size () '(and fixnum (member 1 2 4 8)))
(deftype byte-encoding () '(member :little-endian :big-endian :variable-length))

(defun make-octet-array (type-or-size)
  (let ((array-size (case type-or-size
		      (standard-octet-array +standard-size+)
		      (medium-octet-array +medium-size+)
		      (large-octet-array +large-size+)
		      (jumbo-octet-array +jumbo-size+)
		      (t type-or-size))))
    (make-array array-size :element-type 'octet)))

(defmacro post++ (form)
  `(prog1 ,form (incf ,form)))

;; try out babel crap
(defmacro get-octet (ary index)
  `(aref ,ary (post++ (fill-pointer ,ary))))

(defmacro set-octet (code ary index)
  `(vector-push-extend ,code ,ary))

(defmacro get-char (str index)
  `(char-code (aref ,str (post++ (fill-pointer ,str)))))

(defmacro set-char (code str index)
  `(vector-push-extend (code-char ,code) ,str))

(defun demo-babel ()
  (let* ((src (make-array 5 :element-type 'octet :initial-contents '(#x41 #x42 #x43 #x44 #x45) :adjustable t :fill-pointer 0))
	 (target (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
	 (map (babel-encodings:instantiate-concrete-mappings :encodings (:utf-8)
							     :octet-seq-getter get-octet
							     :octet-seq-setter set-octet
							     :octet-seq-type adjustable-octet-array
							     :code-point-seq-getter get-char
							     :code-point-seq-setter set-char
							     :code-point-seq-type adjustable-string))
	 (mapping (gethash :utf-8 map))
	 (func (babel-encodings:decoder mapping)))

    (funcall func src 0 5 target 0)
    target))
