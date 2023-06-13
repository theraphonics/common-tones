(in-package :common-tones)

;;; Sampling rate conversion (src)

;;; based on kindly advice of Perry Cook -- see his sweep-srate.c. Changed 13-Jan-98 to match prc's code more closely.

;;; Renamed sr-conversion.lisp to avoid file name conflicts


(defmethod mus-file-name ((str t)) str) ; might be string/pathname as file, or nil etc

(defclass src ()
  ((rd :initform nil :initarg :rd :accessor sr-rd)
   (x :initform 0.0 :initarg :x :accessor sr-x)
   (incr :initform 1.0 :initarg :incr :accessor sr-incr)
   (data :initform nil :initarg :data :accessor sr-data)
   (width :initform 5 :initarg :width :accessor sr-width)
   (sinc :initform nil :initarg :sinc :accessor sr-sinc)
   (len :initform nil :initarg :len :accessor sr-len)))

(defmethod print-object ((d src) stream)
  (format stream "#<src: x: ~A, incr: ~A, width: ~A, len: ~A, rd: ~A, data: ~A, sinc: ~A>"
	  (prettified-float (sr-x d))
	  (prettified-float (sr-incr d))
	  (sr-width d) (sr-len d) (sr-rd d)
	  (prettified-array (sr-data d))
	  (prettified-array (sr-sinc d))))

(defmethod src? ((g src)) t)
(defmethod src? ((g t)) nil)

(defvar sinc-density 20)
(defvar *clm-src-width* 5)
(defvar previous-sinc-table nil)
(defvar previous-sinc-table-size -1)

(defun fill-sinc-table (size)
  (if (= size previous-sinc-table-size)
      previous-sinc-table
    (let* ((sinc-table (make-double-array (1+ size)))
	   (win-freq (/ pi size))
	   (sinc-freq (/ pi sinc-density)))
      (setf (aref sinc-table 0) (double 1.0))
      (setf (aref sinc-table size) (double 0.0))
      (loop for i from 1 below size and sf from sinc-freq by sinc-freq and wf from win-freq by win-freq do
	(setf (aref sinc-table i)
	      (double (/ (* (+ 0.5 (* 0.5 (cos wf))) (sin sf)) sf))))
      (setf previous-sinc-table sinc-table)
      (setf previous-sinc-table-size size)
      sinc-table)))

(def-optkey-fun make-src (input (srate 1.0) (width *clm-src-width*))
  ;; input can be a filename, or a mus-input gen, or a function etc
  (let* ((wid (max width (* 2 (ceiling srate))))
	 (size (* wid sinc-density)))
    (make-instance 'src
		   :rd (if (mus-input? input)
			   input
			 (make-file->sample (mus-file-name input)))
		   :x 0.0
		   :incr srate
		   :width wid
		   :sinc (fill-sinc-table size)
		   :len size
		   :data (make-double-array (1+ (* wid 2)) :initial-element 0.0))))

(defun src (s &optional (sr-change 0.0) input-function)
  ;; get data window lined up right, convolve with "warped" sinc
  (let* ((sum 0.0)
	 (loc 0)
	 (srx (+ (sr-incr s) sr-change))
	 (lim (* 2 (sr-width s)))
	 (fsx (floor (sr-x s))))
    (when (> fsx 0)
      (loop for i from fsx below lim do
	(setf (aref (sr-data s) loc) (aref (sr-data s) i))
	(incf loc))
      (if (file->sample? (sr-rd s))
	  (setf (sr-rd s) (make-readin (mus-file-name (sr-rd s)))))
      (if (readin? (sr-rd s))
	  (loop for i from loc below lim do
	    (setf (aref (sr-data s) i) (double (readin (sr-rd s)))))
	(if (or input-function (sr-rd s))
	    (loop for i from loc below lim do
	      (setf (aref (sr-data s) i) (double (funcall (or input-function (sr-rd s)) (if (plusp srx) 1 -1)))))
	  (error "no input source for src?")))
      (decf (sr-x s) fsx))
    ;; now dot-product with (possibly warped) sinc
    (if (minusp srx) (setf srx (- srx)))
    (let* ((factor (if (<= srx 1.0) 1.0 (/ 1.0 srx)))
	   (zf (* factor sinc-density)))
      (loop for i from 0 below lim and x from (* zf (- 1.0 (sr-x s) (sr-width s))) by zf do
	(multiple-value-bind (k frac) (floor (abs x))
	  (if (< k (sr-len s))
	      (incf sum (* (aref (sr-data s) i)
			   (+ (aref (sr-sinc s) k)
			      (* frac (- (aref (sr-sinc s) (1+ k)) (aref (sr-sinc s) k)))))))))
      (incf (sr-x s) srx)
      (* factor sum))))

(defmethod mus-increment ((gen src)) (sr-incr gen))
(defmethod (setf mus-increment) (val (gen src)) (setf (sr-incr gen) val))
(defmethod mus-channel ((gen src)) (if (sr-rd gen) (mus-channel (sr-rd gen))))
(defmethod mus-location ((gen src)) (if (sr-rd gen) (mus-location (sr-rd gen))))
(defmethod (setf mus-location) (val (gen src)) (if (sr-rd gen) (setf (mus-location (sr-rd gen)) (floor val)) val))
(defmethod mus-run ((gen src) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (src gen arg1))