(in-package :common-tones/generators)
/*!< single sideband suppressed carrier amplitude modulation


(defclass ssb-am ()
  ((up :initform nil :initarg :up :accessor ssb-am-up)
   (sin-osc :initform nil :initarg :sin-osc :accessor ssb-am-sin-osc)
   (cos-osc :initform nil :initarg :cos-osc :accessor ssb-am-cos-osc)
   (dly :initform nil :initarg :dly :accessor ssb-am-dly)
   (hilbert :initform nil :initarg :hilbert :accessor ssb-am-hilbert)))

(defmethod print-object ((d ssb-am) stream)
  (format stream "#<ssb-am: ~A, order: ~A>"
	  (prettified-freq (oscil-freq (ssb-am-sin-osc d)) (oscil-phase (ssb-am-sin-osc d)))
	  (mus-order (ssb-am-dly d))))

(defun make-hilbert-transform (len)
  (let* ((arrlen (* 2 len))
	 (arr (make-double-float-array arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- 1.0 (cos (* pi i)))))
	(if (= i 0)
	    (setf (aref arr k) (double 0.0))
	    (setf (aref arr k) (double (* (/ num denom)
					  (+ .54 (* .46 (cos (/ (* i pi) len))))))))))
    (make-fir-filter arrlen arr)))

(def-optkey-fun make-ssb-am ((frequency *clm-default-frequency*) (order 40))
  (make-instance 'ssb-am
		 :up (> frequency 0.0)
		 :sin-osc (make-oscil (abs frequency))
		 :cos-osc (make-oscil (abs frequency) (* 0.5 pi))
		 :dly (make-delay order)
		 :hilbert (make-hilbert-transform order)))

(defmethod ssb-am? ((g ssb-am)) t)
(defmethod ssb-am? ((g t)) nil)

(defun ssb-am (gen &optional (insig 0.0) (fm 0.0))
  (let ((ccos (oscil (ssb-am-cos-osc gen) fm))
	(csin (oscil (ssb-am-sin-osc gen) fm))
	(yh (fir-filter (ssb-am-hilbert gen) insig))
	(yd (delay (ssb-am-dly gen) insig)))
    (if (ssb-am-up gen)
	(- (* ccos yd) ; shift up
	   (* csin yh))
      (+ (* ccos yd) ; shift down
	 (* csin yh)))))

(defmethod mus-run ((gen ssb-am) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (ssb-am gen arg1))
(defmethod mus-frequency ((gen ssb-am)) (mus-frequency (ssb-am-sin-osc gen)))
(defmethod (setf mus-frequency) (val (gen ssb-am))
  (setf (mus-frequency (ssb-am-sin-osc gen)) val)
  (setf (mus-frequency (ssb-am-cos-osc gen)) val))
(defmethod mus-increment ((gen ssb-am)) (mus-increment (ssb-am-sin-osc gen)))
(defmethod (setf mus-increment) (val (gen ssb-am))
  (setf (mus-increment (ssb-am-sin-osc gen)) val)
  (setf (mus-increment (ssb-am-cos-osc gen)) val))
(defmethod mus-phase ((gen ssb-am)) (mus-phase (ssb-am-sin-osc gen)))
(defmethod (setf mus-phase) (val (gen ssb-am))
  (setf (mus-phase (ssb-am-sin-osc gen)) val)
  (setf (mus-phase (ssb-am-cos-osc gen)) (+ val (* 0.5 pi))))
(defmethod mus-data ((gen ssb-am)) (mus-data (ssb-am-dly gen)))
(defmethod mus-length ((gen ssb-am)) (mus-length (ssb-am-dly gen)))
(defmethod mus-order ((gen ssb-am)) (mus-order (ssb-am-dly gen)))
(defmethod mus-interp-type ((gen ssb-am)) mus-interp-none)
(defmethod mus-xcoeffs ((gen ssb-am)) (mus-xcoeffs (ssb-am-hilbert gen)))
(defmethod mus-xcoeff ((gen ssb-am) index) (mus-xcoeff (ssb-am-hilbert gen) index))

(defun mus-a0 (gen) (mus-xcoeff gen 0))
(defun mus-set-a0 (gen val) (setf (mus-xcoeff gen 0) val))
(defsetf mus-a0 mus-set-a0)
(defun mus-a1 (gen) (mus-xcoeff gen 1))
(defun mus-set-a1 (gen val) (setf (mus-xcoeff gen 1) val))
(defsetf mus-a1 mus-set-a1)
(defun mus-a2 (gen) (mus-xcoeff gen 2))
(defun mus-set-a2 (gen val) (setf (mus-xcoeff gen 2) val))
(defsetf mus-a2 mus-set-a2)
(defun mus-b1 (gen) (mus-ycoeff gen 1))
(defun mus-set-b1 (gen val) (setf (mus-ycoeff gen 1) val))
(defsetf mus-b1 mus-set-b1)
(defun mus-b2 (gen) (mus-ycoeff gen 2))
(defun mus-set-b2 (gen val) (setf (mus-ycoeff gen 2) val))
(defsetf mus-b2 mus-set-b2)

(defmethod mus-reset ((gen t))
  (warn "mus-reset is a no-op outside 'run'"))



(defun run-with-fm-and-pm (gen fm pm)
  (setf (mus-phase gen) (+ (mus-phase gen) pm))
  (let ((result (mus-run gen fm)))
    (setf (mus-phase gen) (- (mus-phase gen) pm))
    result))