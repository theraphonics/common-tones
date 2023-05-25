;;; ncos

(defclass ncos ()
  ((n :initform nil :initarg :n :accessor ncosp-n)
   (scaler :initform nil :initarg :scaler :accessor ncosp-scaler)
   (phase :initform nil :initarg :phase :accessor ncosp-phase)
   (freq :initform nil :initarg :freq :accessor ncosp-freq)))

(defmethod print-object ((d ncos) stream)
  (format stream "#<ncos: ~A, n: ~A, scaler: ~A>"
	  (prettified-freq (ncosp-freq d) (ncosp-phase d))
	  (ncosp-n d) (prettified-float (ncosp-scaler d))))

(def-optkey-fun make-ncos ((frequency *clm-default-frequency*) (n 1))
  (let ((cs (make-instance 'ncos
			   :n n
			   :freq (hz->radians frequency)
			   :phase 0.0)))
    (if (zerop n) (warn "ncos with 0 n?"))
    (setf (ncosp-scaler cs) (/ 1.0 n))
    cs))

(defmethod ncos? ((g ncos)) t)
(defmethod ncos? ((g t)) nil)

(defun ncos (cs &optional (fm 0.0))
  (let* ((den (sin (* (ncosp-phase cs) 0.5)))
	 (val (if (= 0.0 den)
		  1.0
		(min 1.0 (* (ncosp-scaler cs)
			    (- (/ (sin (* (ncosp-phase cs)
					  (+ (ncosp-n cs) .5)))
				  (* 2.0 den))
			       0.5))))))
    (incf (ncosp-phase cs) (+ (ncosp-freq cs) fm))
    (if (> (ncosp-phase cs) two-pi) (decf (ncosp-phase cs) two-pi))
    (if (< (ncosp-phase cs) (- two-pi)) (incf (ncosp-phase cs) two-pi))
    val))

(defmethod mus-frequency ((gen ncos)) (radians->hz (ncosp-freq gen)))
(defmethod (setf mus-frequency) (val (gen ncos)) (setf (ncosp-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen ncos)) (ncosp-freq gen))
(defmethod (setf mus-increment) (val (gen ncos)) (setf (ncosp-freq gen) val) val)
(defmethod mus-phase ((gen ncos)) (mod (ncosp-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen ncos)) (setf (ncosp-phase gen) val) val)
(defmethod mus-length ((gen ncos)) (ncosp-n gen))
(defmethod (setf mus-length) (val (gen ncos)) (setf (ncosp-n gen) val) (setf (ncosp-scaler gen) (/ 1.0 val)) val)
(defmethod mus-scaler ((gen ncos)) (ncosp-scaler gen))
(defmethod (setf mus-scaler) (val (gen ncos)) (setf (ncosp-scaler gen) val))

(defmethod mus-run ((gen ncos) &optional (arg1 0.0) (arg2 0.0))
  (declare (ignore arg2))
  (ncos gen arg1))
