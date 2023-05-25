(in-package :common-tones/generators)

(defclass nrxysin ()
  ((phase :initform nil :initarg :phase :accessor nrxy-phase)
   (freq :initform nil :initarg :freq :accessor nrxy-freq)
   (r :initform nil :initarg :r :accessor nrxy-r)
   (n :initform nil :initarg :n :accessor nrxy-n)
   (ratio :initform nil :initarg :ratio :accessor nrxy-ratio)))

(defmethod print-object ((d nrxysin) stream)
  (format stream "#<nrxysin: ~A, r: ~A, n: ~A, ratio: ~A>"
	  (prettified-freq (nrxy-freq d) (nrxy-phase d))
	  (prettified-float (nrxy-r d))
	  (prettified-float (nrxy-n d))
	  (prettified-float (nrxy-ratio d))))

(def-optkey-fun make-nrxysin ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (make-instance 'nrxysin
		 :freq (hz->radians frequency)
		 :phase 0.0
		 :r r :n n :ratio ratio))

(defmethod nrxysin? ((g nrxysin)) t)
(defmethod nrxysin? ((g t)) nil)

(defun nrxysin (gen &optional (fm 0.0))
  (let* ((x (nrxy-phase gen))
	 (y (* x (nrxy-ratio gen)))
	 (n (nrxy-n gen))
	 (r (nrxy-r gen))
	 (norm (/ (- (expt (abs r) n) 1) (- (abs r) 1))))
    (setf (nrxy-phase gen) (+ x (nrxy-freq gen) fm))
    (/ (- (sin x)
	  (* r (sin (- x y)))
	  (* (expt r (1+ n))
	     (- (sin (+ x (* (1+ n) y)))
		(* r (sin (+ x (* n y)))))))
       (* norm
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defmethod mus-frequency ((gen nrxysin)) (radians->hz (nrxy-freq gen)))
(defmethod (setf mus-frequency) (val (gen nrxysin)) (setf (nrxy-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen nrxysin)) (nrxy-freq gen))
(defmethod (setf mus-increment) (val (gen nrxysin)) (setf (nrxy-freq gen) val) val)
(defmethod mus-phase ((gen nrxysin)) (mod (nrxy-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen nrxysin)) (setf (nrxy-phase gen) val) val)
(defmethod mus-run ((gen nrxysin) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (nrxysin gen arg1))
(defmethod mus-scaler ((gen nrxysin)) (nrxy-r gen))
(defmethod (setf mus-scaler) (val (gen nrxysin)) (setf (nrxy-r gen) val) val)
(defmethod mus-length ((gen nrxysin)) (nrxy-n gen))
(defmethod mus-offset ((gen nrxysin)) (nrxy-ratio gen))
