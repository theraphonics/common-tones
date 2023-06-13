(in-package :common-tones)


(defclass nrxycos ()
  ((phase :initform nil :initarg :phase :accessor nrxy-phase)
   (freq :initform nil :initarg :freq :accessor nrxy-freq)
   (r :initform nil :initarg :r :accessor nrxy-r)
   (n :initform nil :initarg :n :accessor nrxy-n)
   (ratio :initform nil :initarg :ratio :accessor nrxy-ratio)))

(defmethod print-object ((d nrxycos) stream)
  (format stream "#<nrxycos: ~A, r: ~A, n: ~A, ratio: ~A>"
	  (prettified-freq (nrxy-freq d) (nrxy-phase d))
	  (prettified-float (nrxy-r d))
	  (prettified-float (nrxy-n d))
	  (prettified-float (nrxy-ratio d))))

(def-optkey-fun make-nrxycos ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (make-instance 'nrxycos
		 :freq (hz->radians frequency)
		 :phase 0.0
		 :r r :n n :ratio ratio))

(defmethod nrxycos? ((g nrxycos)) t)
(defmethod nrxycos? ((g t)) nil)

(defun nrxycos (gen &optional (fm 0.0))
  (let* ((x (nrxy-phase gen))
	 (y (* x (nrxy-ratio gen)))
	 (n (nrxy-n gen))
	 (r (nrxy-r gen))
	 (norm (/ (- (expt (abs r) (+ n 1)) 1) (- (abs r) 1))))
    (setf (nrxy-phase gen) (+ x (nrxy-freq gen) fm))
    (/ (- (cos x)
	  (* r (cos (- x y)))
	  (* (expt r (1+ n))
	     (- (cos (+ x (* (1+ n) y)))
		(* r (cos (+ x (* n y)))))))
       (* norm
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defmethod mus-frequency ((gen nrxycos)) (radians->hz (nrxy-freq gen)))
(defmethod (setf mus-frequency) (val (gen nrxycos)) (setf (nrxy-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen nrxycos)) (nrxy-freq gen))
(defmethod (setf mus-increment) (val (gen nrxycos)) (setf (nrxy-freq gen) val) val)
(defmethod mus-phase ((gen nrxycos)) (mod (nrxy-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen nrxycos)) (setf (nrxy-phase gen) val) val)
(defmethod mus-run ((gen nrxycos) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (nrxycos gen arg1))
(defmethod mus-scaler ((gen nrxycos)) (nrxy-r gen))
(defmethod (setf mus-scaler) (val (gen nrxycos)) (setf (nrxy-r gen) val) val)
(defmethod mus-length ((gen nrxycos)) (nrxy-n gen))
(defmethod mus-offset ((gen nrxycos)) (nrxy-ratio gen))



(defclass asymmetric-fm ()
  ((r :initform nil :initarg :r :accessor asymfm-r)
   (freq :initform nil :initarg :freq :accessor asymfm-freq)
   (ratio :initform nil :initarg :ratio :accessor asymfm-ratio)
   (phase :initform nil :initarg :phase :accessor asymfm-phase)
   (cosr :initform nil :initarg :cosr :accessor asymfm-cosr)
   (sinr :initform nil :initarg :sinr :accessor asymfm-sinr)))

(defmethod print-object ((d asymmetric-fm) stream)
  (format stream "#<asymmetric-fm: ~A, ratio: ~A, r: ~A, cosr: ~A, sinr: ~A>"
	  (prettified-freq (asymfm-freq d) (asymfm-phase d))
	  (prettified-float (asymfm-ratio d))
	  (prettified-float (asymfm-r d))
	  (prettified-float (asymfm-cosr d))
	  (prettified-float (asymfm-sinr d))))

(def-optkey-fun make-asymmetric-fm ((frequency *clm-default-frequency*) (initial-phase 0.0) (r 1.0) (ratio 1.0))
  (if (/= r 0.0)
      (make-instance 'asymmetric-fm
		     :r r
		     :freq (hz->radians frequency)
		     :ratio ratio
		     :phase initial-phase
		     :cosr (* .5 (- r (/ 1.0 r)))
		     :sinr (* .5 (+ r (/ 1.0 r))))))

(defmethod asymmetric-fm? ((g asymmetric-fm)) t)
(defmethod asymmetric-fm? ((g t)) nil)

(defun asymmetric-fm (af index &optional (fm 0.0))
  (let* ((th (asymfm-phase af))
	 (mth (* (asymfm-ratio af) th))
	 (cr (asymfm-cosr af))
	 (sr (asymfm-sinr af))
	 (result (* (exp (* index cr (+ 1.0 (cos mth)))) (cos (+ th (* sr index (sin mth)))))))
    (incf (asymfm-phase af) (+ (asymfm-freq af) fm))
    (when (or (> (asymfm-phase af) 100.0) (< (asymfm-phase af) -100.0))
      (setf (asymfm-phase af) (mod (asymfm-phase af) two-pi)))
    result))

(defmethod mus-frequency ((gen asymmetric-fm)) (radians->hz (asymfm-freq gen)))
(defmethod (setf mus-frequency) (val (gen asymmetric-fm)) (setf (asymfm-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen asymmetric-fm)) (asymfm-freq gen))
(defmethod (setf mus-increment) (val (gen asymmetric-fm)) (setf (asymfm-freq gen) val) val)
(defmethod mus-phase ((gen asymmetric-fm)) (mod (asymfm-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen asymmetric-fm)) (setf (asymfm-phase gen) val) val)
(defmethod mus-run ((gen asymmetric-fm) &optional (arg1 0.0) (arg2 0.0)) (asymmetric-fm gen arg1 arg2))
(defmethod mus-scaler ((gen asymmetric-fm)) (asymfm-r gen))
(defmethod (setf mus-scaler) (val (gen asymmetric-fm))
  (when (/= val 0.0)
    (setf (asymfm-r gen) val)
    (setf (asymfm-cosr gen) (* .5 (- val (/ 1.0 val))))
    (setf (asymfm-sinr gen) (* .5 (+ val (/ 1.0 val)))))
  val)
(defmethod mus-offset ((gen asymmetric-fm)) (asymfm-ratio gen))