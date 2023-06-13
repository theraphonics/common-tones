(in-package :common-tones)

(defclass formant ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (a2 :initform nil :initarg :a2 :accessor flt-a2)
   (b1 :initform nil :initarg :b1 :accessor flt-b1)
   (b2 :initform nil :initarg :b2 :accessor flt-b2)
   (x1 :initform 0.0 :initarg :x1 :accessor flt-x1)
   (x2 :initform 0.0 :initarg :x2 :accessor flt-x2)
   (y1 :initform 0.0 :initarg :y1 :accessor flt-y1)
   (y2 :initform 0.0 :initarg :y2 :accessor flt-y2)
   (radius :initform 1.0 :initarg :radius :accessor radius)
   (frequency :initform 1.0 :initarg :frequency :accessor frequency)))

(defmethod print-object ((d formant) stream)
  (format stream "#<(formant: a0: ~A, a2: ~A, b1: ~A, b2: ~A, x1: ~A, x2: ~A, y1: ~A, y2: ~A>"
	  (flt-a0 d) (flt-a2 d) (flt-b1 d) (flt-b2 d) (flt-x1 d) (flt-x2 d) (flt-y1 d) (flt-y2 d)))

(defmethod formant? ((g formant)) t)
(defmethod formant? ((g t)) nil)

(def-optkey-fun make-formant (frequency radius)
  ;; it might be clearer to use bandwidth?
  (if (minusp radius) (setf radius (clm-cerror "use .5" .5 #'(lambda (n) (not (minusp n))) "formant radius = ~,3F is meaningless" radius)))
  (make-instance 'formant
		 :radius radius :frequency frequency
		 :a0 (- 1.0 (* radius radius))
		 :b1 (- (* 2.0 radius (cos (hz->radians frequency))))
		 :b2 (* radius radius)))

(defun formant (f input &optional freq)
  (if freq
      (setf (mus-frequency f) freq))
  (let* ((inval (* (flt-a0 f) input))
	 (tpinval (+ inval - (flt-x2 f)))
	 (output (- tpinval (* (flt-b1 f) (flt-y1 f)) (* (flt-b2 f) (flt-y2 f)))))
    (setf (flt-y2 f) (flt-y1 f))
    (setf (flt-y1 f) output)
    (setf (flt-x2 f) (flt-x1 f))
    (setf (flt-x1 f) inval)
    output))

(defmethod mus-frequency ((gen formant))
  (frequency gen))

(defmethod (setf mus-frequency) (val (gen formant))
  (let ((fw (hz->radians val))
	(R (radius gen)))
    (setf (frequency gen) val)
    (setf (flt-b1 gen) (* -2.0 R (cos fw))))
  val)

(defmethod mus-order ((gen formant)) 2)

(defmacro formant-bank (amps frms inval)
  `(let ((sum 0.0)
	 (len (length ,frms)))
     (loop for i from 0 below len do (incf sum (* (aref ,amps i) (formant (aref ,frms i) ,inval))))
     sum))

(defmethod mus-run ((gen formant) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (formant gen arg1))

(defmethod mus-scaler ((gen formant))
  (radius gen))

(defmethod (setf mus-scaler) (val (gen formant))
  (setf (radius gen) val)
  (setf (flt-a0 gen) (- 1.0 (* val val)))
  (setf (flt-b1 gen) (- (* 2.0 val (cos (hz->radians (frequency gen))))))
  (setf (flt-b2 gen) (* val val))
  val)



(defclass firmant ()
  ((radius :initform 0.9 :initarg :radius :accessor frm-radius)
   (frequency :initform 1000.0 :initarg :frequency :accessor frm-frequency)
   (eps :initform 1.0 :initarg :eps :accessor frm-eps)
   (xn :initform 0.0 :initarg :x1 :accessor frm-xn)
   (yn :initform 0.0 :initarg :y1 :accessor frm-yn)
   (gain :initform 1.0 :initarg :gain :accessor frm-gain)))


(defmethod print-object ((d firmant) stream)
  (format stream "#<(firmant: frequency: ~A, radius: ~A>"
	  (frm-frequency d) (frm-radius d)))

(defmethod firmant? ((g firmant)) t)
(defmethod firmant? ((g t)) nil)

(def-optkey-fun make-firmant (frequency radius)
  (make-instance 'firmant
		 :radius radius
		 :frequency frequency
		 :eps (* 2.0 (sin (* 0.5 (hz->radians frequency))))
		 :gain (- 1.0 (* radius radius))))

(defun firmant (m input &optional freq)
  (if freq
      (setf (mus-frequency m) freq))
  (let* ((xn1 (+ (* (frm-gain m) input)
		 (* (frm-radius m)
		    (- (frm-xn m)
		       (* (frm-eps m) (frm-yn m))))))
	 (yn1 (* (frm-radius m)
		 (+ (* (frm-eps m) xn1)
		    (frm-yn m)))))
    (setf (frm-xn m) xn1)
    (setf (frm-yn m) yn1)
    yn1))

(defmethod mus-frequency ((gen firmant))
  (radians->hz (frm-frequency gen)))

(defmethod (setf mus-frequency) (val (gen firmant))
  (setf (frm-frequency gen) (hz->radians val))
  (setf (frm-eps gen) (* 2.0 (sin (* 0.5 (frm-frequency gen)))))
  val)

(defmethod mus-order ((gen firmant)) 2)

(defmethod mus-run ((gen firmant) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (firmant gen arg1))

(defmethod mus-scaler ((gen firmant))
  (frm-radius gen))

(defmethod (setf mus-scaler) (val (gen firmant))
  (setf (frm-radius gen) val)
  (setf (frm-gain gen) (- 1.0 (* val val)))
  val)
