(in-package :common-tones)

(defclass wave-train ()
  ((wave :initform nil :initarg :wave :accessor wt-wave)
   (freq :initform nil :initarg :freq :accessor wt-freq)
   (b :initform nil :initarg :b :accessor wt-b)
   (phase :initform nil :initarg :phase :accessor wt-phase)
   (type :initform mus-interp-linear :initarg :type :accessor wt-type)))

(defmethod print-object ((d wave-train) stream)
  (format stream "#<wave-train: freq: ~A, :phase ~A, wave: ~A, b: ~A>"
		       (prettified-float (wt-freq d))
		       (prettified-float (wt-phase d))
		       (prettified-array (wt-wave d))
		       (wt-b d)))

(def-optkey-fun make-wave-train ((frequency *clm-default-frequency*) (initial-phase 0.0) wave (size *clm-table-size*) (type mus-interp-linear))
  (let* ((wavetrain (or wave (make-double-array size)))
	 (wave-size (length wavetrain)))
    (make-instance 'wave-train
		   :wave wavetrain
		   :b nil
		   :phase (if (not (zerop initial-phase))
			      (* wave-size (/ initial-phase two-pi))
			    0.0)
		   :freq frequency
		   :type type)))

(defmethod wave-train? ((g wave-train)) t)
(defmethod wave-train? ((g t)) nil)

(defun wave-train (w &optional (fm 0.0))
  (declare (ignore w fm))
  (warn "wave-train only works inside the run macro"))

(defmethod mus-frequency ((gen wave-train)) (wt-freq gen))
(defmethod (setf mus-frequency) (val (gen wave-train)) (setf (wt-freq gen) val) val)
(defmethod mus-phase ((gen wave-train)) (mod (/ (* two-pi (wt-phase gen)) (length (wt-wave gen))) two-pi))
(defmethod (setf mus-phase) (val (gen wave-train)) (setf (wt-phase gen) (/ (* val (length (wt-wave gen))) two-pi)) val)
(defmethod mus-data ((gen wave-train)) (wt-wave gen))
(defmethod mus-length ((gen wave-train)) (length (wt-wave gen)))
(defmethod mus-run ((gen wave-train) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (wave-train gen arg1))
(defmethod mus-interp-type ((gen wave-train)) (wt-type gen))