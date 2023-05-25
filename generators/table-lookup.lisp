(in-package :common-tones/generators)

(defclass table-lookup ()
  ((freq :initform nil :initarg :freq :accessor tbl-freq)
   (phase :initform nil :initarg :phase :accessor tbl-phase)
   (wave :initform nil :initarg :wave :accessor tbl-wave)
   (type :initform mus-interp-linear :initarg :type :accessor tbl-type)))

(defmethod print-object ((gen table-lookup) stream)
  (format stream "#<(table-lookup: ~A, size: ~A, table: ~A>"
		       (prettified-freq (tbl-freq gen) (tbl-phase gen) (length (tbl-wave gen)))
		       (length (tbl-wave gen))
		       (prettified-array (tbl-wave gen))))

(def-optkey-fun make-table-lookup ((frequency *clm-default-frequency*)
			           (initial-phase 0.0)
			           wave
				   (size *clm-table-size*)
				   (type mus-interp-linear))
  (let* ((wavetable (or wave (make-double-array size)))
	 (tblsiz (length wavetable)))
    (make-instance 'table-lookup
		   :freq (* frequency (/ tblsiz *srate*))
		   :phase (/ (* initial-phase tblsiz) two-pi)
		   :wave wavetable
		   :type type)))

(defmethod table-lookup? ((g table-lookup)) t)
(defmethod table-lookup? ((g t)) nil)

(defun table-lookup (tl &optional (fm-input 0.0))
  (let ((val (array-interp (tbl-wave tl) (tbl-phase tl)))
	(len (length (tbl-wave tl))))
    (incf (tbl-phase tl) (+ (tbl-freq tl) (* fm-input (/ len two-pi))))
    (if (or (> (tbl-phase tl) len) (minusp (tbl-phase tl)))
	(setf (tbl-phase tl) (mod (tbl-phase tl) len)))
    val))

(defmethod mus-frequency ((gen table-lookup)) (/ (* (tbl-freq gen) *srate*) (length (tbl-wave gen))))
(defmethod (setf mus-frequency) (val (gen table-lookup)) (setf (tbl-freq gen) (/ (* val (length (tbl-wave gen))) *srate*)) val)
(defmethod mus-increment ((gen table-lookup)) (tbl-freq gen))
(defmethod (setf mus-increment) (val (gen table-lookup)) (setf (tbl-freq gen) val) val)
(defmethod mus-phase ((gen table-lookup)) (mod (/ (* two-pi (tbl-phase gen)) (length (tbl-wave gen))) two-pi))
(defmethod (setf mus-phase) (val (gen table-lookup)) (setf (tbl-phase gen) (/ (* val (length (tbl-wave gen))) two-pi)) val)
(defmethod mus-data ((gen table-lookup)) (tbl-wave gen))
(defmethod (setf mus-data) (val (gen table-lookup)) (setf (tbl-wave gen) val) val)
(defmethod mus-length ((gen table-lookup)) (length (tbl-wave gen)))
(defmethod mus-run ((gen table-lookup) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (table-lookup gen arg1))
(defmethod mus-interp-type ((gen table-lookup)) (tbl-type gen))



;;; Additive Synthesis -- data comes in "synth table", a list of partial--amp pairs


(defun load-one-sine-wave (partial partial-amp table &optional (partial-phase 0.0))
  (when (/= 0.0 partial-amp)
    (let* ((len (length table))
	   (freq (* partial (/ two-pi len))))
      (loop for i from 0 below len and angle from partial-phase by freq do
	(incf (aref table i) (double (* partial-amp (sin angle))))))))

(defun partials->wave (synth-data &optional utable (norm t))
  (when (not (listp synth-data))
    (setf synth-data (clm-cerror "use '(1 1)" (list 1 1) #'listp "weird argument to partials->wave: ~A" synth-data)))
  (let* ((table (or utable (make-double-array *clm-table-size*))))
    (loop for partial in synth-data by #'cddr and amp in (cdr synth-data) by #'cddr do
      (load-one-sine-wave partial amp table))
    (if norm (normalize-array table))
    table))

(defun phase-partials->wave (synth-data &optional utable (norm t))
  (when (not (listp synth-data))
    (setf synth-data (clm-cerror "use '(1 1)" (list 1 1) #'listp "weird argument to phase-partials->wave: ~A" synth-data)))
  (let* ((table (or utable (make-double-array *clm-table-size*))))
    (loop for partial in synth-data by #'cdddr and amp in (cdr synth-data) by #'cdddr and angle in (cddr synth-data) by #'cdddr do
      (load-one-sine-wave partial amp table angle))
    (if norm (normalize-array table))
    table))