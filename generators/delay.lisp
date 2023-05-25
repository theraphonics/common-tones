(in-package :common-tones/generators)

(defclass delay ()
  ((size :initform nil :initarg :size :accessor dly-size)
   (line :initform nil :initarg :line :accessor dly-line)
   (loc :initform 0 :initarg :loc :accessor dly-loc)
   (zloc :initform 0 :initarg :zloc :accessor dly-zloc)
   (zsize :initform 0 :initarg :zsize :accessor dly-zsize)
   (dloc :initform 0.0 :initarg :dloc :accessor dly-dloc)
   (zdly :initform nil :initarg :zdly :accessor dly-zdly)
   (xscl :initform 0.0 :initarg :xscl :accessor dly-xscl)
   (yscl :initform 0.0 :initarg :yscl :accessor dly-yscl)
   (type :initform mus-interp-none :initarg :type :accessor dly-type)))

(defmethod print-object ((d delay) stream)
  (format stream "#<(delay: size: ~A~A, loc: ~A~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (prettified-array (dly-line d))))

(def-optkey-fun make-delay ((size 1) initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'delay
		   :loc 0
		   :size (floor size)
		   :zsize lsize
		   :zdly max-size
		   :zloc (and max-size (- max-size size))
		   :line (if initial-contents
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize)))
		   :type (or type
			     (if max-size
				 mus-interp-linear
			       mus-interp-none)))))

(defmethod delay? ((g delay)) t)
(defmethod delay? ((g t)) nil)

(defun tap (d &optional (offset 0.0))
  (if (dly-zdly d)
      (if (= offset 0.0)
	  (aref (dly-line d) (dly-zloc d))
	(array-interp (dly-line d) (- (dly-zloc d) offset) (dly-zsize d)))
    (if (= offset 0.0)
	(aref (dly-line d) (dly-loc d))
      (aref (dly-line d) (floor (mod (- (dly-loc d) offset) (dly-size d)))))))

(defun delay-tick (d input)
  (setf (aref (dly-line d) (dly-loc d)) (double input))
  (incf (dly-loc d))
  (if (dly-zdly d)
      (progn
	(if (<= (dly-zsize d) (dly-loc d)) (setf (dly-loc d) 0))
	(incf (dly-zloc d))
	(if (<= (dly-zsize d) (dly-zloc d)) (setf (dly-zloc d) 0)))
    (if (<= (dly-size d) (dly-loc d)) (setf (dly-loc d) 0)))
  input)

(defun delay (d input &optional (pm 0.0))
  (prog1
      (tap d pm)
    (delay-tick d input)))

(defmethod mus-length ((gen delay)) (dly-size gen))
(defmethod mus-order ((gen delay)) (dly-size gen))
(defmethod mus-data ((gen delay)) (dly-line gen))
(defmethod mus-run ((gen delay) &optional (arg1 0.0) (arg2 0.0)) (delay gen arg1 arg2))
(defmethod mus-interp-type ((gen delay)) (dly-type gen))