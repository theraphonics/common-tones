(in-package :common-tones/generators)
/*!< -------- READIN --------


(defclass readin ()
  ((loc :initform nil :initarg :loc :accessor rdin-loc)
   (chn :initform nil :initarg :chn :accessor rdin-chn)
   (fil :initform nil :initarg :fil :accessor rdin-fil)
   (dir :initform nil :initarg :dir :accessor rdin-dir)
   (size :initform nil :initarg :size :accessor rdin-size)))

(defmethod print-object ((d readin) stream)
  (format stream "#<readin: loc: ~A, chn: ~A, dir: ~A, fil: ~A>"
	  (rdin-loc d) (rdin-chn d) (rdin-dir d)
	  (rdin-fil d)))

(def-optkey-fun make-readin (file channel start (direction 1) (size *clm-file-buffer-size*))
  (make-instance 'readin
		 :fil (if (mus-input? file) file (make-file->sample file))
		 :dir direction
		 :loc (or (and start (floor start))
			  (and (mus-input? file)
			       (mus-location file))
			  0)
		 :chn (or channel
			  (and (mus-input? file)
			       (mus-channel file))
			  0)
		 :size size))

(defun readin (rd)
  (declare (ignore rd))
  (warn "Lisp interpreted readin is a no-op"))

(defmethod readin? ((g readin)) t)
(defmethod readin? ((g t)) nil)

(defmethod mus-location ((gen readin)) (rdin-loc gen))
(defmethod (setf mus-location) (val (gen readin)) (setf (rdin-loc gen) val))
(defmethod mus-increment ((gen readin)) (rdin-dir gen))
(defmethod (setf mus-increment) (val (gen readin)) (setf (rdin-dir gen) val))
(defmethod mus-channel ((gen readin)) (rdin-chn gen))
(defmethod mus-input? ((gen readin)) t)
(defmethod mus-run ((gen readin) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (readin gen))
(defmethod mus-file-name ((gen readin)) (mus-file-name (rdin-fil gen)))
(defmethod mus-length ((gen readin)) (mus-length (rdin-fil gen)))

(defun open-input-via-readin (name)
  (let ((filename (if (stringp name)
		      name
		    (if (pathnamep name)
			(filename->string name)
		      (if (mus-input? name)
			  (mus-file-name name))))))
    (if filename
	(make-readin filename)
      name))) ; might be a function, I suppose

(defmethod mus-channels ((gen string)) (sound-chans gen))
(defmethod mus-length ((gen string)) (sound-framples gen))



(defun frample->frample (m f res)
  (let* ((mx-chans (floor (sqrt (array-total-size m))))
	 (in-chans (min (array-total-size f) mx-chans))
	 (out-chans (min in-chans (array-total-size res))))
    (loop for i from 0 below out-chans do
      (setf (aref res i) (double 0.0))
      (loop for j from 0 below in-chans do
	(incf (aref res i) (double (* (aref f j) (aref m (+ (* j mx-chans) i)))))))
    res))