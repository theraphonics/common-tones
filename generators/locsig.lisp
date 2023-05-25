(in-package :common-tones/generators)

;;; locsig

;;; "placement" in speakers (i.e. take degree and distance and pretend to conjure up some amplitudes

;;; before sending the signal out the speakers.  This (despite its name) gives you a very diffuse

;;; apparent source, and under normal conditions, that is exactly the right thing.


;;; backwards compatibility

(defconstant mus-linear 0)
(defconstant mus-sinusoidal 1)
(defvar *clm-locsig-type* mus-interp-linear)

(defclass locsig ()
  ((outn :initform nil :initarg :outn :accessor locs-outn)
   (revn :initform nil :initarg :revn :accessor locs-revn)
   (outf :initform nil :initarg :outf :accessor locs-outf)
   (reverb :initform 0.0 :initarg :reverb :accessor locs-reverb)
   (type :initform mus-interp-linear :initarg :type :accessor locs-type)
   (degree :initform nil :initarg :degree :accessor locs-degree)
   (distance :initform nil :initarg :distance :accessor locs-distance)
   (chans :initform nil :initarg :chans :accessor locs-chans)))

(defmethod print-object ((d locsig) stream)
  (format stream "#<locsig: outn: ~A, revn: ~A>"
	  (prettified-array (locs-outn d))
	  (prettified-array (locs-revn d))))

(defmethod locsig? ((g locsig)) t)
(defmethod locsig? ((g t)) nil)

(defun locsig-ref (gen chan) (aref (locs-outn gen) chan))
(defun locsig-reverb-ref (gen chan) (if (locs-revn gen) (aref (locs-revn gen) chan) 0.0))
(defun locsig-set! (gen chan val) (setf (aref (locs-outn gen) chan) (double val)))
(defun locsig-reverb-set! (gen chan val) (if (locs-revn gen) (setf (aref (locs-revn gen) chan) (double val))))
(defsetf locsig-ref locsig-set!)
(defsetf locsig-reverb-ref locsig-reverb-set!)
(defmethod mus-channels ((gen locsig)) (length (locs-outn gen)))
(defmethod mus-length ((gen locsig)) (length (locs-outn gen)))
(defmethod mus-data ((gen locsig)) (locs-outn gen))
(defmethod mus-xcoeffs ((gen locsig)) (locs-revn gen))
(defmethod mus-xcoeff ((gen locsig) index) (aref (locs-revn gen) index))

(defun locsig-type () *clm-locsig-type*)
(defun set-locsig-type (val) (setf *clm-locsig-type* val))
(defsetf logsig-type set-locsig-type)

(defun fill-locsig (arr chans degree dist scale type)
  (declare (ignore dist))
  (if (= chans 1)
      (setf (aref arr 0) (double scale))
    (let* ((deg (if (= chans 2)
		    (max 0.0 (min 90.0 degree))
		  (let ((val (mod degree 360.0)))
		    (if (< val 0.0) ; I don't think this can happen
			(+ val 360.0)
		      val))))
	   (degs-per-chan (if (= chans 2)
			      90.0
			    (/ 360.0 chans)))
	   (pos (/ deg degs-per-chan))
	   (left (floor pos))
	   (right (mod (+ left 1) chans))
	   (frac (- pos left)))
      (if (= type mus-interp-linear)
	  (progn
	    (setf (aref arr left) (double (* scale (- 1.0 frac))))
	    (setf (aref arr right) (double (* scale frac))))
	(let* ((ldeg (* (/ pi 2) (- 0.5 frac)))
	       (norm (/ (sqrt 2.0) 2.0))
	       (c (cos ldeg))
	       (s (sin ldeg)))
	  (setf (aref arr left) (double (* scale norm (+ c s))))
	  (setf (aref arr right) (double (* scale norm (- c s)))))))))

(defun move-locsig (gen degree distance)
  (let* ((dist (/ 1.0 (max distance 1.0)))
	 (rscale (/ (locs-reverb gen) (sqrt (max distance 1.0)))))
    (mus-reset gen) ; clear out old state, if any
    (if *reverb* (fill-locsig (locs-revn gen) 1 degree dist rscale (locs-type gen)))
    (fill-locsig (locs-outn gen) (mus-channels gen) degree dist dist (locs-type gen))))

(def-optkey-fun make-locsig ((degree 0.0) (distance 1.0) (reverb 0.0) (channels nil) (type *clm-locsig-type*))
  (let* ((dist (/ 1.0 (max distance 1.0)))
	 (sdist (/ 1.0 (sqrt (max distance 1.0))))
	 (out-chans (or channels (and *output* (mus-channels *output*)) *clm-channels* 1))
	 (outn-arr (make-double-array out-chans))
	 (rev-chans (if *reverb* (mus-channels *reverb*) 0))
	 (revn-arr (if *reverb* (make-double-array rev-chans)))
	 (rscale (* sdist reverb)))
    (if *reverb* (fill-locsig revn-arr rev-chans degree dist rscale type))
    (fill-locsig outn-arr out-chans degree dist dist type)
    (make-instance 'locsig
		   :outn outn-arr
		   :revn revn-arr
		   :outf (make-double-array out-chans)
		   :degree degree
		   :distance distance
		   :chans out-chans
		   :reverb reverb
		   :type type)))

(defun locsig (l i in-sig)
  (declare (ignore l i))
  (warn "Lisp interpreted locsig is a no-op")
  in-sig)