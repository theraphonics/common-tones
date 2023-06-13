(in-package :common-tones)

;;; nsin


(defun nsin-scaler (n)
  (if (< n 20)
      (/ 1.0 (nth n (list 1.0 1.0 1.761 2.5 3.24 3.97 4.7 5.42 6.15 6.88
			      7.6 8.33 9.05 9.78 10.51 11.23 11.96 12.68 13.41 14.13)))
    (if (< n 50)
	(/ 1.0 (* n .743))
      (/ 1.0 (* n .733)))))

(defclass nsin (ncos)
  ())

(defmethod print-object ((d nsin) stream)
  (format stream "#<nsin: ~A, n: ~A, scaler: ~A>"
	  (prettified-freq (ncosp-freq d) (ncosp-phase d))
	  (ncosp-n d) (prettified-float (ncosp-scaler d))))

(def-optkey-fun make-nsin ((frequency *clm-default-frequency*) (n 1))
  (let ((cs (make-instance 'nsin
			   :n n
			   :freq (hz->radians frequency)
			   :phase 0.0)))
    (if (zerop n) (warn "nsin with 0 n?"))
    (setf (ncosp-scaler cs) (nsin-scaler n))
    cs))

(defmethod nsin? ((g nsin)) t)
(defmethod nsin? ((g t)) nil)

(defun nsin (cs &optional (fm 0.0))
  (let* ((a2 (* (ncosp-phase cs) 0.5))
	 (den (sin a2))
	 (val (if (= 0.0 den)
		  0.0
		(* (ncosp-scaler cs)
		   (/ (* (sin (* (ncosp-n cs) a2))
			 (sin (* (1+ (ncosp-n cs)) a2)))
		      den)))))
    (incf (ncosp-phase cs) (+ (ncosp-freq cs) fm))
    (if (> (ncosp-phase cs) two-pi) (decf (ncosp-phase cs) two-pi))
    (if (< (ncosp-phase cs) (- two-pi)) (incf (ncosp-phase cs) two-pi))
    val))

(defmethod (setf mus-length) (val (gen nsin))
  (setf (ncosp-n gen) val)
  (setf (ncosp-scaler gen) (nsin-scaler val))
  val)

(defmethod mus-run ((gen nsin) &optional (arg1 0.0) (arg2 0.0))
  (declare (ignore arg2))
  (nsin gen arg1))