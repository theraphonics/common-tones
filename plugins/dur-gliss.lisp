(in-package :common-tones/plugins)

;;; ------------------------------------------------------------------------

;;; another version of gliss-dur (Marco Trevisani)

;;;

;;; This one is very similar to Michael's (above) with the advantage that

;;; one need only provide the envelope.  The disadvantage is that the

;;; envelope x axis must match the precise duration in the input file (all

;;; or a portion).


(defun dur-gliss (x)
  (if (< (list-length x) 4)
      (error "Inside ~S~%You must use a list equal or larger than 4 elements~%" x)
    (if  (oddp (list-length x))
        (error "Inside ~S~%Careful oddp=T. An envelop must be evenp=T~%" x)
      (reduce #'+ (loop for a from 0 to (- (list-length x) 4) by 2
		   collect (* (/ (- (nth (+ 2 a) x) (nth a x)) 2.0)
			      (+ (/ 1.0 (nth (+ 3 a) x))
				 (/ 1.0 (nth (+ 1 a) x)))))))))