(in-package :common-tones/generators)


(defmacro ring-modulate (in1 in2) `(* ,in1 ,in2))

;;; Amplitude modulation (one often seen definition is in1 * (k + in2))

(defmacro amplitude-modulate (am-carrier input1 input2) `(* ,input1 (+ ,am-carrier ,input2)))


(defun polynomial (coeffs x)
  (let* ((top (- (array-dimension coeffs 0) 1))
	 (sum (aref coeffs top)))
    (loop for i from (- top 1) downto 0 do
      (setf sum (+ (* x sum) (aref coeffs i))))
    sum))


(defun array-interp (fn x &optional size)
  (let ((len (or size (length fn))))
    (if (< x 0.0) (incf x len))
    (multiple-value-bind
	(int-part frac-part)
	(truncate x)
      (if (>= int-part len)
	  (setf int-part (mod int-part len)))
      (if (zerop frac-part)
	  (aref fn int-part)
	(+ (aref fn int-part)
	   (* frac-part (- (aref fn (if (< (1+ int-part) len) (1+ int-part) 0))
			   (aref fn int-part))))))))

(defun mus-interpolate (type fn x &optional size yn1)
  (declare (ignore type fn x size yn1))
  (warn "mus-interpolate only works in 'run'"))

(defmacro contrast-enhancement (in-samp &optional (fm-index 1.0))
  `(let ((var ,in-samp))		;don't evaluate in-samp twice (might be expression with side-effects)
     (sin (+ (* var 1.5707964)
	     (* ,fm-index (sin (* var 6.2831855)))))))


(defun fft (xdata ydata n &optional (isign 1))
  (clm-fft xdata ydata n isign))

#-openmcl (defun bes-i0 (x)
  ;; from "Numerical Recipes in C"
  (if (< (abs x) 3.75)
      (let* ((y (expt (/ x 3.75) 2)))
	(+ 1.0
	   (* y (+ 3.5156229
		   (* y (+ 3.0899424
			   (* y (+ 1.2067492
				   (* y (+ 0.2659732
					   (* y (+ 0.360768e-1
						   (* y 0.45813e-2)))))))))))))
    (let* ((ax (abs x))
	   (y (/ 3.75 ax)))
      (* (/ (exp ax) (sqrt ax))
	 (+ 0.39894228
	    (* y (+ 0.1328592e-1
		    (* y (+ 0.225319e-2
			    (* y (+ -0.157565e-2
				    (* y (+ 0.916281e-2
					    (* y (+ -0.2057706e-1
						    (* y (+ 0.2635537e-1
							    (* y (+ -0.1647633e-1
								    (* y 0.392377e-2))))))))))))))))))))
#+openmcl (defun bes-i0 (x) x)

(defconstant rectangular-window 0)
(defconstant hann-window 1)
(defconstant hanning-window 1)
(defconstant welch-window 2)
(defconstant parzen-window 3)
(defconstant bartlett-window 4)
(defconstant hamming-window 5)
(defconstant blackman2-window 6)
(defconstant blackman3-window 7)
(defconstant blackman4-window 8)
(defconstant exponential-window 9)
(defconstant riemann-window 10)
(defconstant kaiser-window 11)
(defconstant cauchy-window 12)
(defconstant poisson-window 13)
(defconstant gaussian-window 14)
(defconstant tukey-window 15)
(defconstant dolph-chebyshev-window 16)
(defconstant hann-poisson-window 17)
(defconstant connes-window 18)
(defconstant samaraki-window 19)
(defconstant ultraspherical-window 20)
(defconstant bartlett-hann-window 21)
(defconstant bohman-window 22)
(defconstant flat-top-window 23)
(defconstant blackman5-window 24)
(defconstant blackman6-window 25)
(defconstant blackman7-window 26)
(defconstant blackman8-window 27)
(defconstant blackman9-window 28)
(defconstant blackman10-window 29)
(defconstant rv2-window 30)
(defconstant rv3-window 31)
(defconstant rv4-window 32)

(defun ultraspherical (n x lambda)
  (if (= n 0)
      1.0
    (let ((fn1 (if (= lambda 0.0)
		   (* 2.0 x)
		 (* 2.0 lambda x))))
      (if (= n 1)
	  fn1
	(let ((fn 1.0)
	      (fn2 1.0))
	  (do ((k 2 (1+ k)))
	      ((> k n) fn)
	    (setf fn (/ (- (* 2.0 x (+ k lambda -1.0) fn1)
			   (* (+ k (* 2.0 lambda) -2.0) fn2))
			k))
	    (setf fn2 fn1)
	    (setf fn1 fn)))))))

(defun dolph-chebyshev (type window gamma mu)
  (let* ((N (length window))
	 (alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (freq (/ pi N))
	 (rl (make-double-float-array N))
	 (im (make-double-float-array N)))
    (if (= type ultraspherical-window)
	(if (= mu 0.0)
	    (setf type dolph-chebyshev-window)
	  (if (= mu 1.0)
	      (setf type samaraki-window))))
    (do ((i 0 (1+ i))
	 (phase 0.0 (+ phase freq)))
	((= i N))
      (if (= type dolph-chebyshev-window)
	  (setf (aref rl i) (double (realpart (cos (* N (acos (* alpha (cos phase))))))))
	(if (= type samaraki-window)
	    (setf (aref rl i) (double (realpart (/ (sin (* (+ N 1.0) (acos (* alpha (cos phase)))))
						   (sin (acos (* alpha (cos phase))))))))
	  (setf (aref rl i) (double (realpart (ultraspherical N (* alpha (cos phase)) mu)))))))
    (fft rl im -1)
    (let ((pk 0.0))
      (do ((i 0 (1+ i)))
	  ((= i N))
	(if (> (abs (aref rl i)) pk)
	    (setf pk (abs (aref rl i)))))
      (if (and (> pk 0.0)
	       (not (= pk 1.0)))
	  (let ((scl (/ 1.0 pk)))
	    (do ((i 0 (1+ i)))
		((= i N))
	      (setf (aref rl i) (double (* scl (aref rl i))))))))
    (do ((i 0 (1+ i))
	 (j (/ N 2)))
	((= i N))
      (setf (aref window i) (double (aref rl j)))
      (setf j (+ j 1))
      (if (= j N) (setf j 0)))
    window))

(def-optkey-fun make-fft-window ((type rectangular-window) (size 32) (beta 0.0) (mu 0.0))
  ;; mostly taken from
  ;;   Fredric J. Harris, "On the Use of Windows for Harmonic Analysis with the
  ;;   Discrete Fourier Transform," Proceedings of the IEEE, Vol. 66, No. 1,
  ;;   January 1978.
  ;;   Albert H. Nuttall, "Some Windows with Very Good Sidelobe Behaviour",
  ;;   IEEE Transactions of Acoustics, Speech, and Signal Processing, Vol. ASSP-29,
  ;;   No. 1, February 1981, pp 84-91

  (let* ((window (make-double-array size))
	 (midn (floor size 2))
	 (midp1 (/ (1+ size) 2))
	 (freq (/ two-pi size))
	 (rate (/ 1.0 midn))
	 (sr (/ two-pi size))
	 (angle 0.0)
	 (expn (+ 1.0 (/ (log 2) midn)))
	 (expsum 1.0)
	 (I0beta (if (= type kaiser-window) (bes-i0 beta)))
	 (val 0.0))

    (macrolet ((lw (&body code)
		  `(loop for i from 0 to midn and j = (1- size) then (1- j) do
		    (progn ,.code)
		    (setf (aref window i) (double val))
		    (setf (aref window j) (double val))))

	       (lcw (&body code)
		 `(loop for i from 0 to midn and j = (1- size) then (1- j) do
		    (let ((cx (cos angle)))
		      (progn ,.code)
		      (setf (aref window i) (double val))
		      (setf (aref window j) (double val))
		      (incf angle freq)))))

      (cond ((= type rectangular-window)
	     (lw (setf val 1.0)))

	    ((= type bartlett-window)
	     (lw (setf val angle)
		 (incf angle rate)))

	    ((= type parzen-window)
	     (lw (setf val (- 1.0 (abs (/ (- i midn) midp1))))))

	    ((= type welch-window)
	     (lw (setf val (- 1.0 (sqr (/ (- i midn) midp1))))))

	    ((= type exponential-window)
	     (lw (setf val (- expsum 1.0))
		 (setf expsum (* expsum expn))))

	    ((= type kaiser-window)
	     (lw (setf val (/ (bes-i0 (* beta (sqrt (- 1.0 (sqr (/ (- midn i) midn)))))) I0beta))))

	    ((= type gaussian-window)
	     (lw (setf val (exp (* -.5 (sqr (* beta (/ (- midn i) midn))))))))

	    ((= type poisson-window)
	     (lw (setf val (exp (* (- beta) (/ (- midn i) midn))))))

	    ((= type riemann-window)
	     (lw (if (= midn i)
		     (setf val 1.0)
		   (setf val (/ (sin (* sr (- midn i))) (* sr (- midn i)))))))

	    ((= type cauchy-window)
	     (lw (setf val (/ 1.0 (+ 1.0 (sqr (/ (* beta (- midn i)) midn)))))))

	    ((= type tukey-window)
	     (lw (let ((pos (* midn (- 1.0 beta))))
		   (if (>= i pos)
		       (setf val 1.0)
		     (setf val (* .5 (- 1.0 (cos (/ (* pi i) pos)))))))))

	    ((= type dolph-chebyshev-window)
	     (dolph-chebyshev dolph-chebyshev-window window beta 0.0))

	    ((= type samaraki-window)
	     (dolph-chebyshev samaraki-window window beta 1.0))

	    ((= type ultraspherical-window)
	     (dolph-chebyshev ultraspherical-window window beta mu))

	    ((= type hann-poisson-window)
	     (lcw (setf val (* (- 0.5 (* 0.5 cx)) (exp (* (- beta) (/ (- midn i) midn)))))))

	    ((= type connes-window)
	     (lw (setf val (sqr (- 1.0 (sqr (/ (- i midn) midp1)))))))

	    ((= type bartlett-hann-window)
	     (lw (setf val (+ 0.62
			      (* -0.48 (abs (- (/ i (1- size)) 0.5)))
			      (* 0.38 (cos (* 2 pi (- (/ i (1- size)) 0.5))))))))

	    ((= type bohman-window)
	     (lw (let ((r (/ (- midn i) midn)))
		   (setf val (+ (* (- 1.0 r) (cos (* pi r)))
				(* (/ 1.0 pi) (sin (* pi r))))))))

	    ((= type flat-top-window)
	     (lcw (setf val (+ 0.2156
			       (* -0.4160 cx)
			       (* 0.2781 (cos (* 2 angle)))
			       (* -0.0836 (cos (* 3 angle)))
			       (* 0.0069 (cos (* 4 angle)))))))

	    ((= type hann-window)
	     (lcw (setf val (- 0.5 (* 0.5 cx)))))

	    ((= type rv2-window)
	     (lcw (setf val (+ .375
			       (* -0.5 cx)
			       (* .125 (cos (* 2 angle)))))))

	    ((= type rv3-window)
	     (lcw (setf val (+ (/ 10.0 32.0)
			       (* (/ -15.0 32.0) cx)
			       (* (/ 6.0 32.0) (cos (* 2 angle)))
			       (* (/ -1.0 32.0) (cos (* 3 angle)))))))

	    ((= type rv4-window)
	     (lcw (setf val (+ (/ 35.0 128.0)
			       (* (/ -56.0 128.0) cx)
			       (* (/ 28.0 128.0) (cos (* 2 angle)))
			       (* (/ -8.0 128.0) (cos (* 3 angle)))
			       (* (/ 1.0 128.0) (cos (* 4 angle)))))))

	    ((= type hamming-window)
	     (lcw (setf val (- 0.54 (* 0.46 cx)))))

	    ((= type blackman2-window)
	     (lcw (setf val (* (+ .34401 (* cx (+ -.49755 (* cx .15844))))))))

	    ((= type blackman3-window)
	     (lcw (setf val (+ .21747 (* cx (+ -.45325 (* cx (+ .28256 (* cx -.04672)))))))))

	    ((= type blackman4-window)
	     (lcw (setf val (+ .08403 (* cx (+ -.29145 (* cx (+ .37569 (* cx (+ -.20762 (* cx .04119)))))))))))

	    ((= type blackman5-window)
	     (lcw (setf val (+ .293557
			       (* -.451935 cx)
			       (* .201416 (cos (* 2 angle)))
			       (* -.047926 (cos (* 3 angle)))
			       (* .00502619 (cos (* 4 angle)))
			       (* -.000137555 (cos (* 5 angle)))))))

	    ((= type blackman6-window)
	     (lcw (setf val (+ .271220
			       (* -.433444 cx)
			       (* .218004 (cos (* 2 angle)))
			       (* -.065785 (cos (* 3 angle)))
			       (* .01076186 (cos (* 4 angle)))
			       (* -.000770012 (cos (* 5 angle)))
			       (* .0000136808 (cos (* 6 angle)))))))

	    ((= type blackman7-window)
	     (lcw (setf val (+ .253317
			       (* -.416327 cx)
			       (* .228839 (cos (* 2 angle)))
			       (* -.081575 (cos (* 3 angle)))
			       (* .01773592 (cos (* 4 angle)))
			       (* -.002096702 (cos (* 5 angle)))
			       (* .0001067741 (cos (* 6 angle)))
			       (* -.0000012807(cos (* 7 angle)))))))

	    ((= type blackman8-window)
	     (lcw (setf val (+ .238433
			       (* -.400554 cx)
			       (* .235824 (cos (* 2 angle)))
			       (* -.095279 (cos (* 3 angle)))
			       (* .02537395 (cos (* 4 angle)))
			       (* -.00415243  (cos (* 5 angle)))
			       (* .0003685604 (cos (* 6 angle)))
			       (* -.0000138435 (cos (* 7 angle)))
			       (* .000000116180(cos (* 8 angle)))))))

	    ((= type blackman9-window)
	     (lcw (setf val (+ .225734
			       (* -.386012 cx)
			       (* .240129 (cos (* 2 angle)))
			       (* -.107054 (cos (* 3 angle)))
			       (* .03325916 (cos (* 4 angle)))
			       (* -.00687337  (cos (* 5 angle)))
			       (* .0008751673 (cos (* 6 angle)))
			       (* -.0000600859 (cos (* 7 angle)))
			       (* .000001710716 (cos (* 8 angle)))
			       (* -.00000001027272(cos (* 9 angle)))))))

	    ((= type blackman10-window)
	     (lcw (setf val (+ .215153
			       (* -.373135 cx)
			       (* .242424 (cos (* 2 angle)))
			       (* -.1166907 (cos (* 3 angle)))
			       (* .04077422 (cos (* 4 angle)))
			       (* -.01000904 (cos (* 5 angle)))
			       (* .0016398069 (cos (* 6 angle)))
			       (* -.0001651660 (cos (* 7 angle)))
			       (* .000008884663 (cos (* 8 angle)))
			       (* -.000000193817 (cos (* 9 angle)))
			       (* .000000000848248(cos (* 10 angle)))))))
	    )

      window)))

(defun spectrum (rdat idat window &optional (type 0))
  (let* ((len (length rdat))
	 (len2 (floor len 2)))
    (multiply-arrays rdat window)
    (clear-array idat)
    (fft rdat idat len 1)
    (let ((maxa 0.0)
	  (20log10 (/ 20 (log 10)))
	  (lowest 1.0e-6))
      (loop for i from 0 below len do
	(setf (aref rdat i) (double (sqrt (+ (sqr (max lowest (aref rdat i))) (sqr (max lowest (aref idat i)))))))
	(setf maxa (max maxa (abs (aref rdat i)))))
      (if (> maxa 0.0)
	  (if (= type 0)
	      (loop for i from 0 below len2 do (setf (aref rdat i) (double (* 20log10 (log (max (/ (aref rdat i) maxa) lowest))))))
	    (if (= type 1)
		(loop for i from 0 below len2 do (setf (aref rdat i) (double (/ (aref rdat i) maxa)))))))
      rdat)))