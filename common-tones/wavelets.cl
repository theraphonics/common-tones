;;; fUn wIth wAvElEts
;;; 
;;; zero-based data, translated from "Numerical Recipes in C" second edition
;;; with Daubechies coefficient data from a package by M. J. Shensa Naval Ocean Systems Center
;;; other coefficients from Wickerhauser "Adapted Wavelet Analysis"
;;; and the UBC Imager Wavelet Package by Bob Lewis

(defun wavelet (data n isign wf cc)
  (let* ((cc-size (length cc))
	 (ccr (make-array cc-size))
	 (sig -1.0))
    (loop for i from 0 below cc-size and j from (1- cc-size) by -1 do 
      (setf (aref ccr j) (* sig (aref cc i)))
      (setf sig (- sig)))
    (if (>= n 4)
	(if (>= isign 0)
	    (let ((nn n))
	      (loop while (>= nn 4) do
		(funcall wf data nn isign cc ccr)
		(setf nn (/ nn 2))))
	  (let ((nn 4))
	    (loop while (<= nn n) do
	      (funcall wf data nn isign cc ccr)
	      (incf nn nn)))))))

(defun pwt (data n isign cc cr)
  (let* ((data1 (make-array n :initial-element 0.0))
	 (n1 (1- n))
	 (ncof (length cc))
	 (nmod (* ncof n))
	 (nh (floor n 2))
	 (joff (- (floor ncof 2)))	;these two should probably be parameters
	 (ioff joff))
    (if (>= isign 0)
	(loop for ii from 0 by 1 and i from 1 by 2 to n do
	  (let ((ni (+ i nmod ioff))
		(nj (+ i nmod joff)))
	    (loop for k from 1 to ncof do
	      (let ((jf (logand n1 (+ ni k))) ;gad wotta kludge...
		    (jr (logand n1 (+ nj k))))
		(incf (aref data1 ii) (* (aref cc (1- k)) (aref data jf)))
		(incf (aref data1 (+ ii nh)) (* (aref cr (1- k)) (aref data jr)))))))
      (loop for ii from 0 by 1 and i from 1 by 2 to n do
	(let ((ai (aref data ii))
	      (ai1 (aref data (+ ii nh)))
	      (ni (+ i nmod ioff))
	      (nj (+ i nmod joff)))
	  (loop for k from 1 to ncof do
	    (let ((jf (logand n1 (+ ni k)))
		  (jr (logand n1 (+ nj k))))
	      (incf (aref data1 jf) (* ai (aref cc (1- k))))
	      (incf (aref data1 jr) (* ai1 (aref cr (1- k)))))))))
    (loop for i from 0 below n do (setf (aref data i) (aref data1 i)))))

;;; --------------------------------
;;; coefficients
(defvar daub4 (make-array 4 
		:initial-contents '(0.4829629131445341 0.8365163037378079 0.2241438680420134 -0.1294095225512604)))
(defvar daub6 (make-array 6
		:initial-contents '(0.332670552950 0.806891509311 0.459877502118 -0.135011020010 -0.085441273882 0.035226291886)))
(defvar daub8 (make-array 8 
		:initial-contents '(0.230377813309 0.714846570553 0.630880767930 -0.027983769417 -0.187034811719 0.030841381836
	                            0.032883011667 -0.010597401785)))
(defvar daub10 (make-array 10 
		 :initial-contents '(0.160102397974 0.603829269797 0.724308528438 0.138428145901 -0.242294887066 -0.032244869585
		                     0.077571493840 -0.006241490213 -0.012580751999 0.003335725285)))
(defvar daub12 (make-array 12 
		 :initial-contents '(0.111540743350 0.494623890398 0.751133908021 0.315250351709 -0.226264693965 -0.129766867567
				     0.097501605587 0.027522865530 -0.031582039317 0.000553842201 0.004777257511 -0.001077301085)))
(defvar daub14 (make-array 14 
		 :initial-contents '(0.077852054085 0.396539319482 0.729132090846 0.469782287405 -0.143906003929 -0.224036184994
			             0.071309219267 0.080612609151 -0.038029936935 -0.016574541631 0.012550998556 0.000429577973
				    -0.001801640704 0.000353713800)))
(defvar daub16 (make-array 16 
		 :initial-contents '(0.054415842243 0.312871590914 0.675630736297 0.585354683654 -0.015829105256 -0.284015542962
			             0.000472484574 0.128747426620 -0.017369301002 -0.044088253931 0.013981027917 0.008746094047
				     -0.004870352993 -0.000391740373 0.000675449406 -0.000117476784)))
(defvar daub18 (make-array 18 
		 :initial-contents '(0.038077947364 0.243834674613 0.604823123690 0.657288078051 0.133197385825 -0.293273783279
			             -0.096840783223 0.148540749338 0.030725681479 -0.067632829061 0.000250947115 0.022361662124
				     -0.004723204758 -0.004281503682 0.001847646883 0.000230385764 -0.000251963189 0.000039347320)))
(defvar daub20 (make-array 20 
		 :initial-contents '(0.026670057901 0.188176800077 0.527201188931 0.688459039453 0.281172343661 -0.249846424327
			             -0.195946274377 0.127369340336 0.093057364604 -0.071394147166 -0.029457536822 0.033212674059
				     0.003606553567 -0.010733175483 0.001395351747 0.001992405295 -0.000685856695 -0.000116466855
				     0.000093588670 -0.000013264203))) ;hooray for emacs macros!

(defconstant SQRT2 1.41421356237309504880168872420969808)

(defvar Battle-Lemarie (make-array 24
			 :initial-contents (list (* SQRT2 -0.002) (* SQRT2 -0.003) (* SQRT2  0.006) (* SQRT2  0.006) (* SQRT2 -0.013)
						 (* SQRT2 -0.012) (* SQRT2  0.030) (* SQRT2  0.023) (* SQRT2 -0.078) (* SQRT2 -0.035)
						 (* SQRT2  0.307) (* SQRT2  0.542) (* SQRT2  0.307) (* SQRT2 -0.035) (* SQRT2 -0.078)
						 (* SQRT2  0.023) (* SQRT2  0.030) (* SQRT2 -0.012) (* SQRT2 -0.013) (* SQRT2  0.006)
						 (* SQRT2  0.006) (* SQRT2 -0.003) (* SQRT2 -0.002) 0.0)))
(defvar Burt-Adelson (make-array 6 
				 :initial-contents (list (* SQRT2 (/ -1.0 20.0)) (* SQRT2 (/ 5.0 20.0)) (* SQRT2 (/ 12.0 20.0))
							 (* SQRT2 (/ 5.0 20.0)) (* SQRT2 (/ -1.0 20.0)) 0.0)))

(defvar Beylkin (make-array 18 
			    :initial-contents '(0.099305765374353 0.424215360812961 0.699825214056600 0.449718251149468
						-.110927598348234 -.264497231446384 0.026900308803690 0.155538731877093
						-.017520746266529 -.088543630622924 0.019679866044322 0.042916387274192
						-.017460408696028 -.014365807968852 0.010040411844631 .0014842347824723
						-.002736031626258 .0006404853285212)))

(defconstant SQRT15 3.87298334620741688517927)

(defvar coif2 (make-array 6 ;"coifman 6" in Wickerhauser
		:initial-contents (list (/ (* SQRT2 (- SQRT15 3)) 32.0) (/ (* SQRT2 (- 1 SQRT15)) 32.0) (/ (* SQRT2 (- 6 (* 2 SQRT15))) 32.0)
					(/ (* SQRT2 (+ (* 2 SQRT15) 6)) 32.0) (/ (* SQRT2 (+ SQRT15 13)) 32.0) (/ (* SQRT2 (- 9 SQRT15)) 32.0))))
(defvar coif4 (make-array 12
	        :initial-contents '(0.0011945726958388 	-0.01284557955324 0.024804330519353 0.050023519962135 -0.15535722285996
				    -0.071638282295294 0.57046500145033 0.75033630585287 0.28061165190244 -0.0074103835186718
				    -0.014611552521451 -0.0013587990591632)))
(defvar coif6 (make-array 18
	        :initial-contents '(-0.0016918510194918 -0.00348787621998426 0.019191160680044 0.021671094636352 -0.098507213321468
				    -0.056997424478478 0.45678712217269 0.78931940900416 0.38055713085151 -0.070438748794943 
				    -0.056514193868065 0.036409962612716 0.0087601307091635 -0.011194759273835 -0.0019213354141368
				    0.0020413809772660 0.00044583039753204 -0.00021625727664696)))

(defvar sym2 (make-array 5
	       :initial-contents (list (* SQRT2 -0.125) (* SQRT2  0.25) (* SQRT2  0.75) (* SQRT2  0.25) (* SQRT2 -0.125))))
(defvar sym3 (make-array 4 
	       :initial-contents (list (/ (* SQRT2 1.0) 8.0) (/ (* SQRT2 3.0) 8.0) (/ (* SQRT2 3.0) 8.0) (/ (* SQRT2 1.0) 8.0))))
(defvar sym4 (make-array 10 
	       :initial-contents (list (/ (* SQRT2   3.0) 128.0) (/ (* SQRT2  -6.0) 128.0) (/ (* SQRT2 -16.0) 128.0)
				       (/ (* SQRT2  38.0) 128.0) (/ (* SQRT2  90.0) 128.0) (/ (* SQRT2  38.0) 128.0)
				       (/ (* SQRT2 -16.0) 128.0) (/ (* SQRT2  -6.0) 128.0) (/ (* SQRT2   3.0) 128.0) 0.0)))
(defvar sym5 (make-array 8 
	       :initial-contents (list (/ (* SQRT2  3.0) 64.0) (/ (* SQRT2 -9.0) 64.0) (/ (* SQRT2 -7.0) 64.0) (/ (* SQRT2 45.0) 64.0)
				       (/ (* SQRT2 45.0) 64.0) (/ (* SQRT2 -7.0) 64.0) (/ (* SQRT2 -9.0) 64.0) (/ (* SQRT2  3.0) 64.0))))
(defvar sym6 (make-array 16 
	       :initial-contents (list (/ (* SQRT2   -35.0) 16384.0) (/ (* SQRT2  -105.0) 16384.0) (/ (* SQRT2  -195.0) 16384.0)
				       (/ (* SQRT2   865.0) 16384.0) (/ (* SQRT2   363.0) 16384.0) (/ (* SQRT2 -3489.0) 16384.0)
				       (/ (* SQRT2  -307.0) 16384.0) (/ (* SQRT2 11025.0) 16384.0) (/ (* SQRT2 11025.0) 16384.0)
				       (/ (* SQRT2  -307.0) 16384.0) (/ (* SQRT2 -3489.0) 16384.0) (/ (* SQRT2   363.0) 16384.0)
				       (/ (* SQRT2   865.0) 16384.0) (/ (* SQRT2  -195.0) 16384.0) (/ (* SQRT2  -105.0) 16384.0)
				       (/ (* SQRT2   -35.0) 16384.0))))

;;; for example:
;;; (setf data (make-array 256))
;;; (loop for i from 0 below 256 do (setf (aref data i) (* i .03)))
;;; (wavelet data 256 0 #'pwt c4) 
;;; followed by (wavelet data 256 -1 #'pwt daub4) 
;;; returns (more or less...) data
