(in-package :clm)

(defvar *statistics* nil)
(defvar *interrupted* 0)
(defvar *offset* 0)
(defvar *srate* *clm-srate*)
(defvar *channels* *clm-channels*)
(defvar *data-format* *clm-data-format*)
(defvar *header-type* *clm-header-type*)
(defvar *safety* *clm-safety*)
(defvar *clm-debug* nil)
(defvar *debug* *clm-debug*)
(defvar *notehook* *clm-notehook*)
(defvar *clipped* *clm-clipped*)
(defvar *verbose* *clm-verbose*)
(defvar *clm-ins* nil)


(def-optkey-fun clm-open-input (file (start 0) (channel 0))
  (let ((fname (search-full-merge-pathnames file *clm-file-name* "test.snd")))
    (if (not fname)
	(warn "can't find ~S" file)
      (make-file->sample :file (filename->string fname)
			 :start start
			 :channel channel))))

(def-optkey-fun open-input* (name (start 0) (channel 0) restartable)
  (let ((ios nil))
    (tagbody
      SEARCH-AGAIN
      (let ((fname (search-full-merge-pathnames name *clm-file-name*)))
	(if fname
	    (setf ios (clm-open-input :file fname :start start :channel channel))
	  (if restartable
	      (restart-case 
		  (break "can't find ~S" name)
		(nil (file-name)
		    :report "try again with a new file name."
		    :interactive (lambda () 
				   (progn
				     (princ "open-input* file: ")
				     (list (read-from-string (read-line)))))
		  (setf name file-name)
		  (go SEARCH-AGAIN)))
	    (warn "can't find ~S" name)))))
    ios))

(defun close-input (i-stream)
  (declare (ignore i-stream))
  nil)

;;; someday: generic outa for CL, not to mention defgenerator and env-any!  

(defvar out-already-warned nil)

(defun out-any (loc data &optional (channel 0) o-stream) 
  (declare (ignore loc data channel o-stream)) 
  (if (not out-already-warned)
      (progn
	(warn "Lisp interpreted out-any is a no-op")
	(setf out-already-warned t))))

;;; these need to be macros for the run macro's benefit
(defmacro outa (loc data &optional (o-stream '*output*)) `(out-any ,loc ,data 0 ,o-stream))
(defmacro outb (loc data &optional (o-stream '*output*)) `(out-any ,loc ,data 1 ,o-stream))
(defmacro outc (loc data &optional (o-stream '*output*)) `(out-any ,loc ,data 2 ,o-stream))
(defmacro outd (loc data &optional (o-stream '*output*)) `(out-any ,loc ,data 3 ,o-stream))

(defvar in-already-warned nil)

(defun in-any (loc channel i-stream)
  (declare (ignore loc channel i-stream)) 
  (if (not in-already-warned)
      (progn
	(warn "Lisp interpreted in-any is a no-op")
	(setf in-already-warned t))))

(defmacro ina (loc i-stream) `(in-any ,loc 0 ,i-stream))
(defmacro inb (loc i-stream) `(in-any ,loc 1 ,i-stream))


(defun whos-to-blame () 
  (let ((site #-openmcl (or (long-site-name) (short-site-name)) )
        (user #-openmcl (first (last (pathname-directory (user-homedir-pathname))))) ;can be (:ABSOLUTE "Net" ...)
        (machine (machine-type))
        (lisp (lisp-implementation-type)))
    (if (or user site machine lisp)
	(format nil "~A~A~A~A~A~A~A~A~A"
		(if user "by " "")
		(if user user "")
		(if site " at " "")
		(if site site "")
		(if machine " (" "")
		(if machine machine "")
		(if machine ")" "")
		(if lisp " using " "")
		(if lisp lisp "")))))

(defun month-name (month) (nth (- month 1) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
(defun day-name (day) (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun timestring () 
  (multiple-value-bind 
      (second minute hour date month year day daylight-saving-p time-zone)
      (get-decoded-time)
    (declare (ignore second time-zone daylight-saving-p))
    (format nil "~A ~D-~A-~D at ~D:~2,'0D"
	    (day-name day) date (month-name month) (- year 2000) hour minute)))

(defun make-banner ()
  (format nil "~&;Written ~A ~A, clm of ~A" (timestring) (whos-to-blame) *clm-date*))

(defun clm-cleanup ()
  (setf *statistics* nil)
  (setf *interrupted* 0)
  (setf *offset* 0)
  (clm-close-output)
  (clm-close-reverb))

(defun seconds->samples (&rest args)
  (if (= (length args) 1)
      (round (* (first args) *srate*))
    (mapcar #'(lambda (val) (round (* val *srate*))) args)))

(defun samples->seconds (&rest args)
  (if (= (length args) 1)
      (/ (first args) *srate*)
    (mapcar #'(lambda (val) (/ val *srate*)) args)))

(defun times->samples (beg dur)
  (values (seconds->samples beg)
	  (seconds->samples (+ beg dur))))

(defvar clm-start-time nil)
(defvar clm-last-begin-time 0)
(defvar clm-outfile-name nil)
(defvar clm-revfile-name nil)
(defvar clm-max-stat-amp 0.0)

(defun print-statistics (stats out-chans &optional (stream t) scaled) 
  (when stats
    (let* ((total-time (float (/ (- (get-internal-real-time) clm-start-time) internal-time-units-per-second)))
	   (ovals (make-double-array out-chans :initial-element 0.0d0))
	   (times (make-integer-array out-chans :initial-element 0)) ; clisp initializes an explicitly integer array with nil?!?
	   (rev-chans (if (and clm-revfile-name (not *clm-delete-reverb*)) (sound-chans clm-revfile-name) 0))
	   (rvals (and (> rev-chans 0) (make-double-array rev-chans :initial-element 0.0d0)))
	   (rtimes (and (> rev-chans 0) (make-integer-array rev-chans :initial-element 0)))
	   (clm-last-end-time (sound-maxamp clm-outfile-name out-chans ovals times)))
      (if (and clm-revfile-name (not *clm-delete-reverb*)) (sound-maxamp clm-revfile-name rev-chans rvals rtimes))
      (if scaled 
	  (setf clm-max-stat-amp 
	    (max (loop for i from 0 below out-chans maximize (aref ovals i))
		 (if (> rev-chans 0)
		     (loop for i from 0 below rev-chans maximize (aref rvals i))
		   0.0))))
      (print-stats stream stats total-time clm-last-end-time out-chans ovals times rev-chans rvals rtimes))))

(defun print-stats (stream stats total-time clm-last-end-time ochans ovals otimes rchans rvals rtimes)
  (flet ((convert-samples-to-seconds (samp) (if samp (float (/ samp *srate*)) 0.0)))
    (format stream "~A~A~A~:{~%  Out~A max amp~A: ~,3F (near ~,3F sec~:P)~}~A"
	    (format nil "~A: ~%  Duration: ~,4F~A, Last begin time: ~,4F~A~%" 
		    (filename->string clm-outfile-name)
		    (convert-samples-to-seconds clm-last-end-time)
		    (if (< clm-last-end-time 1000) (format nil " (~D sample~:P)" clm-last-end-time) "")
		    (convert-samples-to-seconds clm-last-begin-time)
		    (if (< 0 clm-last-begin-time 1000) (format nil " (sample ~D)" clm-last-begin-time) ""))
	    (format nil "  Compute time: ~,3F, Compute ratio: ~,2F" 
		    total-time
		    (if (not (zerop clm-last-end-time))
			(/ total-time (convert-samples-to-seconds clm-last-end-time))
		      0.0))
	    (if (> total-time 3600)
		(let* ((days (floor total-time (* 24 60 60)))
		       (notdays (- total-time (* days 24 60 60)))
		       (hours (floor notdays (* 60 60)))
		       (nothours (- notdays (* hours 60 60)))
		       (minutes (floor nothours 60))
		       (seconds (- nothours (* minutes 60))))
		  (format nil "~%    (~A~A~A~,3F second~:P, finished~A on ~A)"
			    (if (plusp days) (format nil "~D day~:P, " days) "")
			    (if (plusp hours) (format nil "~D hour~:P, " hours) "")
			    (if (plusp minutes) (format nil "~D minute~:P, " minutes) "")
			    seconds
			    (if (plusp days) " (Laus Deo)" "")
			    (timestring)))
	      "")
	    (loop for i from 0 below ochans
	     collect (list (case i (0 "A") (1 "B") (2 "C") (3 "D") (otherwise (format nil "~D" i)))
			   (if (eq stats :scaled) " (before scaling)" "")
			   (aref ovals i)
			   (convert-samples-to-seconds (aref otimes i))))
	    (if clm-revfile-name
		(format nil "~:{~%  Rev~A max amp~A: ~,3F (near ~,3F sec~:P)~}~%"
			(loop for i from 0 below rchans
			  collect (list (case i (0 "A") (1 "B") (2 "C") (3 "D") (otherwise (format nil "~D" i)))
					(if (eq stats :scaled) " (before scaling)" "")
					(aref rvals i)
					(convert-samples-to-seconds (aref rtimes i)))))
	      ""))))

(defun initialize-statistics (stats ofile &optional rfile) 
  (setf *statistics* stats)
  (setf clm-start-time (get-internal-real-time))
  (setf clm-last-begin-time 0)
  (setf clm-outfile-name ofile)
  (setf clm-revfile-name rfile))

(defun full-directory (path)
  ;; various lisps interpret the directory function definition in cltl2 differently
  #+(or lispworks excl) (let ((next-files (directory path))
	       (all-files nil)
	       (curpath path))
	   (loop while next-files do
	     (setf all-files (append all-files next-files))
	     (setf curpath (concatenate 'string curpath "*/"))
	     (setf next-files (directory curpath)))
	   (if all-files (map 'list #'filename->string all-files)))
  #-(or lispworks excl) (let ((files (map 'list #'filename->string
			   #-(or sbcl clisp) (directory path :all nil)
			   #+clisp (directory (concatenate 'string path "*"))
			   #+sbcl (directory path)))
	      (all-files nil))
	  (loop for file in files do
	    (if (char= (elt file (1- (length file))) #\/)
		(setf all-files (append all-files (full-directory file)))
	      (push file all-files)))
	  all-files)
  )
 
(defun sound-files-in-directory (path)
  (let ((dir (full-directory path))
	(sounds nil))
    (loop for fil in dir do
      (if (null (pathname-name fil))
	  (setf sounds (append sounds (sound-files-in-directory fil)))
	(let ((ext (pathname-type fil)))
	  (when (member ext '("snd" "aiff" "aifc" "wav" "au" "aif" "wve" "voc") :test #'string=)
	    (setf sounds (append sounds (list fil)))))))
    sounds))

(defun search-full-merge-pathnames (pathname &optional default backup)
  ;; this is for reads, not writes -- it returns nil if no file found, so
  ;;   the result needs to be checked before calling namestring
  (let ((nam (probe-file (full-merge-pathnames pathname default))))
    (when (not nam)
      (let ((pathlist (append (list backup) *clm-search-list*)))
	(loop for path in pathlist while (not nam) do
	  (if path
	      (setf nam (probe-file (full-merge-pathnames pathname path)))))))
    #+windoze
    (when (not nam) 
      (setf nam 
	    (probe-file 
	     (full-merge-pathnames 
	      (concatenate 'string (filename->string #+excl (excl:current-directory) #-excl (truename "./")) pathname) 
	      default))))
    nam))

(defun snd-memo (outfile memo-str &rest args)
  (let* ((filename (if (stringp outfile) outfile (mus-file-name outfile)))
	 (memo-file-name (concatenate 'string filename ".scm")))
    (with-open-file (file memo-file-name :direction :output :if-does-not-exist :create :if-exists :append)
      (apply #'format file memo-str args))))

(defmacro add-mark (samp &optional (chan 0))
  `(snd-memo *output* "(add-mark ~D *current-sound* ~D)~%" ,samp ,chan))

(defmacro add-region (beg end)
  `(snd-memo *output* "(make-region ~D ~D *current-sound*)~%" ,beg ,end))

(defun prettified-float (fl)
  (if (and fl (numberp fl))
      (if (integerp fl)
	  (format nil "~D" fl)
	(if (< (abs fl) .0005)
	    "0.0"
	  (format nil "~,3F" fl)))
    (format nil "~A" fl)))

(defun prettified-freq (freq phase &optional (wave-length two-pi))
  (let ((rfrq (if (numberp freq) (round (/ (* freq *srate*) wave-length))))
	(rphase (if (numberp phase) (round (* phase (/ 360.0 wave-length))))))
    (format nil "freq: ~A~A, phase: ~A~A" 
	    (prettified-float freq) (if rfrq (format nil " (~A Hz)" rfrq) "")
	    (prettified-float phase) (if rphase (format nil " (~A degrees)" rphase) ""))))

(defun prettified-array (arr)
  ;; follow *clm-array-print-length* 
  (if arr 
      (if (arrayp arr)
	  (let* ((len (length arr))
		 (lim (if *clm-array-print-length* (min *clm-array-print-length* len) len)))
	    (format nil "~A[~{~A~^, ~}~A]"
		    (if (< lim len) (format nil "[~D]" len) "")
		    (loop for i from 0 below lim collect (prettified-float (aref arr i)))
		    (if (< lim len) ",..." "")))
	(if (listp arr)
	    (let* ((len (length arr))
		   (lim (if *clm-array-print-length* (min *clm-array-print-length* len) len)))
	      (format nil "~A[~{~A~^, ~}~A]"
		      (if (< lim len) (format nil "[~D]" len) "")
		      (loop for i from 0 below lim collect (prettified-float (nth i arr)))
		      (if (< lim len) ",..." "")))
	  arr))))


(defmacro in-hz (val) `(* ,val (/ two-pi *srate*)))
(defun hz->radians (val) (* val (/ two-pi *srate*)))
(defun radians->hz (val) (* val (/ *srate* two-pi)))
(defun degrees->radians (x) (* two-pi (/ x 360))) 
(defun radians->degrees (x) (* x (/ 360.0 two-pi)))
(defun db->linear (x) (expt 10.0 (/ x 20.0)))
(defun linear->db (x) (* 20 (log (max x .00001) 10.0)))

(defun dot-product (in1 in2)
  ;; also known as scalar product, and in orthogonal coordinate systems the same as inner product
  (let ((lim (min (array-dimension in2 0)
		  (array-dimension in1 0)))
	(sum 0.0))
    (loop for i from 0 below lim do
      (incf sum (* (aref in1 i) (aref in2 i))))
    sum))

(defun sine-bank (amps phases)
  (let ((len (length amps))
	(sum 0.0))
    (dotimes (i len)
      (incf sum (* (aref amps i) (sin (aref phases i)))))
    sum))

#|
(defun multiply-arrays (rdat window)
  (let ((len (min (length rdat) (length window))))
    (loop for i from 0 below len do
      (setf (aref rdat i) (* (aref rdat i) (aref window i))))
    rdat))
|#

(defun sqr (x) (* x x))


(defun rectangular->polar (rdat idat)
  (let ((len (length rdat)))
    (loop for i from 0 below len do
      (let ((temp (sqrt (+ (sqr (aref rdat i)) (sqr (aref idat i))))))
	(setf (aref idat i) (- (atan (aref idat i) (aref rdat i))))
	(setf (aref rdat i) temp)))
    rdat))

(defun rectangular->magnitudes (rdat idat)
  (let ((len (length rdat)))
    (loop for i from 0 below len do
      (setf (aref rdat i) (sqrt (+ (sqr (aref rdat i)) (sqr (aref idat i))))))
    rdat))

(defun polar->rectangular (rdat idat)
  (let ((len (length rdat)))
    (loop for i from 0 below len do
      (let* ((mag (aref rdat i))
	     (ang (- (aref idat i)))
	     (temp (* mag (sin ang))))
	(setf (aref rdat i) (* mag (cos ang)))
	(setf (aref idat i) temp)))
    rdat))

(defun clear-array (block)
  (loop for i from 0 below (length block) do (setf (aref block i) (double 0.0)))
  block)

(defun normalize-array (table)
  (let* ((lim (length table))
	 (maxval (loop for i from 0 below lim maximize (abs (aref table i)))))
    (if (and (/= maxval 1.0)		;if 1.0 by some miracle, save us a million no-ops
	     (/= maxval 0.0))		;sigh -- an empty array?
	(loop for i from 0 below lim do (setf (aref table i) (/ (aref table i) maxval))))
    table))



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


(defclass oscil ()
  ((freq :initform nil :initarg :freq :accessor oscil-freq)
   (phase :initform nil :initarg :phase :accessor oscil-phase)))

(defmethod print-object ((gen oscil) stream)
  (format stream "#<oscil: ~A>" (prettified-freq (oscil-freq gen) (oscil-phase gen)))
  gen)

(def-optkey-fun make-oscil ((frequency *clm-default-frequency*) (initial-phase 0.0))
  (make-instance 'oscil
		 :freq (hz->radians frequency)
		 :phase initial-phase))

(defun oscil (gen &optional (fm-input 0.0) (pm-input 0.0))
  (prog1 
      (sin (+ (oscil-phase gen) pm-input))
    (incf (oscil-phase gen) (+ (oscil-freq gen) fm-input))
    ;; if we were being extremely careful, we'd add the fm-input into the sin call at the start too.
    (when (or (> (oscil-phase gen) 100.0) (< (oscil-phase gen) -100.0))
      (setf (oscil-phase gen) (mod (oscil-phase gen) two-pi)))))

(defmethod oscil? ((g oscil)) t)
(defmethod oscil? ((g t)) nil)

(defmethod mus-frequency ((gen oscil)) (radians->hz (oscil-freq gen)))
(defmethod (setf mus-frequency) (val (gen oscil)) (setf (oscil-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen oscil)) (oscil-freq gen))
(defmethod (setf mus-increment) (val (gen oscil)) (setf (oscil-freq gen) val) val)
(defmethod mus-phase ((gen oscil)) (mod (oscil-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen oscil)) (setf (oscil-phase gen) val) val)
(defmethod mus-length ((gen oscil)) 1)
(defmethod mus-run ((gen oscil) &optional (arg1 0.0) (arg2 0.0)) (oscil gen arg1 arg2))


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



;;; Comb filter (a delay line with a scaler on the feedback term)
;;;
;;;    in filter parlance, y(n) <= x(n-D) + scaler * y(n-D)
;;;    As a rule of thumb, the decay time of the feedback part is 7*(delay)/(1-scaler) samples,
;;;    so to get a decay of DUR seconds, scaler <= 1-7*D/(DUR*Srate).  (D=delay length here).
;;;    The peak gain is 1/(1-(abs scaler)).
;;;
;;;    See Julius Smith's "An Introduction to Digital Filter Theory" in Strawn "Digital 
;;;    Audio Signal Processing"


(defclass comb (delay) ())
  
(def-optkey-fun make-comb (scaler size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'comb
		   :loc 0
		   :xscl scaler
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

(defmethod print-object ((d comb) stream)
  (format stream "#<(comb: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun comb (d input &optional (pm 0.0))
  (delay d (+ input (* (dly-xscl d) (tap d pm)))))

(defmethod comb? ((g comb)) t)
(defmethod comb? ((g t)) nil)

(defmethod mus-feedback ((gen comb)) (dly-xscl gen))
(defmethod (setf mus-feedback) (val (gen comb)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen comb) &optional (arg1 0.0) (arg2 0.0)) (comb gen arg1 arg2))
(defmethod mus-interp-type ((gen comb)) (dly-type gen))


;;; filtered-comb filter (a delay line with a scaler on the filtered feedback term)

(defclass filtered-comb (delay)
  ((filter :initform nil :initarg :filter :accessor dly-filter)))
  
(def-optkey-fun make-filtered-comb (scaler size initial-contents initial-element max-size type filter)
  (let ((lsize (round (or max-size size))))
    (make-instance 'filtered-comb
		   :loc 0
		   :xscl scaler
		   :size (floor size)
		   :zsize lsize
		   :zdly max-size
		   :zloc (and max-size (- max-size size))
		   :filter filter
		   :line (if initial-contents 
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize)))
		   :type (or type
			     (if max-size
				 mus-interp-linear
			       mus-interp-none)))))

(defmethod print-object ((d filtered-comb) stream)
  (format stream "#<(filtered-comb: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A, filter: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))
	  (dly-filter d)))

(defun filtered-comb (d input &optional (pm 0.0))
  (delay d (+ input (* (dly-xscl d) (mus-run (dly-filter d) (tap d pm))))))

(defmethod filtered-comb? ((g filtered-comb)) t)
(defmethod filtered-comb? ((g t)) nil)

(defmethod mus-feedback ((gen filtered-comb)) (dly-xscl gen))
(defmethod (setf mus-feedback) (val (gen filtered-comb)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen filtered-comb) &optional (arg1 0.0) (arg2 0.0)) (filtered-comb gen arg1 arg2))
(defmethod mus-interp-type ((gen filtered-comb)) (dly-type gen))



;;; Notch filter (a delay line with a feedforward term) -- also known as inverse comb
;;; see Julius Smith's "Music Applications of Digital Waveguides" for a brief discussion

(defclass notch (delay) ())

(def-optkey-fun make-notch (scaler size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'notch
		   :loc 0
		   :xscl scaler
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

(defmethod print-object ((d notch) stream)
  (format stream "#<(notch: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun notch (d input &optional (pm 0.0))
  (+ (* input (dly-xscl d))
     (delay d input pm)))

(defmethod notch? ((g notch)) t)
(defmethod notch? ((g t)) nil)

(defmethod mus-feedforward ((gen notch)) (dly-xscl gen))
(defmethod (setf mus-feedforward) (val (gen notch)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen notch) &optional (arg1 0.0) (arg2 0.0)) (notch gen arg1 arg2))				      
(defmethod mus-interp-type ((gen notch)) (dly-type gen))



;;; All-pass or "moving moving-average comb" filter
;;;
;;;  (if feedback scaler = 0, we get the moving moving-average comb)
;;;  (if both scale terms = 0, we get a pure delay line)
;;;  (if feedback = -feedforward, we get a Schroeder all-pass)
;;;  In filter parlance, y(n) <= feedforward*x(n) + x(n-D) + feedback*y(n-D)
;;; see Peter Samson's article on the Samson box in Strawn, "Digital Audio Signal Processing" for a diagram,
;;; This is the same as the C version in Ofranidis "Introduction to Signal Processing" p371, given that
;;;   I use tap and delay both as "sD" in his notation.


(defclass all-pass (delay) ())

(def-optkey-fun make-all-pass (feedback feedforward size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'all-pass
		   :loc 0
		   :yscl feedback
		   :xscl feedforward
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

(defmethod print-object ((d all-pass) stream)
  (format stream "#<(all-pass: size: ~A~A, loc: ~A~A, feedback: ~A, :feedforward: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-yscl d) (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun all-pass (d input &optional (pm 0.0))
  (let ((d-in (+ input (* (dly-yscl d) (tap d pm)))))
    (+ (delay d d-in pm)
       (* (dly-xscl d) d-in))))

(defmethod all-pass? ((g all-pass)) t)
(defmethod all-pass? ((g t)) nil)

(defmethod mus-feedback ((gen all-pass)) (dly-yscl gen))
(defmethod (setf mus-feedback) (val (gen all-pass)) (setf (dly-yscl gen) val))
(defmethod mus-feedforward ((gen all-pass)) (dly-xscl gen))
(defmethod (setf mus-feedforward) (val (gen all-pass)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen all-pass) &optional (arg1 0.0) (arg2 0.0)) (all-pass gen arg1 arg2))
(defmethod mus-interp-type ((gen all-pass)) (dly-type gen))



(defclass moving-average (delay) ())

(def-optkey-fun make-moving-average (size initial-contents initial-element)
  (let ((lsize (floor size)))
    (make-instance 'moving-average
		   :loc 0
		   :yscl (/ 1.0 lsize)
		   :xscl (if initial-element
			     (* initial-element lsize)
			   (if initial-contents
			       (apply #'+ initial-contents)
			     0.0))
		   :size lsize
		   :zsize lsize
		   :zdly nil
		   :zloc 0
		   :line (if initial-contents 
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize))))))

(defmethod print-object ((d moving-average) stream)
  (format stream "#<(moving-average: size: ~A, loc: ~A, line: ~A>"
	  (dly-size d) (dly-loc d) (prettified-array (dly-line d))))

(defun moving-average (d input)
  (let ((output (delay d input)))
    (incf (dly-xscl d) (- input output))
    (* (dly-xscl d) (dly-yscl d))))

(defmethod moving-average? ((g moving-average)) t)
(defmethod moving-average? ((g t)) nil)

(defmethod mus-run ((gen moving-average) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (moving-average gen arg1))



(defclass filter ()
  ((order :initform nil :initarg :order :accessor flt-order)
   (x :initform nil :initarg :xcoeffs :accessor flt-x)
   (y :initform nil :initarg :ycoeffs :accessor flt-y)
   (state :initform nil :initarg :state :accessor flt-state)))

(defmethod print-object ((d filter) stream)
  (format stream "#<(filter: order: ~A, xcoeffs: ~A, ycoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-x d))
	  (prettified-array (flt-y d))
	  (prettified-array (flt-state d))))

(def-optkey-fun make-filter (order xcoeffs ycoeffs)
  (let ((len (or order (max (length xcoeffs) (length ycoeffs)))))
    (make-instance 'filter
		   :ycoeffs (if ycoeffs (make-double-array len :initial-contents ycoeffs) (make-double-array len))
		   :xcoeffs (if xcoeffs (make-double-array len :initial-contents xcoeffs) (make-double-array len))
		   :state (make-double-array len)
		   :order len)))

(defmethod filter? ((g filter)) t)
(defmethod filter? ((g t)) nil)

(defun filter (fl inp)
  (let ((xout 0.0))
    (if (flt-y fl)
	(if (flt-x fl)
	    (progn
	      (setf (aref (flt-state fl) 0) (double inp))
	      (loop for j from (1- (flt-order fl)) downto 1 do
		(incf xout (* (aref (flt-state fl) j) (aref (flt-x fl) j)))
		(decf (aref (flt-state fl) 0) (* (aref (flt-y fl) j) (aref (flt-state fl) j)))
		(setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
	      (+ xout (* (aref (flt-state fl) 0) (aref (flt-x fl) 0))))
	  (iir-filter fl inp))
      (fir-filter fl inp))))

(defmethod mus-xcoeffs ((gen filter)) (flt-x gen))
(defmethod mus-ycoeffs ((gen filter)) (flt-y gen))
(defmethod mus-xcoeff ((gen filter) index) (aref (flt-x gen) index))
(defmethod mus-ycoeff ((gen filter) index) (aref (flt-y gen) index))
(defmethod (setf mus-xcoeff) (val (gen filter) index) (setf (aref (flt-x gen) index) val))
(defmethod (setf mus-ycoeff) (val (gen filter) index) (setf (aref (flt-y gen) index) val))
(defmethod mus-order ((gen filter)) (flt-order gen))
(defmethod mus-data ((gen filter)) (flt-state gen))
(defmethod mus-length ((gen filter)) (flt-order gen))
(defmethod mus-run ((gen filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (filter gen arg1))



(defclass fir-filter (filter) ())

(def-optkey-fun make-fir-filter (order x1coeffs coeffs)
  (let* ((xcoeffs (or x1coeffs coeffs))
	 (ord (or order (length xcoeffs))))
    (make-instance 'fir-filter
		   :xcoeffs (if xcoeffs (make-double-array ord :initial-contents xcoeffs) (make-double-array ord))
		   :state (make-double-array ord)
		   :order ord)))

(defmethod print-object ((d fir-filter) stream)
  (format stream "#<(fir-filter: order: ~A, xcoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-x d))
	  (prettified-array (flt-state d))))

(defmethod fir-filter? ((g fir-filter)) t)
(defmethod fir-filter? ((g t)) nil)

(defun fir-filter (fl inp)
  (let ((xout 0.0))
    (setf (aref (flt-state fl) 0) (double inp))
    (loop for j from (1- (flt-order fl)) downto 1 do
      (incf xout (* (aref (flt-state fl) j) (aref (flt-x fl) j)))
      (setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
    (+ xout (* (aref (flt-state fl) 0) (aref (flt-x fl) 0)))))

(defmethod mus-run ((gen fir-filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (fir-filter gen arg1))



(defclass iir-filter (filter) ())

(def-optkey-fun make-iir-filter (order y1coeffs coeffs)
  (let* ((ycoeffs (or y1coeffs coeffs))
	 (ord (or order (length ycoeffs))))
    (make-instance 'iir-filter
		   :ycoeffs (if ycoeffs (make-double-array ord :initial-contents ycoeffs) (make-double-array ord))
		   :state (make-double-array ord)
		   :order ord)))

(defmethod print-object ((d iir-filter) stream)
  (format stream "#<(iir-filter: order: ~A, ycoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-y d))
	  (prettified-array (flt-state d))))

(defmethod iir-filter? ((g iir-filter)) t)
(defmethod iir-filter? ((g t)) nil)

(defun iir-filter (fl inp)
  (setf (aref (flt-state fl) 0) (double inp))
  (loop for j from (1- (flt-order fl)) downto 1 do
    (decf (aref (flt-state fl) 0) (* (aref (flt-y fl) j) (aref (flt-state fl) j)))
    (setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
  (aref (flt-state fl) 0))

(defmethod mus-run ((gen iir-filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (iir-filter gen arg1))



;;; one zero  y(n) = a0 x(n) + a1 x(n-1)

(defclass one-zero ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (a1 :initform nil :initarg :a1 :accessor flt-a1)
   (x1 :initform 0.0 :initarg :x1 :accessor flt-x1)))

(defmethod print-object ((d one-zero) stream)
  (format stream "#<(one-zero: a0: ~A, a1: ~A, x1: ~A>"
	  (flt-a0 d) (flt-a1 d) (flt-x1 d)))

(def-optkey-fun make-one-zero (a0 a1)
  (make-instance 'one-zero :a0 a0 :a1 a1))

(defmethod one-zero? ((g one-zero)) t)
(defmethod one-zero? ((g t)) nil)

(defun one-zero (f input) 
  (let ((val (+ (* (flt-a0 f) input) (* (flt-a1 f) (flt-x1 f)))))
    (setf (flt-x1 f) input)
    val))

(defmethod mus-order ((gen one-zero)) 1)
(defmethod mus-run ((gen one-zero) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (one-zero gen arg1))
(defmethod mus-xcoeff ((gen one-zero) loc)
  (if (= loc 0)
      (flt-a0 gen)
    (flt-a1 gen)))
(defmethod (setf mus-xcoeff) (val (gen one-zero) loc)
  (if (= loc 0)
      (setf (flt-a0 gen) val)
    (setf (flt-a1 gen) val)))



;;; one-pole  y(n) = a0 x(n) - b1 y(n-1)

(defclass one-pole ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (b1 :initform nil :initarg :b1 :accessor flt-b1)
   (y1 :initform 0.0 :initarg :y1 :accessor flt-y1)))

(defmethod print-object ((d one-pole) stream)
  (format stream "#<(one-pole: a0: ~A, b1: ~A, y1: ~A>"
	  (flt-a0 d) (flt-b1 d) (flt-y1 d)))

(def-optkey-fun make-one-pole (a0 b1)
  (make-instance 'one-pole :a0 a0 :b1 b1))

(defmethod one-pole? ((g one-pole)) t)
(defmethod one-pole? ((g t)) nil)

(defun one-pole (f input)
  (setf (flt-y1 f) (- (* (flt-a0 f) input) (* (flt-b1 f) (flt-y1 f)))))

(defmethod mus-order ((gen one-pole)) 1)
(defmethod mus-run ((gen one-pole) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (one-pole gen arg1))
(defmethod mus-xcoeff ((gen one-pole) loc)
  (if (= loc 0)
      (flt-a0 gen)))
(defmethod mus-ycoeff ((gen one-pole) loc)
  (if (= loc 1)
      (flt-b1 gen)))
(defmethod (setf mus-xcoeff) (val (gen one-pole) index)
  (declare (ignore index))
  (setf (flt-a0 gen) val))
(defmethod (setf mus-ycoeff) (val (gen one-pole) index)
  (declare (ignore index))
  (setf (flt-b1 gen) val))


;;; two-pole  y(n) = a0 x(n) - b1 y(n-1) - b2 y(n-2)

(defclass two-pole ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (b1 :initform nil :initarg :b1 :accessor flt-b1)
   (b2 :initform nil :initarg :b2 :accessor flt-b2)
   (y1 :initform 0.0 :initarg :y1 :accessor flt-y1)
   (y2 :initform 0.0 :initarg :y2 :accessor flt-y2)))

(defmethod print-object ((d two-pole) stream)
  (format stream "#<(two-pole: a0: ~A, b1: ~A, b2: ~A, y1: ~A, y2: ~A>"
	  (flt-a0 d) (flt-b1 d) (flt-b2 d) (flt-y1 d) (flt-y2 d)))

(defmethod two-pole? ((g two-pole)) t)
(defmethod two-pole? ((g t)) nil)

(defun make-two-pole-base (a0 b1 b2)
  (if (>= (abs b1) 2.0)
      (format t "unstable two-pole filter, b1=~,3F ~A ~A" b1 (if (minusp b1) "<=" ">=") (if (minusp b1) "-2.0" "2.0"))
    (if (>= (abs b2) 1.0)
	(format t "unstable two-pole filter, b2=~,3F ~A ~A" b1 (if (minusp b2) "<=" ">=") (if (minusp b2) "-1.0" "1.0"))
      (if (and (>= (- (* b1 b1) (* b2 4.0)) 0.0)
	       (or (>= (+ b1 b2) 1.0)
		   (>= (- b2 b1) 1.0)))
	  (format t "unstable filter: b1=~,3F, b2=~,3F" b1 b2))))
  (make-instance 'two-pole :a0 a0 :b1 b1 :b2 b2))

(def-optkey-fun make-two-pole (a0 b1 b2 frequency radius)
  (if (or radius frequency (and (not b2) (>= b1 2.0)))
      (make-two-pole-base 1.0 
			  (- (* 2.0 (or radius a0) (cos (hz->radians (or frequency b1)))))
			  (* (or radius a0) (or radius a0)))
    (make-two-pole-base a0 b1 b2)))

(defun two-pole (f input)
  (let ((y0 (- (* (flt-a0 f) input) 
	       (* (flt-b1 f) (flt-y1 f)) 
	       (* (flt-b2 f) (flt-y2 f)))))
    (setf (flt-y2 f) (flt-y1 f))
    (setf (flt-y1 f) y0)
    y0))

(defmethod mus-order ((gen two-pole)) 2)

(defmethod mus-run ((gen two-pole) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (two-pole gen arg1))

(defmethod mus-xcoeff ((gen two-pole) loc)
  (if (= loc 0)
      (flt-a0 gen)))

(defmethod mus-ycoeff ((gen two-pole) loc)
  (if (= loc 1)
      (flt-b1 gen)
    (flt-b2 gen)))

(defmethod (setf mus-xcoeff) (val (gen two-pole) index)
  (declare (ignore index))
  (setf (flt-a0 gen) val))

(defmethod (setf mus-ycoeff) (val (gen two-pole) index)
  (if (= index 1)
      (setf (flt-b1 gen) val)
    (setf (flt-b2 gen) val)))

(defmethod mus-scaler ((gen two-pole))
  (sqrt (flt-b2 gen)))

(defmethod (setf mus-scaler) (val (gen two-pole))
  (setf (flt-b1 gen) (* -2.0 val (cos (hz->radians (mus-frequency gen)))))
  (setf (flt-b2 gen) (* val val))
  val)

(defmethod mus-frequency ((gen two-pole))
  (radians->hz (acos (/ (flt-b1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (val (gen two-pole))
  (setf (flt-b1 gen) (* -2.0 (mus-scaler gen) (cos (hz->radians val))))
  val)


;;; two-zero  y(n) = a0 x(n) + a1 x(n-1) + a2 x(n-2)

(defclass two-zero ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (a1 :initform nil :initarg :a1 :accessor flt-a1)
   (a2 :initform nil :initarg :a2 :accessor flt-a2)
   (x1 :initform 0.0 :initarg :x1 :accessor flt-x1)
   (x2 :initform 0.0 :initarg :x2 :accessor flt-x2)))

(defmethod print-object ((d two-zero) stream)
  (format stream "#<(two-zero: a0: ~A, a1: ~A, a2: ~A, x1: ~A, x2: ~A>"
	  (flt-a0 d) (flt-a1 d) (flt-a2 d) (flt-x1 d) (flt-x2 d)))

(defmethod two-zero? ((g two-zero)) t)
(defmethod two-zero? ((g t)) nil)

(def-optkey-fun make-two-zero (a0 a1 a2 frequency radius)
  (if (or radius frequency (and (not a2) (> a1 20.0)))
      (make-instance 'two-zero 
		     :a0 1.0 
		     :a1 (- (* 2.0 (or radius a0) (cos (hz->radians (or frequency a1))))) 
		     :a2 (* (or radius a0) (or radius a0)))
    (make-instance 'two-zero :a0 a0 :a1 a1 :a2 a2)))

(defun two-zero (f input)
  (let ((y0 (+ (* (flt-a0 f) input)
	       (* (flt-a1 f) (flt-x1 f))
	       (* (flt-a2 f) (flt-x2 f)))))
    (setf (flt-x2 f) (flt-x1 f))
    (setf (flt-x1 f) input)
    y0))

(defmethod mus-order ((gen two-zero)) 2)

(defmethod mus-run ((gen two-zero) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (two-zero gen arg1))

(defmethod mus-xcoeff ((gen two-zero) loc)
  (if (= loc 0)
      (flt-a0 gen)
    (if (= loc 1)
	(flt-a1 gen)
      (flt-a2 gen))))

(defmethod (setf mus-xcoeff) (val (gen two-zero) loc)
  (if (= loc 0)
      (setf (flt-a0 gen) val)
    (if (= loc 1)
	(setf (flt-a1 gen) val)
      (setf (flt-a2 gen) val))))

(defmethod mus-scaler ((gen two-zero))
  (sqrt (flt-a2 gen)))

(defmethod (setf mus-scaler) (val (gen two-zero))
  (setf (flt-a1 gen) (* -2.0 val (cos (hz->radians (mus-frequency gen)))))
  (setf (flt-a2 gen) (* val val))
  val)

(defmethod mus-frequency ((gen two-zero))
  (radians->hz (acos (/ (flt-a1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (val (gen two-zero))
  (setf (flt-a1 gen) (* -2.0 (mus-scaler gen) (cos (hz->radians val))))
  val)



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




;;; Rand and Rand-Interp
;;;
;;;    rand latches its output random number, getting a new number 
;;;    every srate/freq samples -- internally we pretend that our cycle is between 0 and
;;;    two-pi so that the caller can use hz->radians without confusion.  This way, 
;;;    frequency calculations look the same between oscil and rand and so on.
;;;    rand-interp interpolates between successive random numbers.

(defun ran (lo hi)			;returns random numbers between lo and hi
  (if (= hi lo) 
      lo
    #-(and excl cltl2) (+ lo (random (- hi lo)))
    #+(and excl cltl2) (+ lo (* (- hi lo) (random 1.0f0)))
    ))

(defun centered-random (n)          ;[-n .. n] not lisp's [0.0 .. n]
  (if (zerop n) n			;don't die just because n happens to touch 0!
    (- (random (* 2 n)) n)))

(defun mus-random (n) (centered-random n))

(defun inverse-integrate (dist &optional (data-size 512) (e-size 50))
  (let* ((e '())
	 (sum (cadr dist))
	 (first-sum sum)
	 (data (make-double-array data-size))
	 (x0 (car dist))
	 (x1 (nth (- (length dist) 2) dist))
	 (xincr (/ (- x1 x0) e-size)))
    (do ((i 0 (1+ i))
	 (x x0 (+ x xincr)))
	((> i e-size))
      (setf e (cons sum e))
      (setf e (cons x e))
      (setf sum (+ sum (envelope-interp x dist))))
    (let* ((incr (/ (- (cadr e) first-sum) (- data-size 1))))
      (setf e (reverse e))
      (do ((i 0 (1+ i))
	   (x first-sum (+ x incr)))
	  ((= i data-size))
	(setf (aref data i) (double (envelope-interp x e))))
      data)))

(defclass rand ()
  ((freq :initform nil :initarg :freq :accessor noi-freq)
   (base :initform nil :initarg :base :accessor noi-base)
   (phase :initform nil :initarg :phase :accessor noi-phase)
   (incr :initform nil :initarg :incr :accessor noi-incr)
   (output :initform nil :initarg :output :accessor noi-output)
   (distribution :initform nil :initarg :distribution :accessor noi-distribution)
   (distribution-size :initform nil :initarg :distribution-size :accessor noi-distribution-size)))

(defmethod print-object ((d rand) stream)
  (format stream "#<rand: ~A, amplitude: ~A~A>"
	  (prettified-freq (noi-freq d) (noi-phase d))
	  (prettified-float (noi-base d))
	  (if (noi-distribution d)
	      ", with distribution"
	    "")))

(def-optkey-fun make-rand ((frequency *clm-default-frequency*) (amplitude 1.0) (envelope nil) (distribution nil))
  (make-instance 'rand
		 :freq (hz->radians frequency)
		 :base amplitude
		 :phase 0.0
		 :incr 0.0
		 :output 0.0
		 :distribution (or (and envelope (inverse-integrate envelope))
				   distribution)
		 :distribution-size (if envelope 512
				      (if distribution (length distribution)
					0))))

(defmethod rand? ((g rand)) t)
(defmethod rand? ((g t)) nil)

(defun random-any (r)
  (if (noi-distribution r)
      (* (noi-base r)
	 (array-interp (noi-distribution r)
		       (random (float (noi-distribution-size r)))
		       (noi-distribution-size r)))
    (if (zerop (noi-base r))
	0.0
      (- (random (* 2.0 (noi-base r)))
	 (noi-base r)))))

(defun rand (r &optional (sweep 0.0))
  (progn
    (if (>= (noi-phase r) two-pi)
	(progn
	  (loop while (>= (noi-phase r) two-pi) do (decf (noi-phase r) two-pi))
	  (setf (noi-output r) (random-any r))))
    (incf (noi-phase r) (+ (noi-freq r) sweep))
    (loop while (minusp (noi-phase r)) do (incf (noi-phase r) two-pi))
    (noi-output r)))

(defmethod mus-frequency ((gen rand)) (radians->hz (noi-freq gen)))
(defmethod (setf mus-frequency) (val (gen rand)) (setf (noi-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen rand)) (noi-freq gen))
(defmethod (setf mus-increment) (val (gen rand)) (setf (noi-freq gen) val) val)
(defmethod mus-phase ((gen rand)) (mod (noi-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen rand)) (setf (noi-phase gen) val) val)
(defmethod mus-scaler ((gen rand)) (noi-base gen))
(defmethod (setf mus-scaler) (val (gen rand)) (setf (noi-base gen) val))
(defmethod mus-run ((gen rand) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (rand gen arg1))
(defmethod mus-length ((gen rand)) (noi-distribution-size gen))
(defmethod mus-data ((gen rand)) (noi-distribution gen))


(defclass rand-interp (rand) ())

(def-optkey-fun make-rand-interp ((frequency *clm-default-frequency*) (amplitude 1.0) (envelope nil) (distribution nil))
  (make-instance 'rand-interp
		 :freq (hz->radians frequency)
		 :base amplitude
		 :phase 0.0
		 :output 0.0
		 :incr (if (zerop amplitude) 
			   0.0 
			 (* (random amplitude) (/ frequency *srate*)))
		 :distribution (or (and envelope (inverse-integrate envelope))
				   distribution)
		 :distribution-size (if envelope 512
				      (if distribution (length distribution)
					0))))

(defmethod print-object ((d rand-interp) stream)
  (format stream "#<rand-interp: ~A, amplitude: ~A, increment: ~A~A>"
	  (prettified-freq (noi-freq d) (noi-phase d))
	  (prettified-float (noi-base d))
	  (prettified-float (noi-incr d))
	  (if (noi-distribution d)
	      ", with distribution"
	    "")))

(defmethod rand-interp? ((g rand-interp)) t)
(defmethod rand-interp? ((g t)) nil)

(defun rand-interp (r &optional (sweep 0.0))
  (prog1
      (incf (noi-output r) (noi-incr r))
    (when (>= (noi-phase r) two-pi)
      (loop while (>= (noi-phase r) two-pi) do (decf (noi-phase r) two-pi))
      (setf (noi-incr r) (* (- (random-any r)
			       (noi-output r)) 
			    (/ (+ (noi-freq r) sweep) two-pi))))
    ;; the (+ freq sweep) is obviously just a wild guess at the current "frequency"
    (incf (noi-phase r) (+ (noi-freq r) sweep))
    (loop while (minusp (noi-phase r)) do (incf (noi-phase r) two-pi))))

(defmethod mus-frequency ((gen rand-interp)) (radians->hz (noi-freq gen)))
(defmethod (setf mus-frequency) (val (gen rand-interp)) (setf (noi-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen rand-interp)) (noi-freq gen))
(defmethod (setf mus-increment) (val (gen rand-interp)) (setf (noi-freq gen) val) val)
(defmethod mus-phase ((gen rand-interp)) (mod (noi-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen rand-interp)) (setf (noi-phase gen) val) val)
(defmethod mus-run ((gen rand-interp) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (rand-interp gen arg1))
(defmethod mus-length ((gen rand-interp)) (noi-distribution-size gen))
(defmethod mus-data ((gen rand-interp)) (noi-distribution gen))



;;; Envelopes
;;; magify-seg takes an envelope, a starting time in samples, the envelope duration in samples, 
;;; and a y scaler.  It returns another seg-like list (i.e. a list of time-value pairs), 
;;; where the times are pass numbers, and the values are increments to be added on each pass to 
;;; get to the next y-value.   For very large envelopes, (say more than 50 segments), we should 
;;; simply load the thing into an array and use table-lookup to read it out.

(defun magify-seg (envelope duration-in-samples scaler &optional (stepit nil) (offset 0.0))
  (let* ((lim (- (length envelope) 2))
	 (x0 0.0) (y0 0.0) (cur-x 0.0) (y-incr 0.0)
	 (x1 (car envelope))
	 (result nil)
	 (cur-pass 0)
	 (x-diff (- (nth lim envelope) x1))) ; x1 is really x0 here
    (if (zerop x-diff) (warn "envelope repeats x axis values: ~A" envelope))
    (let* ((x-mag (/ (1- duration-in-samples) x-diff)))
      (if (zerop x-mag) 
	  (let ((dur (clm-cerror "use 1.0 for laughs" 1.0 #'plusp "envelope duration is 0.0: ~A" envelope)))
	    (setf x-mag (/ (1- (floor (* *srate* dur))) x-diff))))
      (let* ((inv-x-mag (/ 1.0 x-mag))
	     (y1 (cadr envelope)))
	(loop for i from 0 by 2 below lim and n2 in (cddr envelope) by #'cddr and n3 in (cdddr envelope) by #'cddr do
	  (setf x0 x1)
	  (setf x1 n2)
	  (setf y0 y1)
	  (setf y1 n3)
	  (setf cur-x (max 1 (round (* x-mag (- x1 x0)))))
	  (setf x1 (+ x0 (* inv-x-mag cur-x)))
	  (push cur-pass result)
	  (if (not stepit)
	      (if (= y0 y1)		;no change in y on this segment
		  (setf y-incr 0)    
		(setf y-incr (* scaler (/ (- y1 y0) cur-x))))
	    (setf y-incr (+ offset (* scaler y0))))
	  (push y-incr result)
	  (incf cur-pass cur-x))
	(nreverse result)))))

(defun fix-up-exp-env (e off scl base)
  (if e
      (let* ((min-y (+ off (* scl (cadr e))))
	     (max-y min-y)
	     (val 0.0)
	     (tmp 0.0)
	     (nb (and base (not (zerop base)) (/= 1.0 base)))
	     (b (if nb (/ 1.0 (log base))))
	     (b-1 (if nb (- base 1.0)))
	     (result nil)
	     (flat nil)
	     (len (length e)))
	(loop for i from 1 below len by 2 and ni in (cdr e) by #'cddr and ni-1 in e by #'cddr do
	  (setf val (+ off (* scl ni)))
	  (setf min-y (min min-y val))
	  (setf max-y (max max-y val))
	  (push ni-1 result)
	  (push val result))
	(setf result (nreverse result))
	(setf flat (= min-y max-y))
	(if (not flat) (setf val (/ 1.0 (- max-y min-y))))
	(loop for i from 1 below len by 2 do
	  (if (not flat)
	      (setf tmp (* val (- (nth i result) min-y)))
	    (setf tmp 1.0))
	  ;; tmp is now a number between 0 and 1, we need the power that will give us that number given base...
	  (if nb 
	      (setf (nth i result) (* (log (+ 1.0 (* tmp b-1))) b))
	    (setf (nth i result) tmp)))
	;; that is -- ((base^x)-1)/(base-1) solved for x in terms of base.
	(values result min-y max-y))
    (values nil 0)))

(defclass seg ()
  ((current-value :initform nil :initarg :current-value :accessor seg-current-value)
   (rate :initform nil :initarg :rate :accessor seg-rate)
   (data :initform nil :initarg :data :accessor seg-data)
   (pass :initform nil :initarg :pass :accessor seg-pass)
   (base :initform nil :initarg :base :accessor seg-base)
   (scaler :initform nil :initarg :scaler :accessor seg-scaler)
   (offset :initform nil :initarg :offset :accessor seg-offset)
   (original-scaler :initform nil :initarg :original-scaler :accessor seg-original-scaler)
   (original-offset :initform nil :initarg :original-offset :accessor seg-original-offset)
   (power :initform nil :initarg :power :accessor seg-power)
   (end :initform nil :initarg :end :accessor seg-end)
   (restart-data :initform nil :initarg :restart-data :accessor seg-restart-data)
   (restart-power :initform nil :initarg :restart-power :accessor seg-restart-power)
   (restart-y :initform nil :initarg :restart-y :accessor seg-restart-y)
   (type :initform nil :initarg :type :accessor seg-type)
   (original-data :initform nil :initarg :original-data :accessor seg-original-data)))

(defmethod print-object ((d seg) stream)
  (format stream "#<env: current-value: ~A, rate: ~A, data: ~A, offset: ~A, scaler: ~A, base: ~A, power: ~A, type: ~A, end: ~A, pass: ~A>"
	  (prettified-float (seg-current-value d))
	  (prettified-float (seg-rate d))
	  (prettified-array (seg-data d))
	  (prettified-float (seg-offset d))
	  (prettified-float (seg-scaler d))
	  (prettified-float (seg-base d))
	  (prettified-float (seg-power d))
	  (seg-type d) (seg-end d) (seg-pass d)))

(defmethod env? ((g seg)) t)
(defmethod env? ((g t)) nil)

(defmethod mus-location ((gen seg)) (seg-pass gen))
(defmethod mus-scaler ((gen seg)) (seg-original-scaler gen))
(defmethod mus-offset ((gen seg)) (seg-original-offset gen))
(defmethod mus-data ((gen seg)) (seg-original-data gen))
(defmethod mus-length ((gen seg)) (seg-end gen))

(defmethod (setf mus-location) (val (gen seg))
  ;; apparently named access-env in CLM-1
  (mus-reset gen)
  (let ((ctr 0))
    (setf (seg-pass gen) (min val (seg-end gen)))
    (loop while (and (seg-data gen) (< ctr val)) do
      (setf (seg-rate gen) (cadr (seg-data gen)))
      (setf (seg-data gen) (cddr (seg-data gen)))
      (let ((passes (- (min (or (car (seg-data gen)) (seg-end gen)) val) ctr)))
	(incf ctr passes)
	(if (eq (seg-type gen) :seg)
	    (if (or (null (seg-base gen))
		    (not (zerop (seg-base gen))))
		(incf (seg-current-value gen) (* passes (seg-rate gen)))
	      (setf (seg-current-value gen) (seg-rate gen)))
	  (progn			;type = :exp
	    (incf (seg-power gen) (* passes (seg-rate gen)))
	    (setf (seg-current-value gen) 
	      (+ (seg-offset gen)
		 (* (seg-scaler gen) 
		    (- (expt (seg-base gen) (seg-power gen)) 1.0))))))))))

(defun clm-flatten (L)			;borrowed from /dist/lisp/mac/Lisp-Utilities/extensions.lisp
  (cond ((null L) '())
	((atom L) L)
	((consp L)
	 (if (consp (car L))
	     (append (clm-flatten (car L)) (clm-flatten (cdr L)))
	   (cons (car L) (clm-flatten (cdr L)))))
	(t L)))

(def-optkey-fun make-env (envelope (scaler 1.0) duration (offset 0.0) base end length)
  (if (and envelope ; can be nil -> '(0 0 1 0)
	   (listp envelope)
	   (car envelope)
	   (listp (car envelope)))
      (setf envelope (clm-flatten envelope)))
  (if (and base (numberp base) (minusp base))
      (warn "make-env with :base ~,3F won't work -- the 'base' has to be 0.0 or greater.~
             If you're trying to get convex connecting segments, use a base between 0.0 and 1.0." base))
  (if (and (null duration) (null end) (null length)) 
      (error "make-env needs either :duration, :end, or :length"))
  (let ((dur-in-samples (or length
			    (and end (floor (1+ end)))
			    (floor (* (or duration 0.0) *srate*)))))
    (if (zerop dur-in-samples) (error "make-env with 0 duration?"))
    (let* ((checked-envelope (if (or (not envelope) (= (length envelope) 1))
				 (list 0 0 1 0)
			       (if (= (length envelope) 2)
				   (list (car envelope) (cadr envelope) (1+ (car envelope)) (cadr envelope))
				 envelope)))
	   (y0 (cadr checked-envelope))
	   (init-y (+ offset (* scaler y0))))
      
      (when #-(and sbcl (not little-endian)) (> *safety* 0)
	    #+(and sbcl (not little-endian)) t
	    (let ((x0 (first checked-envelope)))
	      (loop for x1 in (cddr checked-envelope) by #'cddr do
		    (if (< x1 x0) 
			(error "X axis values out of order in: '~A going from ~A to ~A" envelope x0 x1))
		    (setf x0 x1))))
      
      (if (or (null base) (= base 1) (= base 0))
	  (let ((data (magify-seg checked-envelope dur-in-samples scaler (and (numberp base) (zerop base)) offset)))
	    (make-instance 'seg
			   :current-value init-y
			   :offset offset
			   :scaler scaler
			   :rate 0.0
			   :base (or base 1.0)
			   :power nil
			   :end (1- dur-in-samples)
			   :original-offset offset
			   :original-scaler scaler
			   :original-data (copy-list envelope)
			   :pass 0
			   :data data
			   :type :seg
				 :restart-y init-y
				 :restart-power 0.0
				 :restart-data data))
	(multiple-value-bind 
	 (new-e min-y max-y)
	 (fix-up-exp-env checked-envelope offset scaler base)
	 (let ((data (magify-seg new-e dur-in-samples 1.0 nil 0.0)))
	   (make-instance 'seg
			  :current-value init-y
			  :power (cadr new-e)
			  :base base
			  :pass 0
			  :end (1- dur-in-samples)
			  :offset min-y
			  :scaler (/ (- max-y min-y) (- base 1.0))
			  :offset offset
			  :rate 0.0
			  :type :exp
				:data data
				:original-offset offset
				:original-scaler scaler
				:original-data (copy-list envelope)
				:restart-y init-y
				:restart-power (cadr new-e)
				:restart-data data)))))))
  
(defun restart-env (e)
  (mus-reset e))

(defun env-interp (x e &optional base)
  (if (eq (seg-type e) :seg)
      (+ (seg-offset e) (* (seg-scaler e) (envelope-interp x (seg-original-data e) (or base 1.0))))
    (envelope-interp x (seg-original-data e) (or base (seg-base e)))))


(defun env (e)
  (if (eq (seg-type e) :seg)
      (prog1
	  (seg-current-value e)
	(when (and (seg-data e)	;are there any segments queued up?
		   (>= (seg-pass e) (car (seg-data e))))
	  (setf (seg-rate e) (cadr (seg-data e)))
	  (setf (seg-data e) (cddr (seg-data e))))
	(incf (seg-pass e))
	(if (or (null (seg-base e))
		(not (zerop (seg-base e))))
	    (if (and (/= 0.0 (seg-rate e))
		     (<= (seg-pass e) (seg-end e)))
		(incf (seg-current-value e) (seg-rate e)))
	  (setf (seg-current-value e) (seg-rate e))))
    (if (eq (seg-type e) :exp)	;exponential interpolation between break-points
	(prog1
	    (seg-current-value e)
	  (when (and (seg-data e)
		     (>= (seg-pass e) (car (seg-data e))))
	    (setf (seg-rate e) (cadr (seg-data e)))
	    (setf (seg-data e) (cddr (seg-data e))))
	  (incf (seg-pass e))
	  (when (and (/= 0.0 (seg-rate e))
		     (<= (seg-pass e) (seg-end e)))
	    (incf (seg-power e) (seg-rate e))
	    (setf (seg-current-value e) 
		  (+ (seg-offset e)
		     (* (seg-scaler e) 
			(- (expt (seg-base e) (seg-power e)) 1.0))))))
      (error "unknown envelope type: ~A" (seg-type e)))))

(defmethod mus-run ((gen seg) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (env gen))
(defmethod mus-increment ((gen seg)) (seg-base gen))



(defclass triangle-wave ()
  ((current-value :initform nil :initarg :current-value :accessor sw-current-value)
   (freq :initform nil :initarg :freq :accessor sw-freq)
   (phase :initform nil :initarg :phase :accessor sw-phase)
   (base :initform nil :initarg :base :accessor sw-base)))

(defmethod print-object ((d triangle-wave) stream)
  (format stream "#<triangle-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))
	
(defun fix-up-phase (s)
  (if (plusp (sw-phase s))
      (loop while (>= (sw-phase s) two-pi) do (decf (sw-phase s) two-pi))
    (loop while (minusp (sw-phase s)) do (incf (sw-phase s) two-pi))))

(defun tri-val (amplitude phase)
  (* amplitude (if (< phase (/ pi 2.0)) phase 
		 (if (< phase (/ (* 3.0 pi) 2.0))
		     (- pi phase)
		   (- phase two-pi)))))

(def-optkey-fun make-triangle-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0))
  (make-instance 'triangle-wave
		 :current-value (/ (tri-val amplitude initial-phase) (/ pi 2.0))
		 :base (/ (* 2 amplitude) pi)
		 :phase initial-phase
		 :freq (hz->radians frequency)))

(defmethod triangle-wave? ((g triangle-wave)) t)
(defmethod triangle-wave? ((g t)) nil)

(defun triangle-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi))
	(fix-up-phase s))
    (setf (sw-current-value s) (tri-val (sw-base s) (sw-phase s)))))

;;; old method using increments tended to wander (and was off by a factor of two)

(defmethod mus-frequency ((gen triangle-wave)) (radians->hz (sw-freq gen)))
(defmethod (setf mus-frequency) (val (gen triangle-wave)) (setf (sw-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen triangle-wave)) (sw-freq gen))
(defmethod (setf mus-increment) (val (gen triangle-wave)) (setf (sw-freq gen) val) val)
(defmethod mus-phase ((gen triangle-wave)) (mod (sw-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen triangle-wave)) (setf (sw-phase gen) val) val)
(defmethod mus-scaler ((gen triangle-wave)) (/ (* pi (sw-base gen)) 2.0))
(defmethod (setf mus-scaler) (val (gen triangle-wave)) (setf (sw-base gen) (/ (* 2.0 val) pi)) val)
(defmethod mus-run ((gen triangle-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (triangle-wave gen arg1))



(defclass square-wave (triangle-wave)
  ((width :initform pi :initarg :width :accessor sw-width)))

(def-optkey-fun make-square-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0))
  (make-instance 'square-wave
		 :current-value (if (< initial-phase pi) 0.0 amplitude)
		 :base amplitude
		 :phase initial-phase
		 :width pi
		 :freq (hz->radians frequency)))

(defmethod print-object ((d square-wave) stream)
  (format stream "#<square-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod square-wave? ((g square-wave)) t)
(defmethod square-wave? ((g t)) nil)

(defun square-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi)) 
	(fix-up-phase s))
    (setf (sw-current-value s) (if (< (sw-phase s) (sw-width s)) (sw-base s) 0.0))))

(defmethod mus-scaler ((gen square-wave)) (sw-base gen))
(defmethod (setf mus-scaler) (val (gen square-wave)) (setf (sw-base gen) val) val)
(defmethod mus-run ((gen square-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (square-wave gen arg1))
(defmethod mus-width ((gen square-wave)) (/ (sw-width gen) (* 2 pi)))
(defmethod (setf mus-width) (val (gen square-wave)) (setf (sw-width gen) (* 2 pi val)) val)


(defclass sawtooth-wave (triangle-wave) ())

(def-optkey-fun make-sawtooth-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase pi))
  (make-instance 'sawtooth-wave
		 :current-value (* amplitude (/ (- initial-phase pi) pi))
		 :base (/ amplitude pi)
		 :phase initial-phase
		 :freq (hz->radians frequency)))

(defmethod print-object ((d sawtooth-wave) stream)
  (format stream "#<sawtooth-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod sawtooth-wave? ((g sawtooth-wave)) t)
(defmethod sawtooth-wave? ((g t)) nil)

(defun sawtooth-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi))
	(fix-up-phase s))
    (setf (sw-current-value s) (* (sw-base s) (- (sw-phase s) pi)))))

(defmethod mus-scaler ((gen sawtooth-wave)) (* pi (sw-base gen)))
(defmethod (setf mus-scaler) (val (gen sawtooth-wave)) (setf (sw-base gen) (/ val pi)) val)
(defmethod mus-run ((gen sawtooth-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (sawtooth-wave gen arg1))


(defclass pulse-train (triangle-wave) ())

(def-optkey-fun make-pulse-train ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase two-pi))
  (make-instance 'pulse-train
		 :current-value 0.0
		 :base amplitude		; another version alternates sign
		 :phase initial-phase		; this will give us an immediate pulse
		 :freq (hz->radians frequency)))

(defmethod print-object ((d pulse-train) stream)
  (format stream "#<pulse-train: ~A, amplitude: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod pulse-train? ((g pulse-train)) t)
(defmethod pulse-train? ((g t)) nil)

(defun pulse-train (s &optional (fm 0.0))
  (prog1
      (if (>= (abs (sw-phase s)) two-pi)
	  (progn
	    (fix-up-phase s)
	    (sw-base s))		;triggered upon overflow in a sense, so will jitter around if period not integer
					; use ncos for a better pulse
	0.0)
    (incf (sw-phase s) (+ (sw-freq s) fm))))

(defmethod mus-scaler ((gen pulse-train)) (sw-base gen))
(defmethod (setf mus-scaler) (val (gen pulse-train)) (setf (sw-base gen) val) val)
(defmethod mus-run ((gen pulse-train) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (pulse-train gen arg1))


;;; Waveshaping
;;;    see "Digital Waveshaping Synthesis" by Marc Le Brun in JAES 1979 April, vol 27, no 4, p250

(defun signify (harm-amps)		;taken very directly from MLB's Mus10 code.  
					;Here we assume Harm-amps is ordered by partial number.
  (let ((lastpt (length harm-amps)))
    (do ((i 2 (+ i di))
	 (di 1 (- 4 di)))		;successively 1 and 3, getting the pattern + + - - + + ...
	((>= i lastpt) harm-amps)
      (setf (aref harm-amps i) (double (- (aref harm-amps i)))))))

; T(n+1) <= 2xT(n)-T(n-1) gives the Chebychev polynomials of the first kind 
; (T0 = 1, T1 = X to get recursion going)

; we take the array of signified partial amplitudes (harm-amps) and use them to weight the 
; associated Chebychev polynomial

;;; assume we're using synth-data that looks like additive synthesis tables 
;;; (i.e. a list of partial-amp pairs).  That means we have to prepare it for 
;;; the preceding two procedures by loading it into an array.

(defun normalize-partials (partials)
  (let ((sum 0.0))
    (loop for i in (cdr partials) by #'cddr do (incf sum (abs i)))
    (if (zerop sum) (warn "all partials have 0.0 amplitude: ~A" partials))
    (setf sum (/ 1.0 sum))
    (do ((i 1 (+ i 2)))
	((>= i (length partials)) partials)
      (setf (nth i partials) (* (nth i partials) sum)))))

(defun highest-partial (data)
  (if (endp data) 0.0
    (max (car data) (highest-partial (cddr data)))))

(defun massage-partials (data)
  (do* ((i 0 (+ i 2))
	(lim (length data))
	(maxH (highest-partial data))
	(hamps (make-double-array (+ maxH 1))))
      ((>= i lim) hamps)
    (setf (aref hamps (nth i data)) (double (nth (+ i 1) data)))))



(defclass polyshape ()
  ((freq :initform nil :initarg :freq :accessor ws-freq)
   (phase :initform nil :initarg :phase :accessor ws-phase)
   (wave :initform nil :initarg :wave :accessor ws-wave)
   (offset :initform 0.0 :initarg :offset :accessor ws-offset)))

(defmethod print-object ((d polyshape) stream)
  (format stream "#<polyshape: ~A: coeffs[~A]: ~A>"
		       (prettified-freq (ws-freq d) (ws-phase d))
		       (length (ws-wave d))
		       (prettified-array (ws-wave d))))

(def-optkey-fun make-polyshape ((frequency *clm-default-frequency*) (initial-phase 0.0) (coeffs nil) (partials '(1 1)) (kind mus-chebyshev-first-kind))
  (make-instance 'polyshape
		 :wave (or coeffs (partials->polynomial (or partials (list 1.0 1.0)) kind))
		 :freq (hz->radians frequency)
		 :phase initial-phase))

(defun polyshape (w &optional (index 1.0) (fm 0.0))
  (let ((val (polynomial (ws-wave w)
			 (* index (cos (ws-phase w)))))) ; was sin, 31-May-08
    (incf (ws-phase w) (+ (ws-freq w) fm))
    (when (or (> (ws-phase w) 100.0) (< (ws-phase w) -100.0))
      (setf (ws-phase w) (mod (ws-phase w) two-pi)))
    val))

(defmethod polyshape? ((g polyshape)) t)
(defmethod polyshape? ((g t)) nil)


(defclass polywave (polyshape)
  ((index :initform 1.0 :initarg :index :accessor ws-index)
   (top :initform 0 :initarg :top :accessor ws-top)
   (type :initform mus-chebyshev-first-kind :initarg :type :accessor ws-type)))

(defmethod print-object ((d polywave) stream)
  (format stream "#<polywave: ~A: coeffs[~A]: ~A>"
		       (prettified-freq (ws-freq d) (ws-phase d))
		       (length (ws-wave d))
		       (prettified-array (ws-wave d))))

(defmethod mus-scaler ((gen polywave)) (ws-index gen))
(defmethod (setf mus-scaler) (val (gen polywave)) (setf (ws-index gen) val) val)

(def-optkey-fun make-polywave ((frequency *clm-default-frequency*) (partials '(1 1)) (type mus-chebyshev-first-kind))
  (let ((top (loop for partial in partials by #'cddr maximize partial)))
    (make-instance 'polywave
		   :top (1+ top)
		   :type type
		   :wave (let ((coeffs (make-double-array (1+ top)))
			       (len (/ (length partials) 2)))
			   (loop for i from 0 by 1 below len do
			     (setf (aref coeffs (nth (* i 2) partials)) (double (nth (+ (* i 2) 1) partials))))
			   coeffs)
		   :freq (hz->radians frequency)
		   :phase 0.0)))

(defun mus-chebyshev-t-sum (x index n tn)
  (let* ((b1 0.0)
	 (b2 0.0)
	 (cx (* index (cos x)))
	 (x2 (* 2.0 cx))
	 (b (aref tn (- n 1))))
    (loop for i from (- n 2) downto 0 do
      (setf b2 b1)
      (setf b1 b)
      (setf b (- (+ (* b1 x2) (aref tn i)) b2)))
    (- b (* cx b1))))

(defun mus-chebyshev-u-sum (x index n un)
  (let* ((b1 0.0)
	 (b2 0.0)
	 (cx (* index (cos x)))
	 (x2 (* 2.0 cx))
	 (b (aref un (- n 1))))
    (loop for i from (- n 2) downto 1 do
      (setf b2 b1)
      (setf b1 b)
      (setf b (- (+ (* b1 x2) (aref un i)) b2)))
    (* b (sin x))))

(defun polywave (w &optional (fm 0.0))
  (let ((result (if (not (= (ws-type w) mus-chebyshev-second-kind))
		    (mus-chebyshev-t-sum (ws-phase w) (ws-index w) (ws-top w) (ws-wave w))
		  (mus-chebyshev-u-sum (ws-phase w) (ws-index w) (ws-top w) (ws-wave w)))))
    (incf (ws-phase w) (+ (ws-freq w) fm))
    (when (or (> (ws-phase w) 100.0) (< (ws-phase w) -100.0))
      (setf (ws-phase w) (mod (ws-phase w) two-pi)))
    result))

(defmethod polywave? ((g polywave)) t)
(defmethod polywave? ((g t)) nil)



(defun partial-amp (n partials)
  (loop for i on partials by #'cddr do
    (if (= n (car i)) (return (cadr i)))))

(defun partials->polynomial (partials &optional (kind mus-chebyshev-first-kind))
  (let* ((top (floor (highest-partial partials)))
	 (size (+ top 1))
	 (T0 (make-array size :element-type 'integer :initial-element 0))
	 (T1 (make-array size :element-type 'integer :initial-element 0))
	 (Tn (make-array size :element-type 'integer :initial-element 0))
	 (Cc1 (make-array size :element-type 'float :initial-element 0.0))
	 (amp 0.0))
    (if (= kind mus-chebyshev-first-kind)
	(setf (aref T0 0) 1)
      (setf (aref T0 0) 0))
    (setf (aref T1 1) 1)		;initialize Tn recursion (0 in T0 is Un)
    (loop for i from 1 to top do	;linear combination of polynomials weighted by harmonic amplitude
      (setf amp (or (partial-amp i partials) 0.0))
      (when (/= 0.0 amp)
	(if (= kind mus-chebyshev-first-kind)
	    (loop for k from 0 to i do (incf (aref Cc1 k) (* amp (aref T1 k))))
	  (loop for k from 1 to i do (incf (aref Cc1 (- k 1)) (* amp (aref T1 k))))))
      (when (/= i top)
	(loop for k from (+ i 1) downto 1 do
	  (setf (aref Tn k) (- (* 2 (aref T1 (- k 1))) (aref T0 k))))
	(setf (aref Tn 0) (- (aref T0 0)))
	(loop for k from (+ i 1) downto 0 do
	  (setf (aref T0 k) (aref T1 k))
	  (setf (aref T1 k) (aref Tn k)))))
    (let ((cc (make-double-array size)))
      (loop for i from 0 below size do (setf (aref cc i) (double (aref Cc1 i))))
      cc)))



;;; ncos

(defclass ncos ()
  ((n :initform nil :initarg :n :accessor ncosp-n)
   (scaler :initform nil :initarg :scaler :accessor ncosp-scaler)
   (phase :initform nil :initarg :phase :accessor ncosp-phase)
   (freq :initform nil :initarg :freq :accessor ncosp-freq)))

(defmethod print-object ((d ncos) stream)
  (format stream "#<ncos: ~A, n: ~A, scaler: ~A>"
	  (prettified-freq (ncosp-freq d) (ncosp-phase d))
	  (ncosp-n d) (prettified-float (ncosp-scaler d))))

(def-optkey-fun make-ncos ((frequency *clm-default-frequency*) (n 1))
  (let ((cs (make-instance 'ncos
			   :n n
			   :freq (hz->radians frequency)
			   :phase 0.0)))
    (if (zerop n) (warn "ncos with 0 n?"))
    (setf (ncosp-scaler cs) (/ 1.0 n))
    cs))

(defmethod ncos? ((g ncos)) t)
(defmethod ncos? ((g t)) nil)

(defun ncos (cs &optional (fm 0.0))
  (let* ((den (sin (* (ncosp-phase cs) 0.5)))
	 (val (if (= 0.0 den)
		  1.0
		(min 1.0 (* (ncosp-scaler cs)
			    (- (/ (sin (* (ncosp-phase cs)
					  (+ (ncosp-n cs) .5)))
				  (* 2.0 den))
			       0.5))))))
    (incf (ncosp-phase cs) (+ (ncosp-freq cs) fm))
    (if (> (ncosp-phase cs) two-pi) (decf (ncosp-phase cs) two-pi))
    (if (< (ncosp-phase cs) (- two-pi)) (incf (ncosp-phase cs) two-pi))
    val))

(defmethod mus-frequency ((gen ncos)) (radians->hz (ncosp-freq gen)))
(defmethod (setf mus-frequency) (val (gen ncos)) (setf (ncosp-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen ncos)) (ncosp-freq gen))
(defmethod (setf mus-increment) (val (gen ncos)) (setf (ncosp-freq gen) val) val)
(defmethod mus-phase ((gen ncos)) (mod (ncosp-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen ncos)) (setf (ncosp-phase gen) val) val)
(defmethod mus-length ((gen ncos)) (ncosp-n gen))
(defmethod (setf mus-length) (val (gen ncos)) (setf (ncosp-n gen) val) (setf (ncosp-scaler gen) (/ 1.0 val)) val)
(defmethod mus-scaler ((gen ncos)) (ncosp-scaler gen))
(defmethod (setf mus-scaler) (val (gen ncos)) (setf (ncosp-scaler gen) val))

(defmethod mus-run ((gen ncos) &optional (arg1 0.0) (arg2 0.0)) 
  (declare (ignore arg2)) 
  (ncos gen arg1))



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



(defclass nrxysin ()
  ((phase :initform nil :initarg :phase :accessor nrxy-phase)
   (freq :initform nil :initarg :freq :accessor nrxy-freq)
   (r :initform nil :initarg :r :accessor nrxy-r)
   (n :initform nil :initarg :n :accessor nrxy-n)
   (ratio :initform nil :initarg :ratio :accessor nrxy-ratio)))

(defmethod print-object ((d nrxysin) stream)
  (format stream "#<nrxysin: ~A, r: ~A, n: ~A, ratio: ~A>"
	  (prettified-freq (nrxy-freq d) (nrxy-phase d))
	  (prettified-float (nrxy-r d))
	  (prettified-float (nrxy-n d))
	  (prettified-float (nrxy-ratio d))))

(def-optkey-fun make-nrxysin ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (make-instance 'nrxysin
		 :freq (hz->radians frequency)
		 :phase 0.0
		 :r r :n n :ratio ratio))

(defmethod nrxysin? ((g nrxysin)) t)
(defmethod nrxysin? ((g t)) nil)

(defun nrxysin (gen &optional (fm 0.0))
  (let* ((x (nrxy-phase gen))
	 (y (* x (nrxy-ratio gen)))
	 (n (nrxy-n gen))
	 (r (nrxy-r gen))
	 (norm (/ (- (expt (abs r) n) 1) (- (abs r) 1))))
    (setf (nrxy-phase gen) (+ x (nrxy-freq gen) fm))
    (/ (- (sin x)
	  (* r (sin (- x y)))
	  (* (expt r (1+ n))
	     (- (sin (+ x (* (1+ n) y)))
		(* r (sin (+ x (* n y)))))))
       (* norm
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defmethod mus-frequency ((gen nrxysin)) (radians->hz (nrxy-freq gen)))
(defmethod (setf mus-frequency) (val (gen nrxysin)) (setf (nrxy-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen nrxysin)) (nrxy-freq gen))
(defmethod (setf mus-increment) (val (gen nrxysin)) (setf (nrxy-freq gen) val) val)
(defmethod mus-phase ((gen nrxysin)) (mod (nrxy-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen nrxysin)) (setf (nrxy-phase gen) val) val)
(defmethod mus-run ((gen nrxysin) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (nrxysin gen arg1))
(defmethod mus-scaler ((gen nrxysin)) (nrxy-r gen))
(defmethod (setf mus-scaler) (val (gen nrxysin)) (setf (nrxy-r gen) val) val)
(defmethod mus-length ((gen nrxysin)) (nrxy-n gen))
(defmethod mus-offset ((gen nrxysin)) (nrxy-ratio gen))


(defclass nrxycos ()
  ((phase :initform nil :initarg :phase :accessor nrxy-phase)
   (freq :initform nil :initarg :freq :accessor nrxy-freq)
   (r :initform nil :initarg :r :accessor nrxy-r)
   (n :initform nil :initarg :n :accessor nrxy-n)
   (ratio :initform nil :initarg :ratio :accessor nrxy-ratio)))

(defmethod print-object ((d nrxycos) stream)
  (format stream "#<nrxycos: ~A, r: ~A, n: ~A, ratio: ~A>"
	  (prettified-freq (nrxy-freq d) (nrxy-phase d))
	  (prettified-float (nrxy-r d))
	  (prettified-float (nrxy-n d))
	  (prettified-float (nrxy-ratio d))))

(def-optkey-fun make-nrxycos ((frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r .5))
  (make-instance 'nrxycos
		 :freq (hz->radians frequency)
		 :phase 0.0
		 :r r :n n :ratio ratio))

(defmethod nrxycos? ((g nrxycos)) t)
(defmethod nrxycos? ((g t)) nil)

(defun nrxycos (gen &optional (fm 0.0))
  (let* ((x (nrxy-phase gen))
	 (y (* x (nrxy-ratio gen)))
	 (n (nrxy-n gen))
	 (r (nrxy-r gen))
	 (norm (/ (- (expt (abs r) (+ n 1)) 1) (- (abs r) 1))))
    (setf (nrxy-phase gen) (+ x (nrxy-freq gen) fm))
    (/ (- (cos x)
	  (* r (cos (- x y)))
	  (* (expt r (1+ n))
	     (- (cos (+ x (* (1+ n) y)))
		(* r (cos (+ x (* n y)))))))
       (* norm 
	  (+ 1.0 (* r r) (* -2 r (cos y)))))))

(defmethod mus-frequency ((gen nrxycos)) (radians->hz (nrxy-freq gen)))
(defmethod (setf mus-frequency) (val (gen nrxycos)) (setf (nrxy-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen nrxycos)) (nrxy-freq gen))
(defmethod (setf mus-increment) (val (gen nrxycos)) (setf (nrxy-freq gen) val) val)
(defmethod mus-phase ((gen nrxycos)) (mod (nrxy-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen nrxycos)) (setf (nrxy-phase gen) val) val)
(defmethod mus-run ((gen nrxycos) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (nrxycos gen arg1))
(defmethod mus-scaler ((gen nrxycos)) (nrxy-r gen))
(defmethod (setf mus-scaler) (val (gen nrxycos)) (setf (nrxy-r gen) val) val)
(defmethod mus-length ((gen nrxycos)) (nrxy-n gen))
(defmethod mus-offset ((gen nrxycos)) (nrxy-ratio gen))



(defclass asymmetric-fm ()
  ((r :initform nil :initarg :r :accessor asymfm-r)
   (freq :initform nil :initarg :freq :accessor asymfm-freq)
   (ratio :initform nil :initarg :ratio :accessor asymfm-ratio)
   (phase :initform nil :initarg :phase :accessor asymfm-phase)
   (cosr :initform nil :initarg :cosr :accessor asymfm-cosr)
   (sinr :initform nil :initarg :sinr :accessor asymfm-sinr)))

(defmethod print-object ((d asymmetric-fm) stream)
  (format stream "#<asymmetric-fm: ~A, ratio: ~A, r: ~A, cosr: ~A, sinr: ~A>"
	  (prettified-freq (asymfm-freq d) (asymfm-phase d))
	  (prettified-float (asymfm-ratio d))
	  (prettified-float (asymfm-r d))
	  (prettified-float (asymfm-cosr d))
	  (prettified-float (asymfm-sinr d))))

(def-optkey-fun make-asymmetric-fm ((frequency *clm-default-frequency*) (initial-phase 0.0) (r 1.0) (ratio 1.0))
  (if (/= r 0.0)
      (make-instance 'asymmetric-fm
		     :r r
		     :freq (hz->radians frequency)
		     :ratio ratio
		     :phase initial-phase
		     :cosr (* .5 (- r (/ 1.0 r)))
		     :sinr (* .5 (+ r (/ 1.0 r))))))

(defmethod asymmetric-fm? ((g asymmetric-fm)) t)
(defmethod asymmetric-fm? ((g t)) nil)
			
(defun asymmetric-fm (af index &optional (fm 0.0))
  (let* ((th (asymfm-phase af))
	 (mth (* (asymfm-ratio af) th))
	 (cr (asymfm-cosr af))
	 (sr (asymfm-sinr af))
	 (result (* (exp (* index cr (+ 1.0 (cos mth)))) (cos (+ th (* sr index (sin mth)))))))
    (incf (asymfm-phase af) (+ (asymfm-freq af) fm))
    (when (or (> (asymfm-phase af) 100.0) (< (asymfm-phase af) -100.0))
      (setf (asymfm-phase af) (mod (asymfm-phase af) two-pi)))
    result))

(defmethod mus-frequency ((gen asymmetric-fm)) (radians->hz (asymfm-freq gen)))
(defmethod (setf mus-frequency) (val (gen asymmetric-fm)) (setf (asymfm-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen asymmetric-fm)) (asymfm-freq gen))
(defmethod (setf mus-increment) (val (gen asymmetric-fm)) (setf (asymfm-freq gen) val) val)
(defmethod mus-phase ((gen asymmetric-fm)) (mod (asymfm-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen asymmetric-fm)) (setf (asymfm-phase gen) val) val)
(defmethod mus-run ((gen asymmetric-fm) &optional (arg1 0.0) (arg2 0.0)) (asymmetric-fm gen arg1 arg2))
(defmethod mus-scaler ((gen asymmetric-fm)) (asymfm-r gen))
(defmethod (setf mus-scaler) (val (gen asymmetric-fm))
  (when (/= val 0.0)
    (setf (asymfm-r gen) val)
    (setf (asymfm-cosr gen) (* .5 (- val (/ 1.0 val))))
    (setf (asymfm-sinr gen) (* .5 (+ val (/ 1.0 val)))))
  val)
(defmethod mus-offset ((gen asymmetric-fm)) (asymfm-ratio gen))



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



;;; -------- move-sound (dlocsig) --------

(defclass move-sound ()
  ((outf :initform nil :initarg :outf :accessor dlocs-outf)
   (revf :initform nil :initarg :revf :accessor dlocs-revf)
   (data :initform nil :initarg :data :accessor dlocs-data)))

(defmethod move-sound? ((g move-sound)) t)
(defmethod move-sound? ((g t)) nil)

(defun make-move-sound (dlocs-list &optional output revout)
  (make-instance 'move-sound
		 :data dlocs-list
		 :outf (or output *output*)
		 :revf (or revout *reverb*)))

(defun move-sound (l i in-sig)
  (declare (ignore l i))
  (warn "Lisp interpreted move-sound is a no-op")
  in-sig)

(defmethod print-object ((d move-sound) stream)
  (format stream "#<move-sound: data: ~A>"
	  (dlocs-data d)))



;;; -------- FILE->SAMPLE --------

(defclass file->sample ()
  ((fil :initform nil :initarg :fil :accessor f2s-fil)
   ;; here and in file->frample the loc/chn fields and start/channel args are purely for
   ;;   backwards compatibility -- they provide a path between open-input (which calls
   ;;   make-file->sample), and readin (which may get its start/channel info from
   ;;   open-input in CL/CLM -- this is an obsolete way to do things, but many
   ;;   instruments still use it.  (see simple-rd-start in ug2.ins)
   (loc :initform 0 :initarg :loc :accessor f2s-loc)
   (chn :initform 0 :initarg :chn :accessor f2s-chn)
   (size :initform nil :initarg :size :accessor f2s-size)))   

(defmethod print-object ((d file->sample) stream)
  (format stream "#<file->sample: fil: ~A, chan: ~A, start: ~A>" (f2s-fil d) (f2s-chn d) (f2s-loc d)))

(def-optkey-fun make-file->sample (file (start 0) (channel 0) (size *clm-file-buffer-size*))
  (if file
      (make-instance 'file->sample :fil file :chn channel :loc (floor start) :size size)
    nil))

(defmethod file->sample? ((g file->sample)) t)
(defmethod file->sample? ((g t)) nil)
(defmethod mus-input? ((obj file->sample)) t)
(defmethod mus-input? ((obj t)) nil)
(defmethod mus-file-name ((gen file->sample)) (f2s-fil gen))

(defmethod mus-channel ((gen file->sample)) (f2s-chn gen))
(defmethod mus-location ((gen file->sample)) (f2s-loc gen))
(defmethod mus-length ((gen file->sample)) (sound-framples (f2s-fil gen)))
(defmethod mus-channels ((gen file->sample)) (mus-sound-chans (mus-file-name gen)))

(defun file->sample (obj samp &optional (chn 0))
  (declare (ignore obj samp chn))
  (warn "file->sample is a no-op outside 'run'"))

(defmethod mus-increment ((g file->sample)) 1)
(defmethod (setf mus-increment) (val (g file->sample)) val)



;;; -------- FILE->FRAMPLE --------

(defclass file->frample ()
  ((fil :initform nil :initarg :fil :accessor f2f-fil)
   (loc :initform 0 :initarg :loc :accessor f2f-loc)
   (chn :initform 0 :initarg :chn :accessor f2f-chn)
   (size :initform nil :initarg :size :accessor f2f-size)))

(defmethod print-object ((d file->frample) stream)
  (format stream "#<file->frample: fil: ~A>" (f2f-fil d)))

(def-optkey-fun make-file->frample (file (start 0) (channel 0) (size *clm-file-buffer-size*))
  (if file
      (make-instance 'file->frample :fil file :chn channel :loc start :size size)
    nil))

(defmethod file->frample? ((g file->frample)) t)
(defmethod file->frample? ((g t)) nil)
(defmethod mus-input? ((obj file->frample)) t)
(defmethod mus-file-name ((gen file->frample)) (f2f-fil gen))

(defmethod mus-channel ((gen file->frample)) (f2f-chn gen))
(defmethod mus-location ((gen file->frample)) (f2f-loc gen))
(defmethod mus-length ((gen file->frample)) (sound-framples (f2s-fil gen)))
(defmethod mus-channels ((gen file->frample)) (mus-sound-chans (mus-file-name gen)))

(defun file->frample (obj samp frm)
  (declare (ignore obj samp frm))
  (warn "file->frample is a no-op outside 'run'"))

(defmethod mus-increment ((g file->frample)) 1)
(defmethod (setf mus-increment) (val (g file->frample)) val)


;;; -------- SAMPLE->FILE --------

(defclass sample->file ()
  ((loc :initform 0 :initarg :start :accessor s2f-loc)
   (chns :initform 1 :initarg :chans :accessor s2f-chns)
   (file :initform nil :initarg :file :accessor s2f-file)
   (frm :initform nil :initarg :format :accessor s2f-frm)
   (typ :initform nil :initarg :type :accessor s2f-typ)
   (com :initform nil :initarg :comment :accessor s2f-com)
   (safety :initform 0 :initarg :safety :accessor s2f-safety)))

(defmethod print-object ((d sample->file) stream)
  (format stream "#<sample->file: file: ~A, chans: ~A, loc: ~A>" (s2f-file d) (s2f-chns d) (s2f-loc d)))

(defun make-sample->file (name &optional (chans 1) (format *clm-data-format*) (type *clm-header-type*) (comment nil))
  (make-instance 'sample->file
		 :file name
		 :chans chans
		 :format format
		 :type type
		 :safety 0
		 :comment comment))

(defmethod sample->file? ((g sample->file)) t)
(defmethod sample->file? ((g t)) nil)

(defmethod mus-output? ((obj sample->file)) t)
(defmethod mus-file-name ((gen sample->file)) (s2f-file gen))
(defmethod mus-channels ((gen sample->file)) (s2f-chns gen))
(defmethod mus-location ((gen sample->file)) (s2f-loc gen))

(defmethod mus-channel ((gen sample->file)) 0)

(defun sample->file (obj samp chan val)
  (declare (ignore obj samp chan val))
  (warn "Lisp interpreted sample->file is a no-op"))

(defmethod mus-increment ((g sample->file)) 1)
(defmethod (setf mus-increment) (val (g sample->file)) val)


(defmethod mus-safety ((gen sample->file))
  (s2f-safety gen))

(defmethod (setf mus-safety) (val (gen sample->file))
  (setf (s2f-safety gen) val)
  val)



;;; -------- FRAMPLE->FILE --------

(defclass frample->file ()
  ((loc :initform 0 :initarg :start :accessor f2f-loc)
   (chns :initform 1 :initarg :chans :accessor f2f-chns)
   (file :initform nil :initarg :file :accessor f2f-file)
   (frm :initform nil :initarg :format :accessor f2f-frm)
   (typ :initform nil :initarg :type :accessor f2f-typ)
   (com :initform nil :initarg :comment :accessor f2f-com)
   (safety :initform 0 :initarg :safety :accessor f2f-safety)))

(defmethod print-object ((d frample->file) stream)
  (format stream "#<frample->file: file: ~A, chans: ~A, loc: ~A>" (f2f-file d) (f2f-chns d) (f2f-loc d)))

(defun make-frample->file (name &optional (chans 1) (format *clm-data-format*) (type *clm-header-type*) (comment nil))
  (make-instance 'frample->file
		 :file name
		 :chans chans
		 :format format
		 :type type
		 :safety 0
		 :comment comment))

(defmethod frample->file? ((g frample->file)) t)
(defmethod frample->file? ((g t)) nil)

(defmethod mus-output? ((obj frample->file)) t)
(defmethod mus-file-name ((gen frample->file)) (f2f-file gen))
(defmethod mus-channels ((gen frample->file)) (f2f-chns gen))
(defmethod mus-location ((gen frample->file)) (f2f-loc gen))

(defmethod mus-channel ((gen frample->file)) 0)

(defun frample->file (obj samp val)
  (declare (ignore obj samp val))
  (warn "Lisp interpreted frample->file is a no-op"))

(defmethod mus-increment ((g frample->file)) 1)
(defmethod (setf mus-increment) (val (g frample->file)) val)

(defmethod mus-safety ((gen frample->file))
  (f2f-safety gen))

(defmethod (setf mus-safety) (val (gen frample->file))
  (setf (f2f-safety gen) val)
  val)



;;; -------- READIN --------

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



;;; Sampling rate conversion (src)
;;; based on kindly advice of Perry Cook -- see his sweep-srate.c. Changed 13-Jan-98 to match prc's code more closely.

(defmethod mus-file-name ((str t)) str) ; might be string/pathname as file, or nil etc

(defclass src ()
  ((rd :initform nil :initarg :rd :accessor sr-rd)
   (x :initform 0.0 :initarg :x :accessor sr-x)
   (incr :initform 1.0 :initarg :incr :accessor sr-incr)
   (data :initform nil :initarg :data :accessor sr-data)
   (width :initform 5 :initarg :width :accessor sr-width)
   (sinc :initform nil :initarg :sinc :accessor sr-sinc)
   (len :initform nil :initarg :len :accessor sr-len)))

(defmethod print-object ((d src) stream)
  (format stream "#<src: x: ~A, incr: ~A, width: ~A, len: ~A, rd: ~A, data: ~A, sinc: ~A>"
	  (prettified-float (sr-x d))
	  (prettified-float (sr-incr d))
	  (sr-width d) (sr-len d) (sr-rd d)
	  (prettified-array (sr-data d))
	  (prettified-array (sr-sinc d))))

(defmethod src? ((g src)) t)
(defmethod src? ((g t)) nil)

(defvar sinc-density 20)
(defvar *clm-src-width* 5)
(defvar previous-sinc-table nil)
(defvar previous-sinc-table-size -1)

(defun fill-sinc-table (size)
  (if (= size previous-sinc-table-size) 
      previous-sinc-table
    (let* ((sinc-table (make-double-array (1+ size)))
	   (win-freq (/ pi size))
	   (sinc-freq (/ pi sinc-density)))
      (setf (aref sinc-table 0) (double 1.0))
      (setf (aref sinc-table size) (double 0.0))
      (loop for i from 1 below size and sf from sinc-freq by sinc-freq and wf from win-freq by win-freq do
	(setf (aref sinc-table i) 
	      (double (/ (* (+ 0.5 (* 0.5 (cos wf))) (sin sf)) sf))))
      (setf previous-sinc-table sinc-table)
      (setf previous-sinc-table-size size)
      sinc-table)))

(def-optkey-fun make-src (input (srate 1.0) (width *clm-src-width*))
  ;; input can be a filename, or a mus-input gen, or a function etc
  (let* ((wid (max width (* 2 (ceiling srate))))
	 (size (* wid sinc-density)))
    (make-instance 'src
		   :rd (if (mus-input? input)
			   input
			 (make-file->sample (mus-file-name input)))
		   :x 0.0
		   :incr srate
		   :width wid
		   :sinc (fill-sinc-table size)
		   :len size
		   :data (make-double-array (1+ (* wid 2)) :initial-element 0.0))))

(defun src (s &optional (sr-change 0.0) input-function)
  ;; get data window lined up right, convolve with "warped" sinc
  (let* ((sum 0.0)
	 (loc 0)
	 (srx (+ (sr-incr s) sr-change))
	 (lim (* 2 (sr-width s)))
	 (fsx (floor (sr-x s))))
    (when (> fsx 0)
      (loop for i from fsx below lim do
	(setf (aref (sr-data s) loc) (aref (sr-data s) i))
	(incf loc))
      (if (file->sample? (sr-rd s))
	  (setf (sr-rd s) (make-readin (mus-file-name (sr-rd s)))))
      (if (readin? (sr-rd s))
	  (loop for i from loc below lim do
	    (setf (aref (sr-data s) i) (double (readin (sr-rd s)))))
	(if (or input-function (sr-rd s))
	    (loop for i from loc below lim do
	      (setf (aref (sr-data s) i) (double (funcall (or input-function (sr-rd s)) (if (plusp srx) 1 -1)))))
	  (error "no input source for src?")))
      (decf (sr-x s) fsx))
    ;; now dot-product with (possibly warped) sinc
    (if (minusp srx) (setf srx (- srx)))
    (let* ((factor (if (<= srx 1.0) 1.0 (/ 1.0 srx)))
	   (zf (* factor sinc-density)))
      (loop for i from 0 below lim and x from (* zf (- 1.0 (sr-x s) (sr-width s))) by zf do
	(multiple-value-bind (k frac) (floor (abs x))
	  (if (< k (sr-len s))
	      (incf sum (* (aref (sr-data s) i)
			   (+ (aref (sr-sinc s) k)
			      (* frac (- (aref (sr-sinc s) (1+ k)) (aref (sr-sinc s) k)))))))))
      (incf (sr-x s) srx)
      (* factor sum))))

(defmethod mus-increment ((gen src)) (sr-incr gen))
(defmethod (setf mus-increment) (val (gen src)) (setf (sr-incr gen) val))
(defmethod mus-channel ((gen src)) (if (sr-rd gen) (mus-channel (sr-rd gen))))
(defmethod mus-location ((gen src)) (if (sr-rd gen) (mus-location (sr-rd gen))))
(defmethod (setf mus-location) (val (gen src)) (if (sr-rd gen) (setf (mus-location (sr-rd gen)) (floor val)) val))
(defmethod mus-run ((gen src) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (src gen arg1))



(defclass convolve ()
  ((filtr :initform nil :initarg :filtr :accessor conv-filtr)
   (fltlen :initform nil :initarg :fltlen :accessor conv-fltlen)
   (size :initform nil :initarg :size :accessor conv-size)
   (hop :initform nil :initarg :hop :accessor conv-hop)
   (rd :initform nil :initarg :rd :accessor conv-rd)
   (b :initform nil :initarg :b :accessor conv-b)
   (datar :initform nil :initarg :datar :accessor conv-datar)
   (datai :initform nil :initarg :datai :accessor conv-datai)))

(defmethod print-object ((d convolve) stream)
  (format stream "#<convolve:  size: ~A, hop: ~A, rd: ~A, b: ~A, datar: ~A, datai: ~A, filtr: ~A, fltlen: ~A>"
	  (conv-size d) (conv-hop d)
	  (conv-rd d) (conv-b d)
	  (prettified-array (conv-datar d))
	  (prettified-array (conv-datai d))
	  (prettified-array (conv-filtr d))
	  (prettified-array (conv-fltlen d))))

(defmethod convolve? ((g convolve)) t)
(defmethod convolve? ((g t)) nil)

(def-optkey-fun make-convolve (input filter fft-size filter-size filter-scaler)
  ;; changed 21-Mar-01 to take filter-size arg into account
  ;; added filter-scaler arg and (mus-channel filter) 31-Jan-07
  (let* ((impulse (if (mus-input? filter)
		      (let ((arr (make-double-array (or filter-size (mus-length filter)))))
			(file->array (mus-file-name filter) (mus-channel filter) (mus-location filter) (or filter-size (mus-length filter)) arr)
			arr)
		    (if (stringp filter)
			(let* ((samps (sound-framples filter))
			       (arr (make-double-array (or filter-size samps))))
			  (file->array filter 0 0 (or filter-size samps) arr)
			  arr)
		      (if (arrayp filter)
			  filter
			(warn "make-convolve can't handle filter arg: ~A" filter)))))
	 (fft1 (floor (max (expt 2 (ceiling (log (length impulse) 2))) (or fft-size 16)))) ; was 128 -- actually why anything other than 1 here?
	 (fft2 (floor (* fft1 2))))
    (if (and impulse
	     (not (find-if #'(lambda (a) (not (= a 0.0))) impulse)))
	(warn ";make-convolve filter contains only 0.0"))
    (if (and impulse
	     (numberp filter-scaler))
	(let ((lim (array-dimension impulse 0)))
	  (loop for i from 0 below lim do
	    (setf (aref impulse i) (* filter-scaler (aref impulse i))))))
    (make-instance 'convolve
		   :filtr impulse
		   :fltlen (length impulse)
		   :size fft2
		   :hop fft1
		   :datar nil
		   :datai nil
		   :b nil
		   :rd (if (mus-input? input)
			   input
			 (make-file->sample (mus-file-name input))))))

(defun convolve (c &optional input-function)
  (declare (ignore c input-function))
  (warn "convolve only works inside the run macro"))

(defmethod mus-length ((gen convolve)) (conv-size gen))
(defmethod mus-data ((gen convolve)) nil)
(defmethod mus-xcoeffs ((gen convolve)) (conv-filtr gen))
(defmethod mus-run ((gen convolve) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (convolve gen))

(defun convolve-one-channel (file1 file2 file1-chan file2-chan file1-len file2-len fftlen data1 data2)
  (file->array file1 file1-chan 0 file1-len data1)
  (file->array file2 file2-chan 0 file2-len data2)
  (convolution data1 data2 fftlen)
  data1)
  
(defun convolve-files (file1 file2 &optional (maxamp 1.0) (output-file "tmp.snd"))
  (let* ((file1-len (sound-framples file1))
	 (file2-len (sound-framples file2))
	 (file1-chans (sound-chans file1))
	 (file2-chans (sound-chans file2))
	 (output-chans (max file1-chans file2-chans))
	 (fftlen (expt 2 (ceiling (log (+ file1-len file2-len) 2))))
	 (outlen (+ file1-len file2-len 1))
	 (data1 (make-double-array fftlen :initial-element 0.0))
	 (data2 (make-double-array fftlen :initial-element 0.0)))
    (if (= output-chans 1)
	(let* ((outdat (convolve-one-channel file1 file2 0 0 file1-len file2-len fftlen data1 data2))
	       (maxval (loop for i from 0 below outlen maximize (abs (aref outdat i)))))
	    (if (/= maxval 0.0)
		(let ((maxv (/ maxamp maxval)))
		  (loop for i from 0 below outlen do
		    (setf (aref outdat i) (* (aref outdat i) maxv)))))
	    (array->file output-file outdat outlen (sound-srate file1) 1))
      (let* ((totallen (* outlen output-chans))
	     (outdat (make-double-array totallen))
	     (c1 0)
	     (c2 0))
	(loop for i from 0 below output-chans do
	  (let ((curdat (convolve-one-channel file1 file2 c1 c2 file1-len file2-len fftlen data1 data2)))
	    (loop for j from i below totallen by output-chans and k from 0 by 1 do
	      (setf (aref outdat j) (aref curdat k)))
	    (incf c1)
	    (if (>= c1 file1-chans) (setf c1 0))
	    (incf c2)
	    (if (>= c2 file2-chans) (setf c2 0))
	    (loop for i from 0 below fftlen do
	      (setf (aref data1 i) (double 0.0))
	      (setf (aref data2 i) (double 0.0)))))
	(let ((maxval (loop for i from 0 below totallen maximize (abs (aref outdat i)))))
	  (if (/= maxval 0.0)
	      (let ((maxv (/ maxamp maxval)))
		(loop for i from 0 below totallen do
		  (setf (aref outdat i) (* (aref outdat i) maxv)))))
	  (array->file output-file outdat totallen (sound-srate file1) output-chans))))))


;;; Granulate was originally called SpeedFile (in Mixer.Sai)
;;;   Another version of SpeedFile alternated between forward and backward segments.

(defclass granulate ()
  ((rd :initform nil :initarg :rd :accessor spd-rd)
   (len :initform nil :initarg :len :accessor spd-len)
   (rmp :initform nil :initarg :rmp :accessor spd-rmp)
   (amp :initform nil :initarg :amp :accessor spd-amp)
   (input-hop :initform nil :initarg :input-hop :accessor spd-input-hop)
   (output-hop :initform nil :initarg :output-hop :accessor spd-output-hop)
   (cur-in :initform nil :initarg :cur-in :accessor spd-cur-in)
   (cur-out :initform 0 :initarg :cur-out :accessor spd-cur-out)
   (data :initform nil :initarg :b :accessor spd-data)
   (s20 :initform nil :initarg :s20 :accessor spd-s20)
   (s50 :initform nil :initarg :s50 :accessor spd-s50)
   (ctr :initform 0 :initarg :ctr :accessor spd-ctr)
   (block-len :initform nil :initarg :block-len :accessor spd-block-len)
   (in-data :initform nil :initarg :in-data :accessor spd-in-data)
   (in-data-start :initform 0 :accessor spd-in-data-start)
   (in-data-len :initform 0 :accessor spd-in-data-len)
   (grain :initform nil :accessor spd-grain)
   (edit :initform nil :initarg :edit :accessor spd-edit)

   ;; rest of fields for new run macro's benefit
   (expansion :initform nil :initarg :expansion :accessor spd-expansion)
   (length :initform nil :initarg :length :accessor spd-length)
   (hop :initform nil :initarg :hop :accessor spd-hop)
   (ramp :initform nil :initarg :ramp :accessor spd-ramp)
   (jitter :initform nil :initarg :jitter :accessor spd-jitter)
   (max-size :initform nil :initarg :max-size :accessor spd-max-size)))

(defmethod print-object ((d granulate) stream)
  (format stream "#<granulate: amp: ~A, len: ~A (~A), rmp: ~A, input-hop: ~A, output-hop: ~A, cur-in: ~A (~A), cur-out: ~A, s20: ~A, s50: ~A, ctr: ~A, rd: ~A, data: ~A, in-data: ~A>"
	  (prettified-float (spd-amp d))
	  (spd-len d) (spd-block-len d) (spd-rmp d) (spd-input-hop d) (spd-output-hop d)
	  (spd-cur-in d) (spd-in-data-start d) (spd-cur-out d) (spd-s20 d) (spd-s50 d) (spd-ctr d)
	  (spd-rd d) 
	  (prettified-array (spd-data d))
	  (prettified-array (spd-in-data d))))

(def-optkey-fun make-granulate (input
				(expansion 1.0)
				(length .15)
				(scaler .6)
				(hop .05)
				(ramp .4)     ;amount of segment spent sloping up or down (envelope)
				(jitter 1.0)
				max-size
				edit)
   (let ((val (make-instance 'granulate
			    :cur-out 0
			    :rd (if (mus-input? input)
				    input
				  (make-file->sample (mus-file-name input)))
			    :cur-in 0 ;start
			    :len (ceiling (* length *srate*))
			    :rmp (floor (* ramp length *srate*))
			    :amp scaler
			    :input-hop (floor (/ (* hop *srate*)
					      (if (numberp expansion) expansion
						(if (null expansion) 1.0
						  (let* ((argtype (type-of expansion)))
						    (warn "the expansion argument to make-granulate: ~A, should be of type number, not ~A"
							  expansion argtype)
						    1.0)))))
			    :output-hop (floor (* hop *srate*))
			    :s20 (floor (* jitter (/ *srate* 20)))
			    :s50 (floor (* jitter (/ *srate* 50)))
			    :edit edit
			    :ctr 0
			    :expansion expansion
			    :length length
			    :hop hop
			    :ramp ramp
			    :jitter jitter
			    :max-size max-size)))
     (let ((block-length (ceiling (or max-size (* *srate* (+ hop length))))))
       (setf (spd-block-len val) block-length)
       (if (<= block-length 0) (warn "granulate block has ~D elements?" block-length))
       (if (readin? input)
         (setf (spd-cur-in val) (mus-location input))
         (progn
	  (setf (spd-in-data-len val) (+ block-length (spd-s20 val) 1))
	  (if (spd-edit val) (setf (spd-grain val) (make-double-array (spd-in-data-len val))))
	  (setf (spd-in-data-start val) (spd-in-data-len val))))
       val)))

(defmethod granulate? ((g granulate)) t)
(defmethod granulate? ((g t)) nil)

(defun granulate (e &optional input-function)
  (if (not (spd-data e)) (setf (spd-data e) (make-double-array (spd-block-len e))))
  (let ((cur-val (aref (spd-data e) (floor (spd-ctr e)))))
    (incf (spd-ctr e))

    ;; do we need the next grain?
    (when (>= (spd-ctr e) (spd-cur-out e))
      (let* ((start (floor (spd-cur-out e)))
	     (end (max 0 (- (spd-len e) start))))
	(if (> end 0)
	    (loop for i from 0 below end and j from start do
	      (setf (aref (spd-data e) i) (aref (spd-data e) j))))
	(loop for i from end below (spd-block-len e) do
	  (setf (aref (spd-data e) i) (double 0.0))))
      
      ;; we need unidirectional input from the input-function if it's not a file reader
      ;; so we save partial results in spd-in-data; this input has to be basically
      ;; regular (i.e. follow input-hop) with local (non-accumulating) jitter
      (when (not (readin? (spd-rd e)))
	(if (not (spd-in-data e)) (setf (spd-in-data e) (make-double-array (spd-in-data-len e))))
	(let ((start (spd-in-data-start e))
	      (len (spd-in-data-len e)))
	  (when (> start len)		; in hop is larger than buffer size
	    (let ((extra (- start len)))
	      (loop for i from 0 below extra do (funcall (or input-function (spd-rd e)) 1))
	      (setf start len)))
	  (if (< start len)
	      (loop for i from 0 and k from start below len do
		(setf (aref (spd-in-data e) i) (aref (spd-in-data e) k))))
	  (loop for i from (- len start) below len do
	    (setf (aref (spd-in-data e) i) (double (funcall (or input-function (spd-rd e)) 1))))
	  (setf (spd-in-data-start e) (spd-input-hop e))))

      (if (spd-edit e)
	  (loop for i from 0 below (spd-in-data-len e) do (setf (aref (spd-grain e) i) (double 0.0))))

      (let ((data (if (spd-edit e) (spd-grain e) (spd-data e))))
	;; add in enveloped grain
	(let* ((amp 0.0)
	       (incr (/ (spd-amp e) (spd-rmp e)))
	       (steady-end (- (spd-len e) (spd-rmp e))))
	  (if (readin? (spd-rd e))
	      (loop for i from 0 below (spd-len e) do
		(incf (aref data i) (double (* amp (readin (spd-rd e)))))
		(if (< i (spd-rmp e)) (incf amp incr) (if (> i steady-end) (decf amp incr))))
	    (let ((curstart (floor (random (spd-s20 e)))))
	      (loop for i from 0 below (spd-len e) and j from curstart do
		(incf (aref data i) (double (* amp (aref (spd-in-data e) j))))
		(if (< i (spd-rmp e)) (incf amp incr) (if (> i steady-end) (decf amp incr))))))))

      (if (spd-edit e)
	  (let ((new-len (min (funcall (spd-edit e) e) (spd-in-data-len e))))
	    (if (<= new-len 0)
		(setf new-len (spd-len e)))
	    (loop for i from 0 below new-len do
	      (incf (aref (spd-data e) i) (aref (spd-grain e) i)))))

      ;; set up counters to trigger next grain
      (decf (spd-ctr e) (spd-cur-out e))
      (setf (spd-cur-out e) (max 0 (+ (spd-output-hop e) (- (random (spd-s50 e)) (floor (spd-s50 e) 2)))))
      (when (readin? (spd-rd e))
	(setf (mus-location (spd-rd e)) (+ (spd-cur-in e) (random (spd-s20 e))))
	(incf (spd-cur-in e) (spd-input-hop e))))
    cur-val))

(defmethod mus-frequency ((gen granulate)) (double (/ (spd-output-hop gen) *srate*)))
(defmethod (setf mus-frequency) (val (gen granulate)) (setf (spd-output-hop gen) (round (* *srate* val))) val)
(defmethod mus-ramp ((gen granulate)) (spd-rmp gen))
(defmethod (setf mus-ramp) (val (gen granulate)) (setf (spd-rmp gen) val))
(defmethod mus-hop ((gen granulate)) (spd-output-hop gen))
(defmethod (setf mus-hop) (val (gen granulate)) (setf (spd-output-hop gen) val))
(defmethod mus-scaler ((gen granulate)) (spd-amp gen))
(defmethod (setf mus-scaler) (val (gen granulate)) (setf (spd-amp gen) val))
(defmethod mus-increment ((gen granulate)) (double (/ (spd-output-hop gen) (spd-input-hop gen))))
(defmethod (setf mus-increment) (val (gen granulate)) (setf (spd-input-hop gen) (floor (/ (spd-output-hop gen) val))) val)
(defmethod mus-length ((gen granulate)) (spd-len gen))
(defmethod (setf mus-length) (val (gen granulate)) (setf (spd-len gen) val))
(defmethod mus-run ((gen granulate) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (granulate gen))
(defmethod mus-data ((gen granulate)) (spd-grain gen))


(defclass phase-vocoder ()
  ((input :initform nil :initarg :input :accessor pv-input)
   (output :initform nil :initarg :output :accessor pv-output)
   (interp :initform nil :initarg :interp :accessor pv-interp)
   (filptr :initform 0 :initarg :filptr :accessor pv-filptr)
   (N :initform 512 :initarg :N :accessor pv-N)
   (win :initform nil :initarg :window :accessor pv-window)
   (in-data :initform nil :accessor pv-in-data)
   (D :initform nil :initarg :D :accessor pv-D)
   (amp-increments :initform nil :initarg :amp-increments :accessor pv-amp-increments)
   (amps :initform nil :initarg :amps :accessor pv-amps)
   (freqs :initform nil :initarg :freqs :accessor pv-freqs)
   (phases :initform nil :initarg :phases :accessor pv-phases)
   (phaseinc :initform nil :initarg :phaseinc :accessor pv-phase-increments)
   (lastphase :initform nil :initarg :lastphase :accessor pv-previous-phases)
   (analyze :initform nil :initarg :analyze :accessor pv-analyze)
   (synthesize :initform nil :initarg :synthesize :accessor pv-synthesize)
   (edit :initform nil :initarg :edit :accessor pv-edit)
   (pitch :initform 1.0 :initarg :pitch :accessor pv-pitch)
   (overlap :initform nil :initarg :overlap :accessor pv-overlap)
   ))

(defmethod print-object ((d phase-vocoder) stream)
  (format stream "#<phase-vocoder: N: ~A, D: ~A, interp: ~A, output: ~A>"
	  (pv-N d) (pv-D d) (pv-interp d) (pv-output d)))

(def-optkey-fun make-phase-vocoder (input
				    (fft-size 512)
				    (overlap 4)
				    (interp 256)
				    (pitch 1.0)
				    (analyze nil)
				    (edit nil)
				    (synthesize nil))
  (let ((N2 (floor fft-size 2))
	(D (/ fft-size overlap)))
    (make-instance 'phase-vocoder
		   :N fft-size
		   :interp interp
		   :D D
		   :pitch pitch
		   :output interp
		   :overlap overlap
		   :filptr 0
		   :window (let ((win (make-fft-window hamming-window fft-size))
				 (scl (/ 2.0 (* 0.54 fft-size))))
			     (dotimes (i fft-size)
			       (setf (aref win i) (* (aref win i) scl)))
			     win)
		   :amp-increments (make-double-array fft-size)
		   :freqs (make-double-array fft-size)
		   :amps (make-double-array N2)
		   :phases (make-double-array N2)
		   :lastphase (make-double-array N2)
		   :phaseinc (make-double-array N2)
		   :input (if (mus-input? input)
			      input
			    (make-file->sample (mus-file-name input)))
		   :analyze analyze
		   :edit edit
		   :synthesize synthesize)))

(defmethod phase-vocoder? ((g phase-vocoder)) t)
(defmethod phase-vocoder? ((g t)) nil)

(defun phase-vocoder (pv &optional input)
  (let ((N2 (floor (pv-N pv) 2)))
    (when (>= (pv-output pv) (pv-interp pv))
      ;; get next amp/phase data block
      (let* ((N (pv-N pv))
	     (D (pv-D pv))
	     (amps (pv-amp-increments pv))
	     (freqs (pv-freqs pv))
	     (filptr (pv-filptr pv)))

	(if (or (not (pv-analyze pv))
		(funcall (pv-analyze pv) pv input))
	    ;; if no analysis func, do:
	    (progn
	      (dotimes (i N) (setf (aref freqs i) (double 0.0)))
	      (setf (pv-output pv) 0)
	      (if (not (pv-in-data pv))
		  (progn
		    (setf (pv-in-data pv) (make-double-array N))
		    (dotimes (i N) (setf (aref (pv-in-data pv) i) (double (funcall (or input (pv-input pv)) 1)))))
		(let ((indat (pv-in-data pv)))
		  ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		  (do ((i 0 (1+ i))
		       (j D (1+ j)))
		      ((= j N))
		    (setf (aref indat i) (double (aref indat j))))
		  (do ((i (- N D) (1+ i)))
		      ((= i N))
		    (setf (aref indat i) (double (funcall (or input (pv-input pv)) 1))))))
	      (let ((buf (mod filptr N)))
		(do ((k 0 (1+ k)))
		    ((= k N))
		  (setf (aref amps buf) (double (* (aref (pv-window pv) k) (aref (pv-in-data pv) k))))
		  (incf buf)
		  (if (= buf N) (setf buf 0))))
	      (incf (pv-filptr pv) D)
	      (fft amps freqs N 1)
	      (rectangular->polar amps freqs)))
	
	(if (or (not (pv-edit pv))
		(funcall (pv-edit pv) pv))
	    (progn
	      ;; if no editing func:
	      (do ((k 0 (1+ k))
		   (pscl (/ 1.0 D))
		   (kscl (/ (* 2.0 pi) N)))
		  ((= k (floor N 2)))
		(let ((phasediff (- (aref freqs k) (aref (pv-previous-phases pv) k))))
		  (setf (aref (pv-previous-phases pv) k) (double (aref freqs k)))
		  (if (> phasediff pi) (do () ((<= phasediff pi)) (setf phasediff (- phasediff (* 2.0 pi)))))
		  (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (setf phasediff (+ phasediff (* 2.0 pi)))))
		  (setf (aref freqs k) (double (* (pv-pitch pv) (+ (* pscl phasediff) (* k kscl)))))))))

	(let ((scl (/ 1.0 (pv-interp pv))))
	  (dotimes (i N2)
	    (setf (aref amps i) (double (* scl (- (aref amps i) (aref (pv-amps pv) i)))))
	    (setf (aref freqs i) (double (* scl (- (aref freqs i) (aref (pv-phase-increments pv) i)))))))))

    (incf (pv-output pv))
    (if (pv-synthesize pv)
	(funcall (pv-synthesize pv) pv)
      ;; if no synthesis func:
      ;; synthesize next sample
      (progn
	(dotimes (i N2)
	  (incf (aref (pv-amps pv) i) (double (aref (pv-amp-increments pv) i)))
	  (incf (aref (pv-phase-increments pv) i) (double (aref (pv-freqs pv) i)))
	  (incf (aref (pv-phases pv) i) (double (aref (pv-phase-increments pv) i))))
	(sine-bank (pv-amps pv) (pv-phases pv))))))

(defmethod mus-hop ((gen phase-vocoder)) (pv-D gen))
(defmethod (setf mus-hop) (val (gen phase-vocoder)) (setf (pv-D gen) val))
(defmethod mus-length ((gen phase-vocoder)) (pv-N gen))
(defmethod mus-increment ((gen phase-vocoder)) (pv-interp gen))
(defmethod (setf mus-increment) (val (gen phase-vocoder)) (setf (pv-interp gen) val))
;;;(defmethod mus-data ((gen phase-vocoder)) (pv-in-data gen))
(defmethod mus-frequency ((gen phase-vocoder)) (pv-pitch gen))
(defmethod (setf mus-frequency) (val (gen phase-vocoder)) (setf (pv-pitch gen) val))
(defmethod mus-run ((gen phase-vocoder) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (phase-vocoder gen))



(defun mus-apply (&optional args) (apply #'mus-run args))

;;; single sideband suppressed carrier amplitude modulation

(defclass ssb-am ()
  ((up :initform nil :initarg :up :accessor ssb-am-up)
   (sin-osc :initform nil :initarg :sin-osc :accessor ssb-am-sin-osc)
   (cos-osc :initform nil :initarg :cos-osc :accessor ssb-am-cos-osc)
   (dly :initform nil :initarg :dly :accessor ssb-am-dly)
   (hilbert :initform nil :initarg :hilbert :accessor ssb-am-hilbert)))

(defmethod print-object ((d ssb-am) stream)
  (format stream "#<ssb-am: ~A, order: ~A>"
	  (prettified-freq (oscil-freq (ssb-am-sin-osc d)) (oscil-phase (ssb-am-sin-osc d)))
	  (mus-order (ssb-am-dly d))))

(defun make-hilbert-transform (len)
  (let* ((arrlen (* 2 len))
	 (arr (make-double-float-array arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- 1.0 (cos (* pi i)))))
	(if (= i 0)
	    (setf (aref arr k) (double 0.0))
	    (setf (aref arr k) (double (* (/ num denom) 
					  (+ .54 (* .46 (cos (/ (* i pi) len))))))))))
    (make-fir-filter arrlen arr)))

(def-optkey-fun make-ssb-am ((frequency *clm-default-frequency*) (order 40))
  (make-instance 'ssb-am
		 :up (> frequency 0.0)
		 :sin-osc (make-oscil (abs frequency))
		 :cos-osc (make-oscil (abs frequency) (* 0.5 pi))
		 :dly (make-delay order)
		 :hilbert (make-hilbert-transform order)))

(defmethod ssb-am? ((g ssb-am)) t)
(defmethod ssb-am? ((g t)) nil)

(defun ssb-am (gen &optional (insig 0.0) (fm 0.0))
  (let ((ccos (oscil (ssb-am-cos-osc gen) fm))
	(csin (oscil (ssb-am-sin-osc gen) fm))
	(yh (fir-filter (ssb-am-hilbert gen) insig))
	(yd (delay (ssb-am-dly gen) insig)))
    (if (ssb-am-up gen)
	(- (* ccos yd) ; shift up
	   (* csin yh))
      (+ (* ccos yd) ; shift down
	 (* csin yh)))))

(defmethod mus-run ((gen ssb-am) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (ssb-am gen arg1))
(defmethod mus-frequency ((gen ssb-am)) (mus-frequency (ssb-am-sin-osc gen)))
(defmethod (setf mus-frequency) (val (gen ssb-am))
  (setf (mus-frequency (ssb-am-sin-osc gen)) val)
  (setf (mus-frequency (ssb-am-cos-osc gen)) val))
(defmethod mus-increment ((gen ssb-am)) (mus-increment (ssb-am-sin-osc gen)))
(defmethod (setf mus-increment) (val (gen ssb-am))
  (setf (mus-increment (ssb-am-sin-osc gen)) val)
  (setf (mus-increment (ssb-am-cos-osc gen)) val))
(defmethod mus-phase ((gen ssb-am)) (mus-phase (ssb-am-sin-osc gen)))
(defmethod (setf mus-phase) (val (gen ssb-am))
  (setf (mus-phase (ssb-am-sin-osc gen)) val)
  (setf (mus-phase (ssb-am-cos-osc gen)) (+ val (* 0.5 pi))))
(defmethod mus-data ((gen ssb-am)) (mus-data (ssb-am-dly gen)))
(defmethod mus-length ((gen ssb-am)) (mus-length (ssb-am-dly gen)))
(defmethod mus-order ((gen ssb-am)) (mus-order (ssb-am-dly gen)))
(defmethod mus-interp-type ((gen ssb-am)) mus-interp-none)
(defmethod mus-xcoeffs ((gen ssb-am)) (mus-xcoeffs (ssb-am-hilbert gen)))
(defmethod mus-xcoeff ((gen ssb-am) index) (mus-xcoeff (ssb-am-hilbert gen) index))

(defun mus-a0 (gen) (mus-xcoeff gen 0))
(defun mus-set-a0 (gen val) (setf (mus-xcoeff gen 0) val))
(defsetf mus-a0 mus-set-a0)
(defun mus-a1 (gen) (mus-xcoeff gen 1))
(defun mus-set-a1 (gen val) (setf (mus-xcoeff gen 1) val))
(defsetf mus-a1 mus-set-a1)
(defun mus-a2 (gen) (mus-xcoeff gen 2))
(defun mus-set-a2 (gen val) (setf (mus-xcoeff gen 2) val))
(defsetf mus-a2 mus-set-a2)
(defun mus-b1 (gen) (mus-ycoeff gen 1))
(defun mus-set-b1 (gen val) (setf (mus-ycoeff gen 1) val))
(defsetf mus-b1 mus-set-b1)
(defun mus-b2 (gen) (mus-ycoeff gen 2))
(defun mus-set-b2 (gen val) (setf (mus-ycoeff gen 2) val))
(defsetf mus-b2 mus-set-b2)

(defmethod mus-reset ((gen t))
  (warn "mus-reset is a no-op outside 'run'"))



(defun run-with-fm-and-pm (gen fm pm)
  (setf (mus-phase gen) (+ (mus-phase gen) pm))
  (let ((result (mus-run gen fm)))
    (setf (mus-phase gen) (- (mus-phase gen) pm))
    result))
