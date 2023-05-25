(in-package :common-tones)

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
