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
