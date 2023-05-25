(in-package :common-tones/generators)

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
