;;; test clm ffi

;;;

;;; links are in ffi.lisp, sndlib2clm.lisp, and clm1.lisp

;;;

;;; the linked functions are:

;;; sndlib2clm.lisp:

;;; mus-error->string mus-sound-samples mus-sound-framples mus-sound-datum-size mus-sound-data-location mus-sound-chans

;;; mus-sound-srate mus-sound-header-type mus-sound-data-format mus-sound-original-format mus-sound-write-date mus-sound-comment-start

;;; mus-sound-comment-end mus-sound-length mus-sound-type-specifier mus-sound-bits-per-sample mus-header-type-name mus-data-format-name

;;; mus-sound-comment mus-sound-duration mus-sound-initialize mus-sound-forget

;;; mus-audio-initialize mus-file-probe mus-clipping

;;; mus-set-clipping mus-header-samples mus-header-data-location

;;; mus-header-srate mus-header-type mus-header-format mus-header-comment-start mus-header-comment-end mus-header-type-specifier

;;; mus-header-bits-per-sample mus-header-loop-mode mus-header-loop-start mus-header-loop-end mus-header-mark-position mus-header-base-note

;;; mus-header-base-detune mus-header-set-raw-defaults mus-header-true-length mus-header-original-format mus-samples-to-bytes mus-bytes-to-samples

;;; mus-header-read mus-header-write mus-header-aux-comment-start mus-header-aux-comment-end mus-header-initialize mus-header-writable

;;; mus-header-sf2-entries mus-header-sf2-name mus-header-sf2-start mus-header-sf2-end mus-header-sf2-loop-start mus-header-sf2-loop-end

;;; mus-header-original-format-name mus-bytes-per-sample mus-header-chans

;;;

;;; ffi.lisp wrappers

;;; sound-chans sound-duration sound-data-format sound-data-location sound-datum-size sound-header-type sound-length sound-samples

;;; sound-framples sound-srate sound-comment mus-sound-loop-info

;;;

;;; ffi.lisp via cmus.c:

;;; array->file basic-convolve clm-continue-output clm-continue-reverb clm-fft clm-make-output clm-make-reverb

;;; clm-mix clm-random clm-scale-file file->array initialize-cmus mus-file-buffer-size

;;; mus-set-file-buffer-size mus-set-rand-seed mus-set-srate mus-srate reset-audio reset-headers reset-io sound-maxamp init-sc

;;;

;;; clm1.lisp:

;;; clm-seek-bytes clm-seek-floats clm-read-floats clm-write-floats clm-read-ints clm-write-ints c-open-input-file

;;; c-open-output-file c-create-file c-close c-seek clm-swap-ints clm-swap-doubles

;;;

;;; functions used in with-sound are not re-tested here (clm-scale-file, clm-mix, etc)


#-(or sun opencml mac-osx) (defvar oboe "/home/bil/cl/oboe.snd")
#+sun                      (defvar oboe "/export/home/bil/cl/oboe.snd")
#+(or openmcl mac-osx)     (defvar oboe "/Users/bil/cl/oboe.snd")

#-(or sun opencml mac-osx) (defvar nasahal "/home/bil/sf1/nasahal8.wav")
#+sun                      (defvar nasahal "/export/home/bil/sf1/nasahal8.wav")
#+(or openmcl mac-osx)     (defvar nasahal "/Users/bil/sf1/nasahal8.wav")

#-(or sun opencml mac-osx) (defvar forest "/home/bil/sf1/forest.aiff")
#+sun                      (defvar forest "/export/home/bil/sf1/forest.aiff")
#+(or openmcl mac-osx)     (defvar forest "/Users/bil/sf1/forest.aiff")

#-(or sun opencml mac-osx) (defvar four "/home/bil/cl/4.aiff")
#+sun                      (defvar four "/export/home/bil/cl/4.aiff")
#+(or openmcl mac-osx)     (defvar four "/Users/bil/cl/4.aiff")

#-(or sun opencml mac-osx) (defvar oss "OSS")
#+sun                      (defvar oss "sun probably")
#+(or openmcl mac-osx)     (defvar oss "Mac OSX")

#-(or sun opencml mac-osx) (defvar foss "OSS 3.8.2")
#+sun                      (defvar foss "Sun")
#+(or openmcl mac-osx)     (defvar foss "Mac OSX audio")

#-(or sun opencml mac-osx) (defvar sf2 "/home/bil/sf1/RealDrums.sf2")
#+sun                      (defvar sf2 "/export/home/bil/sf1/RealDrums.sf2")
#+(or openmcl mac-osx)     (defvar sf2 "/Users/bil/sf1/RealDrums.sf2")

(clm-initialize-links)

(defun header-test (name acend acstart detune base bits ce cs dl df le ls lm mark orig oname
		    samples srate bytes type spec1 spec2 typename writable chans frames dsize comment
		    sdur sdate)

  (mus-header-initialize) ; should be a no-op
  (mus-header-read name)
  (let ((val (mus-header-aux-comment-end 0)))
    (if (not (= val acend))
	(format t "~%;~A mus-header-aux-comment-end: ~A (~A)" name val acend)))
  (let ((val (mus-header-aux-comment-start 0)))
    (if (not (= val acstart))
	(format t "~%;~A mus-header-aux-comment-start: ~A (~A)" name val acstart)))
  (let ((val (mus-header-base-detune)))
    (if (not (= val detune))
	(format t "~%;~A mus-header-base-detune: ~A (~A)" name val detune)))
  (let ((val (mus-header-base-note)))
    (if (not (= val base))
	(format t "~%;~A mus-header-base-note: ~A (~A)" name val base)))
  (let ((val (mus-header-bits-per-sample)))
    (if (not (= val bits))
	(format t "~%;~A mus-header-bits-per-sample: ~A (~A)" name val bits)))
  (let ((val (mus-header-comment-end)))
    (if (not (= val ce))
	(format t "~%;~A mus-header-comment-end: ~A (~A)" name val ce)))
  (let ((val (mus-header-comment-start)))
    (if (not (= val cs))
	(format t "~%;~A mus-header-comment-start: ~A (~A)" name val cs)))
  (let ((val (mus-header-data-location)))
    (if (not (= val dl))
	(format t "~%;~A mus-header-data-location: ~A (~A)" name val dl)))
  (let ((val (mus-header-format)))
    (if (not (= val df))
	(format t "~%;~A mus-header-format: ~A (~A)" name val df)))
  (let ((val (mus-header-loop-end 0)))
    (if (not (= val le))
	(format t "~%;~A mus-header-loop-end: ~A (~A)" name val le)))
  (let ((val (mus-header-loop-start 0)))
    (if (not (= val ls))
	(format t "~%;~A mus-header-loop-start: ~A (~A)" name val ls)))
  (let ((val (mus-header-loop-mode 0)))
    (if (not (= val lm))
	(format t "~%;~A mus-header-loop-mode: ~A (~A)" name val lm)))
  (let ((val (mus-header-mark-position 0)))
    (if (not (= val mark))
	(format t "~%;~A mus-header-mark-position: ~A (~A)" name val mark)))
  (let ((val (mus-header-original-format)))
    (if (not (= val orig))
	(format t "~%;~A mus-header-original-format: ~A (~A)" name val orig)))
  (let ((val (mus-header-original-format-name (mus-header-original-format) (mus-header-type))))
    (if (not (string= val oname))
	(format t "~%;~A mus-header-original-format-name: ~A (~A)" name val oname)))
  (let ((val (mus-header-samples)))
    (if (not (= val samples))
	(format t "~%;~A mus-header-samples: ~A (~A)" name val samples)))
  (let ((val (mus-header-srate)))
    (if (not (= val srate))
	(format t "~%;~A mus-header-srate: ~A (~A)" name val srate)))
  (let ((val (mus-header-true-length)))
    (if (not (= val bytes))
	(format t "~%;~A mus-header-true-length: ~A (~A)" name val bytes)))
  (let ((val (mus-header-type)))
    (if (not (= val type))
	(format t "~%;~A mus-header-type: ~A (~A)" name val type)))
  (let ((val (mus-header-type-specifier)))
    (if (and (not (= val spec1))
	     (not (= val spec2)))
	(format t "~%;~A mus-header-type-specifier: ~A (~A ~A)" name val spec1 spec2)))
  (let ((val (mus-header-type-name type)))
    (if (not (string= val typename))
	(format t "~%;~A mus-header-type-name: ~A (~A)" name val typename)))
  (let ((val (mus-header-writable mus-next mus-bshort)))
    (if (not (eql val writable))
	(format t "~%;~A mus-header-writable: ~A (~A)" name val writable)))
  (let ((c (mus-header-chans)))
    (if (not (= c chans))
	(format t "~%;~A mus-header-chans: ~A (~A)" name c chans)))

  (mus-sound-initialize)
  (let ((b (mus-sound-bits-per-sample name)))
    (if (not (= b bits))
	(format t "~%;~A mus-sound-bits-per-sample: ~A (~A)" name b bits)))
  (let ((c (mus-sound-chans name)))
    (if (not (= c chans))
	(format t "~%;~A mus-sound-chans: ~A (~A)" name c chans)))
  (let ((val (mus-sound-comment-end name)))
    (if (not (= val ce))
	(format t "~%;~A mus-sound-comment-end: ~A (~A)" name val ce)))
  (let ((val (mus-sound-comment-start name)))
    (if (not (= val cs))
	(format t "~%;~A mus-sound-comment-start: ~A (~A)" name val cs)))
  (let ((val (mus-sound-data-location name)))
    (if (not (= val dl))
	(format t "~%;~A mus-sound-data-location: ~A (~A)" name val dl)))
  (let ((val (mus-sound-data-format name)))
    (if (not (= val df))
	(format t "~%;~A mus-sound-data-format: ~A (~A)" name val df)))
  (let ((val (mus-sound-samples name)))
    (if (not (= val samples))
	(format t "~%;~A mus-sound-samples: ~A (~A)" name val samples)))
  (let ((val (mus-sound-framples name)))
    (if (not (= val frames))
	(format t "~%;~A mus-sound-framples: ~A (~A)" name val frames)))
  (let ((val (mus-sound-srate name)))
    (if (not (= val srate))
	(format t "~%;~A mus-sound-srate: ~A (~A)" name val srate)))
  (let ((val (mus-sound-datum-size name)))
    (if (not (= val dsize))
	(format t "~%;~A mus-sound-datum-size: ~A (~A)" name val dsize)))
  (let ((com (mus-sound-comment name)))
    (if (not (stringp com))
	(format t "~%;~A nil comment" name)
      (if (not (string= com comment))
	  (format t "~%;~A mus-sound-comment: ~A (~A)" name com comment))))
  (let ((dur (mus-sound-duration name)))
    (if (> (abs (- dur sdur)) .001)
	(format t "~%;~A mus-sound-duration: ~A (~A)" name dur sdur)))
  (let ((val (mus-sound-header-type name)))
    (if (not (= val type))
	(format t "~%;~A mus-sound-header-type: ~A (~A)" name val type)))
  (let ((val (mus-sound-length name)))
    (if (not (= val bytes))
	(format t "~%;~A mus-sound-length: ~A (~A)" name val bytes)))
  (let ((val (mus-sound-type-specifier name)))
    (if (and (not (= val spec1))
	     (not (= val spec2)))
	(format t "~%;~A mus-sound-type-specifier: ~A (~A ~A)" name val spec1 spec2)))
  (let ((val (mus-sound-original-format name)))
    (if (not (= val orig))
	(format t "~%;~A mus-sound-original-format: ~A (~A)" name val orig)))
  (let ((date (mus-sound-write-date name)))
    (if (not (= date sdate))
	(format t "~%;~A mus-sound-write-date: ~A (~A)" name date sdate)))

  (let ((c (sound-chans name)))
    (if (not (= c chans))
	(format t "~%;~A sound-chans: ~A (~A)" name c chans)))
  (let ((val (sound-data-location name)))
    (if (not (= val dl))
	(format t "~%;~A sound-data-location: ~A (~A)" name val dl)))
  (let ((val (sound-data-format name)))
    (if (not (= val df))
	(format t "~%;~A sound-data-format: ~A (~A)" name val df)))
  (let ((val (sound-samples name)))
    (if (not (= val samples))
	(format t "~%;~A sound-samples: ~A (~A)" name val samples)))
  (let ((val (sound-framples name)))
    (if (not (= val frames))
	(format t "~%;~A sound-framples: ~A (~A)" name val frames)))
  (let ((val (sound-srate name)))
    (if (not (= val srate))
	(format t "~%;~A sound-srate: ~A (~A)" name val srate)))
  (let ((val (sound-datum-size name)))
    (if (not (= val dsize))
	(format t "~%;~A sound-datum-size: ~A (~A)" name val dsize)))
  (let ((com (sound-comment name)))
    (if (not (string= com comment))
	(format t "~%;~A sound-comment: ~A (~A)" name com comment)))
  (let ((dur (sound-duration name)))
    (if (> (abs (- dur sdur)) .001)
	(format t "~%;~A sound-duration: ~A (~A)" name dur sdur)))
  (let ((val (sound-header-type name)))
    (if (not (= val type))
	(format t "~%;~A sound-header-type: ~A (~A)" name val type)))
  )

(header-test oboe 0 0 0 0 0 0 0 28 mus-bshort 0 0 0 -1 3 "unknown"
	     50828 22050 101684 mus-next 1684960046 779316836 "Sun/Next" t 1 50828 2 ""
	     2.305 1099318209)

(header-test forest 123 116 0 60 0 0 0 186 mus-bshort 2 1 1 -1 16 "unknown"
	     344610 44100 689406 mus-aiff 1179011393 1095321158 "AIFF" t 2 172305 2 (format nil "Prosonus~%forest~%")
	     3.907 953229249)
(let ((vals (mus-sound-loop-info forest)))
  (if (or (not (arrayp vals))
	  (not (= (aref vals 0) 1))
	  (not (= (aref vals 1) 2))
	  (not (= (aref vals 2) 3))
	  (not (= (aref vals 3) 4))
	  (not (= (aref vals 4) 60))
	  (not (= (aref vals 5) 0))
	  (not (= (aref vals 6) 1))
	  (not (= (aref vals 7) 0)))
      (format t "~%;mus-sound-loop-info: ~A" vals)))

(header-test nasahal 109251 109182 0 60 8 0 0 44 mus-ubyte 2 1 0 -1 1 "unknown"
	     109130 11025 109252 mus-riff 1163280727 1463899717 "RIFF" t 1 109130 1
	     (format nil "ICRD: 1997-02-22~%IENG: Paul R. Roger~%ISFT: Sound Forge 4.0~%")
	     9.898 886785774)

(mus-header-read sf2)
(let ((val0 (mus-header-sf2-loop-end 0))
      (val1 (mus-header-sf2-loop-end 1)))
  (if (not (= val0 20602))
      (format t "~%;mus-header-sf2-loop-end (0): ~A (20602)" val0))
  (if (not (= val1 29692))
      (format t "~%;mus-header-sf2-loop-end (1): ~A (29692)" val1)))
(let ((val0 (mus-header-sf2-loop-start 0))
      (val2 (mus-header-sf2-loop-start 2)))
  (if (not (= val0 8))
      (format t "~%;mus-header-sf2-loop-start (0): ~A (8)" val0))
  (if (not (= val2 29740))
      (format t "~%;mus-header-sf2-loop-start (2): ~A (29740)" val2)))
(let ((val0 (mus-header-sf2-end 0))
      (val1 (mus-header-sf2-end 1)))
  (if (not (= val0 20610))
      (format t "~%;mus-header-sf2-end (0): ~A (20610)" val0))
  (if (not (= val1 29700))
      (format t "~%;mus-header-sf2-end (1): ~A (29700)" val1)))
(let ((val0 (mus-header-sf2-start 0))
      (val2 (mus-header-sf2-start 2)))
  (if (not (= val0 0))
      (format t "~%;mus-header-sf2-start (0): ~A (0)" val0))
  (if (not (= val2 29732))
      (format t "~%;mus-header-sf2-start (2): ~A (29732)" val2)))
(let ((val (mus-header-sf2-entries)))
  (if (not (= val 21))
      (format t "~%;mus-header-sf2-entries: ~A (21)" val)))
(let ((val0 (mus-header-sf2-name 0))
      (val10 (mus-header-sf2-name 10)))
  (if (not (string= val0 "Snare 4"))
      (format t "~%;mus-header-sf2-name (0): ~A (\"Snare 4\")" val0))
  (if (not (string= val10 "Rim"))
      (format t "~%;mus-header-sf2-name (10): ~A (\"Rim\")" val10)))

(initialize-cmus) ; no-op
(let ((samples (mus-bytes-to-samples mus-lint 32)))
  (if (not (= samples 8))
      (format t "~%;mus-bytes-to-samples: ~A (8)" samples)))
(let ((bytes (mus-bytes-per-sample mus-bint)))
  (if (not (= bytes 4))
      (format t "~%;mus-bytes-per-sample: ~A (4)" bytes)))
(let ((str (mus-data-format-name mus-bshort)))
  (if (not (string= str "big endian short (16 bits)"))
      (format t "~%;mus-data-format-name: ~A" str)))
(let ((str (mus-error-type->string 12)))
  (if (not (string= str "no sample input")) ; MUS_NO_SAMPLE_INPUT
      (format t "~%;mus-error-type->string: ~A" str)))
(let* ((new-value (mus-clipping)))
  (mus-set-clipping new-value)
  (let ((def (mus-clipping)))
    (if (not (eql def new-value))
	(format t "~%;mus-set-clipping: ~A" def)))
  (mus-set-clipping new-value))

(let ((sbytes (mus-samples-to-bytes mus-mulaw 32)))
  (if (not (= sbytes 32))
      (format t "~%;mus-samples-to-bytes: ~A" sbytes)))
(mus-sound-forget forest)

(let* ((default (mus-file-buffer-size))
       (new-value 4096))
  (mus-set-file-buffer-size new-value)
  (let ((bytes (mus-file-buffer-size)))
    (if (not (= bytes new-value))
	(format t ";~%mus-set-file-buffer-size: ~A" bytes)))
  (mus-set-file-buffer-size default))

(let ((sr (mus-srate)))
  (if (not (= sr 22050.0))
      (format t ";~%mus-srate: ~A" sr)))
(mus-set-srate (double 44100.0))
(let ((sr (mus-srate)))
  (if (not (= sr 44100.0))
      (format t ";~%mus-set-srate: ~A" sr)))
(mus-set-srate (double 22050.0))

(mus-set-rand-seed 12345)
(mus-audio-initialize) ; no-op

(let ((farr (make-double-float-array 4))
      (iarr (make-integer-array 4 :initial-element 0)))
  (sound-maxamp oboe 1 farr iarr)
  (if (not (= 24971 (aref iarr 0)))
      (format t "~%;sound-maxamp oboe sample: ~A" (aref iarr 0)))
  (if (> (abs (- (aref farr 0) 0.147)) .001)
      (format t "~%;sound-maxamp oboe amp: ~A" (aref farr 0)))
  (sound-maxamp four 4 farr iarr)
  (if (not (= 810071 (aref iarr 0)))
      (format t "~%;sound-maxamp 4.aiff 0 sample: ~A" (aref iarr 0)))
  (if (> (abs (- (aref farr 0) 0.245)) .001)
      (format t "~%;sound-maxamp 4.aiff 0 amp: ~A" (aref farr 0)))
  (if (not (= 810071 (aref iarr 1)))
      (format t "~%;sound-maxamp 4.aiff 1 sample: ~A" (aref iarr 0)))
  (if (> (abs (- (aref farr 1) 0.490)) .001)
      (format t "~%;sound-maxamp 4.aiff 1 amp: ~A" (aref farr 0)))
  (if (not (= 810071 (aref iarr 2)))
      (format t "~%;sound-maxamp 4.aiff 2 sample: ~A" (aref iarr 0)))
  (if (> (abs (- (aref farr 2) 0.735)) .001)
      (format t "~%;sound-maxamp 4.aiff 2 amp: ~A" (aref farr 0)))
  (if (not (= 810071 (aref iarr 3)))
      (format t "~%;sound-maxamp 4.aiff 3 sample: ~A" (aref iarr 0)))
  (if (> (abs (- (aref farr 3) 0.980)) .001)
      (format t "~%;sound-maxamp 4.aiff 3 amp: ~A" (aref farr 0))))

(let ((file (c-create-file "test.data"))
      (arr (make-double-float-array 16 :initial-element (double 0.0))))
  (loop for i from 0 to 15 do
    (setf (aref arr i) (double (* i .1))))
  (clm-write-floats file arr 16)
  (c-close file))

(let ((file (c-open-input-file "test.data"))
	(arr (make-double-float-array 16 :initial-element (double 0.0))))
  (clm-read-floats file arr 16)
  (loop for i from 0 to 15 do
    (if (> (abs (- (aref arr i) (* i .1))) .001)
	(format t "~%;clm1 floats: arr[~D] = ~F (~F)~%" i (aref arr i) (* i .1))))
  (clm-seek-floats file 2)
  (clm-read-floats file arr 10)
  (loop for i from 0 to 9 do
    (if (> (abs (- (aref arr i) (* (+ i 2) .1))) .001)
	(format t "~%;seek clm1 floats: arr[~D] = ~F (~F)~%" i (aref arr i) (* (+ i 2) .1))))
  #-clisp (clm-swap-doubles arr 4)
  (c-close file))

(delete-file "test.data")

(let ((file (c-open-output-file "test.data"))
      (arr (make-integer-array 16 :initial-element 0)))
  (loop for i from 0 to 15 do
    (setf (aref arr i) i))
  (clm-write-ints file arr 16)
  (c-close file))

(let ((file (c-open-input-file "test.data"))
	(arr (make-integer-array 16 :initial-element 0)))
  (clm-read-ints file arr 16)
  (loop for i from 0 to 15 do
    (if	(not (= (aref arr i) i))
	(format t "~%;clm1 ints: arr[~D] = ~D (should be ~D)~%" i (aref arr i) i)))
  (clm-seek-bytes file (* 4 2))
  (clm-read-ints file arr 10)
  (loop for i from 0 to 9 do
    (if	(not (= (aref arr i) (+ i 2)))
	(format t "~%;seek clm1 ints: arr[~D] = ~D (should be ~D)~%" i (aref arr i) (+ i 2))))
  (clm-swap-ints arr 4)
  (if (not (= (aref arr 1) 50331648))
      (format t "~%;clm-swap-ints: ~A (should be 50331648)" (aref arr 1)))
  (c-seek file 3 0)
  (c-close file))

(delete-file "test.data")

(let ((temp-file (let ((data (make-double-float-array 32)))
		   (do ((i 0 (1+ i)))
		       ((= i 32))
		     (setf (aref data i) (double (* i .01))))
		   (array->file "temp.snd" data 32 22050 1)
		   "temp.snd"))
      (new-data (make-double-float-array 32)))
  (file->array temp-file 0 0 32 new-data)
  (loop for i from 0 to 31 do
    (if (> (abs (- (aref new-data i) (* i .01))) .001)
	(format t "~%;array->file [~D]: ~A (should be ~A)" i (aref new-data i) (* i .01))))
  (delete-file temp-file))

(let ((last-val 10.0))
  (loop for i from 0 to 10 do
    (let ((val (clm-random (double 1.0))))
      (if (< val 0.0) (format t "~%;clm-random: ~A (should be >= 0)" val))
      (if (= val last-val) (format t "~%;clm-random: ~A ~A?" last-val val))
      (if (> val 1.0) (format t "~%;clm-random: ~A (should be <= 1.0)" val))
      (setf last-val val))))

(let ((comment "this is a comment"))
  (mus-header-write "test.snd" mus-next 22050 1 0 0 mus-bshort comment (length comment))
  (if (not (mus-file-probe "test.snd"))
      (format t "~%;mus-header-write no output?")
    (progn
      (if (not (= (mus-sound-srate "test.snd") 22050)) (format t "~%;mus-header-write srate: ~A" (mus-sound-srate "test.snd")))
      (if (not (= (mus-sound-chans "test.snd") 1)) (format t "~%;mus-header-write chans: ~A" (mus-sound-chans "test.snd")))
      (if (not (= (mus-sound-header-type "test.snd") mus-next)) (format t "~%;mus-header-write type: ~A" (mus-sound-header-type "test.snd")))
      (if (not (= (mus-sound-data-format "test.snd") mus-bshort)) (format t "~%;mus-header-write format: ~A" (mus-sound-data-format "test.snd")))
      (if (not (string= (mus-sound-comment "test.snd") comment)) (format t "~%;mus-header-write comment: ~A" (mus-sound-comment "test.snd"))))))

(delete-file "test.snd")

(mus-header-set-raw-defaults 44100 1 mus-bshort)

#+linux (mus-oss-set-buffers 4 12)

#-(or clisp sbcl)
(let* ((data (make-double-array 8 :initial-contents '(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))
       (signal (make-double-array 8 :initial-contents '(0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0))))
  (convolution data signal 8)
  (if (> (abs (- (aref data 2) 1.0)) .001)
      (format t "~%;convolution: ~A" data)))

#-(or clisp sbcl)
(let* ((rdata (make-double-array 8))
       (idata (make-double-array 8)))
  (setf (aref rdata 2) (double 1.0))
  (fft rdata idata 8 1)
  (if (> (abs (- (aref rdata 2) -1.0)) .001)
      (format t "~%;clm-fft(2): ~A (should be -1.0)" (aref rdata 2)))
  (if (> (abs (- (aref idata 3) -1.0)) .001)
      (format t "~%;clm-fft(3): ~A (should be -1.0)" (aref idata 3)))
  (fft rdata idata 8 -1)
  (if (> (abs (- (aref rdata 2) 8.0)) .001)
      (format t "~%;clm-ifft(2): ~A (should be 8.0)" (aref rdata 2)))
  (if (> (abs (- (aref idata 3) 0.0)) .001)
      (format t "~%;clm-ifft(3): ~A (should be 0.0)" (aref idata 3))))

#-(or clisp sbcl)
(let* ((data (make-double-array 8 :initial-contents '(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))
       (signal (make-double-array 8 :initial-contents '(0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0))))
  (correlate data signal 8)
  (if (> (abs (- (aref data 2) 1.0)) .001)
      (format t "~%;correlate: ~A" data)))

#-(or clisp sbcl)
(let* ((data (make-double-array 8 :initial-contents '(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))))
  (autocorrelate data 8)
  (if (> (abs (- (aref data 0) 1.0)) .001)
      (format t "~%;autocorrelate: ~A" data)))

#-(or clisp sbcl)
(let* ((data (make-double-array 8 :initial-contents '(0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0))))
  (autocorrelate data 8)
  (if (> (abs (- (aref data 0) 1.0)) .001)
      (format t "~%;autocorrelate: ~A" data)))