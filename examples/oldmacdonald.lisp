;; Old MacDonald -- including the lyrics to the first verse -- without
;; consonants!
;; --William Andrew Burnson <burnson2 (at) uiuc.edu>

;; Initialize everything

; (cd "/Lisp/clm-3/")

;(load "all.lisp")

(load (compile-file "jcvoi.ins"))

(defun note->hz ( note)
  (let* ((low 16.351597)
	 (raw (string note))
	 (key '((c bs) (df cs) (d) (ds ef)
		(e ff) (f es) (gf fs) (g)
		(gs af) (a) (as bf) (b cf)))
	 (pos (position-if #'digit-char-p raw))
	 (int (or (position (intern (subseq raw 0 pos))
			    key :test #'member)
		  (error "cant resolve note ~S" nam)))
	 (oct (if pos (parse-integer raw :start pos) 4))
	 )
    (* low (expt 2 (+ oct (/ int 12))))))

(defparameter gtime 0)
(defparameter ampf '(0 0 .4 1 2.6 1 3 0))
(defparameter w8 0.25)
(defparameter w4 0.5)
(defparameter w2 1.0)

(defun pwait (dur)
  (incf gtime dur))

(defun cow ()
  (fm-voice gtime 1.0 110 0.8 5 1
	    ampf ampf ampf ampf ampf ampf ampf
	    1 0 0 0.0 1 0.01 0 '(0 1 1 0) 0.2))

(defun vox (dur note vowel sex vib)
  ;;dur is in seconds
  ;;note is in note name
  ;;vowel is :ah :ee :uh :ow :ay
  ;;sex is :male :female
  (let ((v 1)
	(s 1))
    (ecase vowel
      ((:ah :oh) (setq v 1))
      (:ee (setq v 2))
      (:uh (setq v 3))
      (:ow (setq v 4))
      (:ay (setq v 5)))
    (ecase sex
      (:male (setq s 1))
      (:female (setq s 2)))

    (fm-voice gtime dur (/ (note->hz note) 2) 0.5 v s
	      ampf ampf ampf ampf ampf ampf ampf
	      1 0 0 0.0 1 0.00 0 ampf 0.00)))

(with-sound (:output "oldmacdonald.aiff")

  ;; old macdonald

  (pwait (* -1 gtime))

  (vox w4 :c3 :ah :male 0.0)
  (pwait w8)

  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)

  (vox w4 :ef3 :ee :male 0.0)
  (pwait w8)

  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)


;;;

  (vox w4 :f3 :ay :female 0.0)
  (pwait w8)

  (vox w8 :c4 :ow :female 0.0)
  (vox w8 :fs4 :ow :female 0.0)
  (pwait w8)

  (vox w4 :a3 :uh :male 0.0)
  (pwait w8)

  (vox w8 :cs4 :ow :female 0.0)
  (vox w8 :gs4 :ow :female 0.0)
  (pwait w8)

;;;


  (vox w4 :d3 :ee :male 0.0)
  (pwait w8)

  (vox w8 :a3 :ow :female 0.0)
  (vox w8 :fs4 :ow :female 0.0)
  (pwait w8)

  (vox w4 :g3 :ee :male 0.0)
  (pwait w8)

  (vox w8 :b3 :ow :female 0.0)
  (vox w8 :f4 :ow :female 0.0)
  (pwait w8)

;;;

  (cow)

  (vox w8 :g4 :ee :female 0.0)
  (vox w8 :e5 :ee :female 0.0)
  (pwait w8)
  (vox w8 :fs4 :ee :female 0.0)
  (vox w8 :ds5 :ee :female 0.0)
  (pwait w8)
  (vox w8 :f4 :ee :female 0.0)
  (vox w8 :d5 :ee :female 0.0)
  (pwait w8)
  (vox w8 :ef4 :ee :female 0.0)
  (vox w8 :cs5 :ee :female 0.0)
  (pwait w8)


;;; ;;;;;;


  (vox w8  :c6  :ow  :female 0.5) ;;
  (vox w4 :c3 :ah :male 0.0)
  (pwait w8)


  (vox w8  :c6  :ay  :female 0.5) ;;
  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)

  (vox w8  :c6  :ah  :female 0.5) ;;
  (vox w4 :ef3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :g5  :uh  :female 0.5) ;;
  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)


;;;


  (vox w8  :a5  :ah  :female 0.5) ;;
  (vox w4 :f3 :ay :female 0.0)
  (pwait w8)

  (vox w8  :a5  :ay  :female 0.5) ;;
  (vox w8 :c4 :ow :female 0.0)
  (vox w8 :fs4 :oh :female 0.0)
  (pwait w8)

  (vox w4  :g5  :ah  :female 0.5) ;;
  (vox w4 :a3 :uh :male 0.0)
  (pwait w8)

  (vox w8 :cs5 :ow :female 0.0)
  (vox w8 :gs5 :ow :female 0.0)
  (pwait w8)

;;;

  (vox w8  :e6  :ee  :female 0.5) ;;
  (vox w4 :d3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :e6  :ay  :female 0.5) ;;
  (vox w8 :a3 :ow :female 0.0)
  (vox w8 :fs4 :ow :female 0.0)
  (pwait w8)

  (vox w8  :d6  :ee  :female 0.5) ;;
  (vox w4 :g3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :d6  :ay  :female 0.5) ;;
  (vox w8 :b3 :ow :female 0.0)
  (vox w8 :f4 :ow :female 0.0)
  (pwait w8)

;;;

  (vox w4  :c6  :oh  :female 0.5) ;;

  (vox w8 :g4 :ee :female 0.0)
  (vox w8 :e5 :ee :female 0.0)
  (pwait w8)
  (vox w8 :fs4 :ee :female 0.0)
  (vox w8 :ds5 :ee :female 0.0)
  (pwait w8)
  (cow)
  (vox w4  :g5  :ah  :female 0.5) ;;
  (vox w8 :f4 :ee :female 0.0)
  (vox w8 :d5 :ee :female 0.0)
  (pwait w8)
  (vox w8 :ef4 :ee :female 0.0)
  (vox w8 :cs5 :ee :female 0.0)
  (pwait w8)

;;; ;;;;;;


  (vox w8  :c6  :ah  :female 0.5) ;;
  (vox w4 :c3 :ah :male 0.0)
  (pwait w8)


  (vox w8  :c6  :ee  :female 0.5) ;;
  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)

  (vox w8  :c6  :ah  :female 0.5) ;;
  (vox w4 :ef3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :g5  :ee  :female 0.5) ;;
  (vox w8 :g3 :ow :female 0.0)
  (vox w8 :e4 :ow :female 0.0)
  (pwait w8)


;;;


  (vox w8  :a5  :ah  :female 0.5) ;;
  (vox w4 :f3 :ay :female 0.0)
  (pwait w8)

  (vox w8  :a5  :ay  :female 0.5) ;;
  (vox w8 :c4 :ow :female 0.0)
  (vox w8 :fs4 :oh :female 0.0)
  (pwait w8)

  (vox w4  :g5  :ow  :female 0.5) ;;
  (vox w4 :a3 :uh :male 0.0)
  (pwait w8)

  (vox w8 :cs5 :ow :female 0.0)
  (vox w8 :gs5 :ow :female 0.0)
  (pwait w8)

;;;

  (vox w8  :e6  :ee  :female 0.5) ;;
  (vox w4 :d3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :e6  :ay  :female 0.5) ;;
  (vox w8 :a3 :ow :female 0.0)
  (vox w8 :fs4 :ow :female 0.0)
  (pwait w8)

  (vox w8  :d6  :ee  :female 0.5) ;;
  (vox w4 :g3 :ee :male 0.0)
  (pwait w8)

  (vox w8  :d6  :ay  :female 0.5) ;;
  (vox w8 :b3 :ow :female 0.0)
  (vox w8 :f4 :ow :female 0.0)
  (pwait w8)

;;;

  (vox w2  :c3  :ah  :male 1.0)
  (vox w2  :g3  :ah  :male 1.0)
  (vox w2  :c4  :ah  :male 1.0)
  (vox w2  :e5  :ah  :female 1.0)
  (vox w2  :c6  :oh  :female 0.5) ;;
  )