;;; a "make" file for clm for ccrma
;;;
;;; this file assumes you want to save clm as an executable image

(pushnew :ccrma *features*)
(pushnew :alsa *features*)
(pushnew :cltl2 *features*)

(if (not (boundp 'clm-directory)) (setf clm-directory "/usr/ccrma/lisp/src/clm/"))
(if (not (boundp 'clm-bin-directory)) (setf clm-bin-directory "/usr/ccrma/lisp/obj/clm3/"))

(load (concatenate 'string clm-directory "all.lisp"))

(setf default-sound-file-name "/zap/test.snd")
(setf default-reverb-file-name "/zap/reverb.snd")
	
(excl:dumplisp :name "clm.dxl")
;;; now to start this requires lisp -I <clm-dir>/clm.dxl
