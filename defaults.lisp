;;; this file contains some of the global default values that control CLM
;;; in many cases the *clm-<>* form is *<>* during a given run; that is,
;;; *clm-channels* is the default, *channels* is the current setting


(in-package :common-tones)

(defvar *clm-srate* 44100)
(defvar *clm-channels* 1)

(defvar *clm-file-buffer-size* (* 64 1024))

(defvar *clm-file-name* "test.aiff")

(defvar *clm-header-type* mus-aifc)

(defvar *clm-data-format* mus-ldouble)

(defvar *clm-tempfile-data-format* mus-ldouble)
(defvar *clm-tempfile-header-type* mus-next)

(defvar *clm-verbose* nil)		; will cause instrument names and so on to be printed out
(defvar *clm-play* t)	                ; default for play arg in with-sound
(defvar *clm-player* nil)	        ; user-supplied DAC function
(defvar *clm-table-size* 512)      	; used for table-lookup tables and others
(defvar *clm-safety* 0)			; safety setting in Run (1: check gen, 2:check array indices)
(defvar *clm-array-print-length* 10)	; how much of an array to print out during debugging
(defvar *clm-init* nil)			; name of init file used during load process
(defvar *clm-search-list* (list ""))    ; directories to search in open-input*
(defvar *clm-notehook* nil)             ; default notehook
(defvar *clm-clipped* t)                ; is out-going data clipped or wrapped-around (the latter can cause arithmetic exceptions)
(defvar *clm-delete-reverb* nil)        ; whether with-sound should delete the reverb stream
(defvar *clm-reverb-channels* 1)        ; channels in reverb stream
(defvar *clm-statistics* nil)           ; statistics arg in with-sound

(defvar *clm-default-frequency* 0.0)    ; make-* default frequency
