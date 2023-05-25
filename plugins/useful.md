;;; useful hacks for CLM:

;;;

;;; hey:              add a comment to file just computed

;;; gliss-dur:        true, post-src change, file duration (see also dur-gliss)

;;; how to run CLM from a shell script

;;; sf and ssf:       info on sound files in directory

;;; load-ins:         load all instruments in a directory

;;; with-instruments: list instruments needed by subsequent note list for autoloading

;;; add and cut:      easier access to *offset*

;;; rmix:             mix with portion sent to reverb

;;; remote-play:      play sound (e.g. with-sound output) on a remote host

;;; fft-mag-and-phase: fft rectangular to polar coordinates

;;; amp-to-dB et al:  macros for translating between dB and linear amps


; (export '(db-to-amp amp-to-db vol-to-amp adb-to-amp amp-to-adb rmix with-instruments load-ins))