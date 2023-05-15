(in-package :common-tones/plugins)

;;;  ------------------------------------------------------------------------
;;; get a listing of sound files in a given directory (courtesy Marco Trevisani)

(defun sf (path)
  (let ((dir (directory path)))
    (loop for fil in dir do
      (let ((typ (pathname-type fil))
	    (name (filename->string fil)))
	(when (or (string= typ "snd") (string= typ "aiff"))
	  (format t "~2&Filename:~S ~%  Sampling-rate:~D Channels:~D Size:~,3FMb ~&  Duration:~,3Fsec"
		  name
		  (sound-srate name)
		  (sound-chans name)
		  (float (/ (* (sound-samples name) (sound-datum-size name)) 1000000))
		  (sound-duration name)))))))

;;; short-sf
;;; just filename and duration
(defun ssf (path)
  (let ((dir (directory path)))
    (loop for fil in dir do
      (let ((typ (pathname-type fil))
	    (name (filename->string fil)))
	(when (or (string= typ "snd") (string= typ "aiff"))
	  (format t "~2&Filename:~S ~%DURATION:~,4Fsec"
		  name (sound-duration name)))))))

;;; example (sf "/user/marco/mrc/temp/")
;;; and     (ssf "/user/marco/mrc/temp/")
