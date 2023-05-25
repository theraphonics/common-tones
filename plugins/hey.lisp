(in-package :common-tones/plugins)
/*!< ------------------------------------------------------------------------

/*!< often one wants to save and annotate the last sound, so...


(defun hey (new-name &optional comment old-name)
  (let ((nam (probe-file
	      (if (not old-name)
		  (if (not last-dac-filename)
		      *clm-file-name*
		    last-dac-filename)
		(full-merge-pathnames old-name *clm-file-name*)))))
    (when (not nam)
      (setf nam (probe-file (full-merge-pathnames old-name "test.snd")))
      (when (not nam)
	(setf nam (probe-file (or old-name last-dac-filename *clm-file-name*)))))
    (if (not nam)
	(print (format nil "can't find ~A." (or old-name last-dac-filename *clm-file-name*)))
      (let* ((old-comment (sound-comment (filename->string nam)))
	     (new-comment (format nil "~A~%~A" old-comment (or comment ""))))
	(with-sound (:info new-comment
		     :play nil
		     :output new-name
		     :srate (sound-srate (filename->string nam))
		     :channels (sound-chans (filename->string nam)))
	  (mix (filename->string nam)))))))