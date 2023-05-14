



;;;  ------------------------------------------------------------------------
;;; add and cut -- quick access to *clm-beg-offset*

(defun add (secs) (incf *offset* (floor (* *srate* secs))))
(defun cut (secs) (decf *offset* (floor (* *srate* secs))))
