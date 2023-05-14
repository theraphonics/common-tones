;;; --------------------------------
;;; initialize CLM

#+sbcl (shadowing-import 'clm:double) ;#%$^#@!!!!
(use-package :clm)

;#+(and ccrma excl) (with-open-file (ifile "/etc/clm.conf" :direction :input :if-does-not-exist nil) (load ifile))
;;; it might be nice to make this a documented feature, and perhaps compatible with /etc/snd.conf

#-(or cmu sbcl)
(let ((init-file (merge-pathnames (or clm::*clm-init* "clm-init")
				  #+excl excl::*source-pathname*
				  #+openmcl ccl:*loading-file-source-file*
				  #+(or clisp lispworks) *load-pathname*
				  )))
  (with-open-file (ifile init-file	;bug -- with-open-file of nonexistent file gets CLOSE error no matter what
		   :direction :input
		   :if-does-not-exist nil)
    (if (streamp ifile)
	(load init-file))))

#+(or cmu sbcl)
(let ((init-file (open (merge-pathnames (or clm::*clm-init* "clm-init")
 					(truename *load-pathname*))
		       :direction :input
		       :if-does-not-exist nil)))
  (if init-file
      (progn
	(load init-file)
	(close init-file))))

;;; see README.clm for current status

(clm-initialize-links)
