;;; Load file for CLM.
;;; 
;;; The CLM sources are found on the current directory unless the variable clm-directory is set.
;;; The binaries (and compiled C modules, where applicable) are written to clm-bin-directory, if bound.
;;; See make-clm.cl for one example of how to set everything up.
;;;
;;; for example, say we've untarred clm-4.tar to the current directory,
;;; but we want the .fasl and .o files written to /zap -- start lisp,
;;; (setf clm-directory ".") (setf clm-bin-directory "/zap/") (load "all.lisp")
;;;
;;; (setf *clm-player* (concatenate 'string clm-bin-directory "sndplay"))
;;;   causes CLM to use its local version of sndplay
;;;
;;; to force the configure script to run even if mus-config.h exists, (pushnew :reconfigure *features*)

(pushnew :clm2 *features*) ; for CM
(pushnew :clm3 *features*)
(pushnew :clm4 *features*)
(pushnew :clm5 *features*)

#+(and openmcl (not linux) (not linux-target) (not linuxppc-target)) (pushnew :mac-osx *features*)
#+(or darwin macosx) (pushnew :mac-osx *features*) ; acl uses macosx

(proclaim '(special clm-directory clm-bin-directory))

(if (not (boundp 'clm-directory)) 
    (setf clm-directory 
      (namestring
       (truename 
	(directory-namestring (or *load-pathname* "./"))))))

#+openmcl (if (and (stringp clm-directory)
		   (> (length clm-directory) 0)
		   (not (char= (elt clm-directory (1- (length clm-directory))) #\/)))
	      (setf clm-directory (concatenate 'string clm-directory "/")))

(if (not (boundp 'clm-bin-directory)) (setf clm-bin-directory clm-directory))


#|
  The various setup possibilities are handled via Lisp's *features* list --
  we push something on that list to tell Lisp what to include in the current setup.
  The CLM *features* are:

     :reconfigure    force configure script to run again

The *features* that CLM adds itself are:

     :clm
     :linux           if linux86
     :windoze         windows
     :little-endian (:big-endian) if not already present
     :acl-50          Franz changed the foreign function interface in ACL 5
     :acl-60          Franz changed the foreign function interface in ACL 6
     :acl-61 :acl-62  and so on...
     
  The other *features* that CLM currently notices (leaving aside walk.lisp and loop.lisp) are:

     :sgi             SGI
     :sun,solaris     Sun (sometimes ignored or removed by clm)
     :hpux            HPUX.
     :i386, i486, pc386  386-style processor (includes 486 and Pentium)
     :freebsd         ACL, CmuCL on FreeBSD.
     :linux86         Linux (in excl, if this is present, and :little-endian, we remove :sun from *features*)
     :cmu             CMU-CL
     :excl            Franz Inc's Lisp (Allegro CL)
     :alsa            use ALSA, rather than OSS (Linux)
     :jack            use the Jack Audio Connection Kit driver (linux), also requires libsamplerate
     :powerpc or :ppc Macintosh PowerPC
     :irix            SGI.
     :cltl2           Current lisp is a version 2 CL -- that is, it uses the later package syntax, etc
     :x3j13           A late enough version of the upcoming ANSI CL to have read-sequence
     :little-endian   Underlying machine is little endian (also :big-endian)
     :dlfcn           Foreign code loader is dlopen-related (SGI, Linux)
     :ccrma           Special ACL 4.3 output type for us (avoid collision with ACL 4.3 on SGI and Linux)
                      and use /zap rather than /tmp for clm/snd communication paths
     :win98           Windows 98 where longer file names are apparently accessible
     :mswindows       ACL under Windows
     :allegro-cl-lite ACL with no compiler (the free version of ACL under Windoze)
     :mac-osx         Mac OSX -- this might need to be pushed explicity before loading this file
     :sbcl            Steel Bank CL (cmu-cl descendent)
     :openmcl         OpenMCL
     :darwin          Mac OSX
     :macosx          ACL 7/8 OSX
     :clisp           Clisp
|#

#+(and (or solaris sparc sunos) (not sun)) (pushnew :sun *features*)
#+(or mswindows win98 win32) (pushnew :windoze *features*)
#+(and linux86 (not linux)) (pushnew :linux *features*)
#+(and excl linux86 little-endian) (setf *features* (remove :sun *features*))
#+bsd386 (pushnew :linux *features*)
;;; acl-50 et all pushed in acl.cl
#+irix (pushnew :sgi *features*)
#+(or x3j13 draft-ansi-cl-2) (pushnew :cltl2 *features*)
#+(and cmu freebsd) (pushnew :linux *features*)
#+(and clisp unix macos) (pushnew :mac-osx *features*)


#-windoze (if (and (stringp clm-bin-directory)
		   (> (length clm-bin-directory) 1)
		   (not (char= (elt clm-bin-directory (1- (length clm-bin-directory))) #\/)))
	      (setf clm-bin-directory (concatenate 'string clm-bin-directory "/")))

#+excl (load (concatenate 'string clm-directory "acl.cl"))

(let ((oops (remove-duplicates (intersection *features* (list :excl :cmu)))))
  (if (and oops (> (length oops) 1))
      (warn "somehow all.lisp thinks ~D different lisps are running here: ~A" (length oops) oops)))

(let ((oops (remove-duplicates (intersection *features* (list :sun :linux :sgi :hpux)))))
  (if (and oops (> (length oops) 1))
      (warn "somehow all.lisp thinks ~D different machines are running here: ~A" (length oops) oops)))

#+(and (not (or big-endian little-endian)) (or (and openmcl x86-target) x86-64 pc386 bsd386 i386 i486 i586 i686 x86 win32)) (pushnew :little-endian *features*)
#+(and (not (or big-endian little-endian)) (or (and openmcl (not x86-target)) sgi hpux powerpc ppc)) (pushnew :big-endian *features*)
#+(and (not (or big-endian little-endian)) excl)
  (pushnew
   (let ((x '#(1)))
     (if (not (= 0 (sys::memref x #.(sys::mdparam 'comp::md-svector-data0-adj) 0 :unsigned-byte)))
	 :little-endian
       :big-endian))
   *features*)


(pushnew :clm *features*)

;#+windoze (excl:shell "cp config.windoze mus-config.h")

(defvar reconfigure #+reconfigure t #-reconfigure nil)

(if (not (probe-file (concatenate 'string clm-directory "mus-config.h")))
    (setf reconfigure t))
(defvar configure nil)

#+(and linux (not oss) (not jack) (not alsa)) (pushnew :alsa *features*)
#+(and clisp unix (not macos) (not sun) (not oss) (not jack) (not alsa)) (pushnew :alsa *features*)

(if reconfigure
    (progn
      (setf configure (concatenate 'string "cd " clm-directory " && ./configure --quiet"))
      
      #+(and clozure darwinx8632-target)
      (setf configure (concatenate 'string configure " CLFAGS='-arch i386' LDFLAGS='-arch i386'"))
      #+lispworks-32bit (setf configure (concatenate 'string configure " --host=i686"))
      #+alsa (setf configure (concatenate 'string configure " --with-alsa"))
      #+oss (setf configure (concatenate 'string configure " --with-oss"))
      #+jack (setf configure (concatenate 'string configure " --with-jack"))
      ;#+sb-thread (setf configure (concatenate 'string configure " --enable-threads"))
      ;removed 28-Sep-11
      
      (format t ";   running ~A~%" configure)
      #+excl (excl:run-shell-command configure :wait t)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+lispworks (sys::call-system-showing-output configure)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+clisp (ext::shell configure)
      )
  #-openmcl (format t ";   using existing configuration file mus-config.h~%")
  )

(defvar *shared-object-extension* 
  #+dlfcn "so" 
  #+dlhp "sl" 
  #+(or windoze dlwin) "dll" 
  #+(or (and excl macosx) (and openmcl (not linux-target) (not linuxppc-target))) "dylib"
  #-(or dlfcn dlhp dlwin windoze (and excl macosx) (and openmcl (not linux-target) (not linuxppc-target))) "so"
  )

(defvar pre-c-name #-windoze "saved-" #+windoze "n")
(defvar *obj-ext* #-windoze ".o" #+windoze ".obj")

(defvar *cflags* (concatenate 'string " -I" clm-directory " -O2"
			      #-windoze " -g"
			      #+(and excl linux debug) " -Wall"
			      #+lispworks-32bit " -m32"
			      #+(and excl macosx) " -dynamic"
			      #+(and clozure darwinx8632-target) " -arch i386"
			      #+(or macosx (and mac-osx openmcl)) " -no-cpp-precomp"
			      #+clisp " -fPIC"
			      #+(or netbsd x86-64) " -fPIC"
			      #+(and sgi acl-50) " -n32 -w"
			      #+(and excl linux) " -fPIC"        ; 23-Mar-2018
			      #+(and excl freebsd) " -fPIC -DPIC"
			      ))

#+alsa (if (probe-file "/usr/include/alsa/asoundlib.h") (setf *cflags* (concatenate 'string *cflags* " -DHAVE_ALSA_ASOUNDLIB_H")))

(defvar *csoflags* (concatenate 'string
				#+sgi " -shared -all"
				#+(or (and linux (not lispworks-32bit)) clisp ) " -shared -fPIC"
				#+(or linuxppc-target (and mac-osx (not excl) (not cmu) (not sbcl) (not openmcl))) " -shared -whole-archive"
				#+(and lispworks-32bit) "-L/usr/lib -m32 --shared"
				#+(and openmcl linux-target) " -shared"
				#+(and mac-osx cmu) " -dynamiclib"
				#+(and excl macosx) " -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress"
				#+hpux " +z -Ae +DA1.1" ;" -b"?
				#+sun " -G"
				#+windoze " -D_MT -MD -nologo -LD -Zi -W3"
				#+(and excl freebsd) " -Bshareable -Bdynamic"
				#+netbsd " -shared"
				))

(defvar *lib-ext* #+windoze "lib" #-windoze "a")
(defvar *ld* #+windoze "cl" 
             #+(and (or lispworks clisp sbcl openmcl (and mac-osx cmu)) (not linuxppc-target)) "gcc" 
	     #-(or (and (or lispworks clisp sbcl openmcl (and mac-osx cmu)) (not linuxppc-target)) windoze) "ld")
(defvar *cc* #+windoze "cl" #-windoze "gcc")
(defvar *ldflags* #+windoze " " #-windoze " -r -o ")


;;; --------------------------------
;;; Allegro CL 

#+excl (setf (excl:package-definition-lock (find-package ':common-lisp)) nil)

#+excl (when (eq excl:*current-case-mode* :case-sensitive-upper)
	 (warn "you've chosen the one case (sensitive-upper) that is incompatble with clm -- changing to insensitive-upper")
	 (excl:set-case-mode :case-insensitive-upper))

#+(and excl windoze) (chdir clm-directory)
#+(and excl windoze) (setf *default-pathname-defaults* (excl:current-directory))

;;; in ACL 8.2/Linux, there can be a problem with selinux leading to the loader complaint:
;;;   "libclm.so: cannot restore segment prot after reloc: Permission denied."
;;; it's a bother to call chcon -t textrel_shlib_t libclm.so by hand all the time, so
;;;   in that case define use-chcon t below

(defvar use-chcon nil)

#+excl (progn

  (require :foreign)
  (setf excl:*global-gc-behavior* :auto) ;turn off the gc tenure message
  (setf excl:*redefinition-warnings* nil)

  (defun compile-and-load (name)
    (let* ((dir clm-directory)
	   (bindir clm-bin-directory)
	   (cname (concatenate 'string dir name ".lisp"))
	   (lname #-allegro-cl-lite (concatenate 'string bindir name "." excl:*fasl-default-type*)
		  #+allegro-cl-lite cname))
      (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
      #-allegro-cl-lite
      (if (probe-file cname)
	  (if (or (not (probe-file lname))
		  (> (file-write-date (truename cname)) (file-write-date (truename lname))))
	      (handler-bind ((excl:compiler-no-in-package-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
			     (excl:compiler-undefined-functions-called-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
			     (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
			    (compile-file cname :output-file lname))))
      (handler-bind ((simple-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))) (load lname))
      ))

  #-loop (let ((*default-pathname-defaults* (parse-namestring ""))) (require :loop))

  (setq sys:*source-file-types* '("cl" "lisp" nil "ins" "cm" "clm" "cmn"))

  (setq sys:*load-search-list*		;don't want to compile note-lists
    (append sys:*load-search-list* (list (make-pathname :type "ins")
					 (make-pathname :type "cm")
					 (make-pathname :type "clm") 
					 (make-pathname :type "cmn") 
					 )))
  )


;;; --------------------------------
;;; OpenMCL

#+openmcl 
(unless (get-dispatch-macro-character #\# #\,) 
  ;; since the "#," dispatch macro used by walk.lisp is not part of 
  ;; ANSI CL it was (rather gratuitously) removed from openmcl 1.0. 
  ;; we add it back here. 
  (set-dispatch-macro-character 
   #\# 
   #\, 
   #'(lambda (stream subchar numarg) 
       (let* ((sharp-comma-token ccl::*reading-for-cfasl*)) 
         (if (or *read-suppress* (not ccl::*compiling-file*) (not sharp-comma-token)) 
             (ccl::read-eval stream subchar numarg) 
             (progn 
               (ccl::require-no-numarg subchar numarg) 
               (list sharp-comma-token (read stream t nil t))))))))

#+openmcl
(defun compile-and-load (name)
  (let* ((dir clm-directory)
     (bindir clm-bin-directory)
     (cname (concatenate 'string dir name ".lisp"))
     (lname (concatenate 'string bindir name "." (pathname-type (compile-file-pathname cname)))))
    (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
    (if (probe-file cname)
	(if (or (not (probe-file lname))
		(> (file-write-date (truename cname)) (file-write-date (truename lname))))
	    (compile-file cname :output-file lname)))
    (load lname)))


;;; --------------------------------
;;; CMU CL

#+cmu (declaim (optimize (extensions:inhibit-warnings 3))) 
#+cmu (setf extensions::*gc-verbose* nil)
#+cmu (setf *compile-print* nil)
#+cmu (setf *compile-verbose* nil)

#+cmu (defun compile-and-load (name)
	(let* ((dir clm-directory)
	       (bindir clm-bin-directory)
	       (cname (concatenate 'string dir name ".lisp"))
	       (lname (concatenate 'string bindir name "." (c:backend-fasl-file-type c:*backend*))))
	  (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	  (if (probe-file cname)
	      (if (or (not (probe-file lname))
		      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		  (compile-file cname :output-file lname)))
	  (load lname)))


;;; --------------------------------
;;; Steel Bank CL

#+sbcl (setf *compile-print* nil)
#+sbcl (setf *compile-verbose* nil)
#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

#+sbcl (defun compile-and-load (name)
	(let* ((dir clm-directory)
	       (bindir clm-bin-directory)
	       (cname (concatenate 'string dir name ".lisp"))
	       (lname (concatenate 'string bindir name ".fasl")))
	  (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	  (if (probe-file cname)
	      (if (or (not (probe-file lname))
		      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		  (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
				 (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		    (format t "~%;compiling ~A" cname)
		    (force-output)
		    (compile-file cname :output-file lname))))
	  (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
	    (format t "~%;loading ~A" lname)
	    (load lname)
	    (force-output)
	    )))


;;; --------------------------------
;;; Lispworks

#+lispworks(setf *compile-print* nil)
#+lispworks(setf *compile-verbose* nil)
#+lispworks(declaim (muffle-warning compiler-note))

#+lispworks(defun compile-and-load (name)
	     (let* ((dir clm-directory)
		    (bindir clm-bin-directory)
		    (cname (concatenate 'string dir name ".lisp"))
		    (lname (concatenate 'string bindir name "." (pathname-type (cl-user::compile-file-pathname "")))))
	       (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	       (if (probe-file cname)
		   (if (or (not (probe-file lname))
			   (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		       (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
				      (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
			 (format t "~%;compiling ~A" cname)
			 (force-output)
			 (compile-file cname :output-file lname))))
	       (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		 (format t "~%;loading ~A" lname)
		 (load lname)
		 (force-output)
		 )))


;;; --------------------------------
;;; Clisp

#+clisp (defun compile-and-load (name)
	  (handler-bind
	      ((t #'(lambda (c)
		      (when (find-restart 'continue)
			(invoke-restart 'continue)))))
	    (let* ((dir clm-directory)
		 (bindir clm-bin-directory)
		 (cname (concatenate 'string dir name ".lisp"))
		 (lname (concatenate 'string bindir name ".fas")))
	    (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	    (if (probe-file cname)
		(if (or (not (probe-file lname))
			(> (file-write-date (truename cname)) (file-write-date (truename lname))))
		    (compile-file cname :output-file lname)))
	    (load lname))))
	  

;;; --------------------------------
;;; compile the .c files and build libraries, where applicable

(defvar someone-compiled nil)
(setf someone-compiled nil)
(defvar someone-loaded nil)
(setf someone-loaded nil)

(defvar c-compiler "cc") ; shouldn't this be *cc*? 

(defun cc-it (n) 
  (let* ((lname (concatenate 'string clm-bin-directory n *obj-ext*))
	 (cname (concatenate 'string clm-directory n ".c"))
	 (cc-name (or #+(and excl (not windoze)) (sys:getenv "CC")
		      #+lucid (lcl:environment-variable "CC")
		      #+cmu (cdr (assoc (intern "CC" "KEYWORD") ext:*environment-list*))
		      #+sbcl "gcc"
		      #+clisp "gcc"
		      #+lispworks "gcc"
		      #-(or sun hpux windoze) "cc"
		      #+(or sun hpux) "gcc"
		      #+windoze "cl"
		      "gcc"))
	 (cstr (concatenate 'string 
			    cc-name " "
			    cname
			    " -c"
			    *cflags*
			    #-windoze " -o " #+windoze " -Fo"
			    lname
			    )))
    (setf c-compiler cc-name)
    (when (or (not (probe-file lname))
	      (not (probe-file cname))
	      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
      (princ (format nil "; Compiling ~S~%" cname)) (force-output)
      #+debug (princ (format nil "; ~A~%" cstr)) (force-output)
      (setf someone-compiled t)
      #+excl (excl:shell cstr)
      #+lispworks (sys::run-shell-command cstr)
      #+clisp (ext:shell cstr)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" cstr) :output t)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" cstr) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" cstr) :output t)
      )))

(cc-it "xen")
(cc-it "io")
(cc-it "headers")
(cc-it "audio")
(cc-it "sound")
(cc-it "clm")
(cc-it "vct")
(cc-it "cmus")

#+(or cmu sbcl excl openmcl clisp lispworks)
(let ((shared-name (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*)))
  (when (or someone-compiled (not (probe-file shared-name)))
    (princ (format nil "; Creating ~S~%" shared-name))
    (setf someone-loaded t)
    (let ((str (concatenate 
		'string
		*ld* " "
		#-(and cmu freebsd) *csoflags* " "
		#+(and cmu freebsd) "-r -L/usr/lib "
		#+(or (and clisp mac-osx) (and sbcl darwin) (and openmcl (not linux-target) (not linuxppc-target))) "-dynamiclib "
		#-windoze "-o " #+windoze "-Fe"
		shared-name " "
		clm-bin-directory "xen" *obj-ext* " "
		clm-bin-directory "headers" *obj-ext* " "
		clm-bin-directory "audio" *obj-ext* " "
		clm-bin-directory "io" *obj-ext* " "
		clm-bin-directory "sound" *obj-ext* " "
		clm-bin-directory "clm" *obj-ext* " "
		clm-bin-directory "vct" *obj-ext* " "
		clm-bin-directory "cmus" *obj-ext* " "
		#+windoze clm-bin-directory #+windoze "libclm.def "

		#+sgi "-laudio "
		#+alsa "-lasound "
		#+jack "-ljack -lsamplerate -lasound "
		;#+sb-thread "-lpthread "

		;; try to find the ACL shared library that has acl_printf and call_lisp_address
		;#+(and sgi allegro-v5.0) (concatenate 'string (namestring (truename "sys:")) "libacl5.0.so ")
		
		;#+(and (or allegro-v5.0 allegro-v5.0.1 allegro-v6.0) (not (or windoze linux)))
		#+(and acl-50 (or sun (not acl-70)) (not (or windoze linux)))
		(concatenate 'string (namestring (translate-logical-pathname "sys:")) (excl::get-shared-library-name) " ")

		;; cl-lite version of get-shared-library-name returns a name that doesn't exist
		;;   and we can't include it anyway because of appalling Windoze file name stupidity
		;;   windoze users are on their own here -- fixup the &#@%$^# name below.
		#+(and windoze (not allegro-cl-lite) (not acl-62) (not acl-70)) clm-bin-directory
		#+(and windoze (not allegro-cl-lite) (not acl-60) (not acl-61) (not acl-62)) "acl503.lib "
		#+(and windoze (not allegro-cl-lite) acl-60 (not acl-61) (not acl-62)) "acl601.lib "
		#+(and windoze (not allegro-cl-lite) acl-62 (not acl-70)) (concatenate 'string "\"" (namestring (truename "sys:")) "acli623.lib\"")
		#+(and windoze (not allegro-cl-lite) acl-70 (not acl-80)) (concatenate 'string "\"" (namestring (truename "sys:")) "acl701.lib\"")

                #+(and windoze (not allegro-cl-professional) acl-80) (concatenate 'string "\"" (namestring (truename "sys:")) "acli8010t.lib\"") 
                #+(and windoze allegro-cl-professional acl-80) (concatenate 'string "\"" (namestring (truename "sys:")) "acli8010.lib\"") 
		
		#+windoze " user32.lib gdi32.lib kernel32.lib comctl32.lib comdlg32.lib winmm.lib advapi32.lib "
		
		#-(or windoze (and openmcl linuxppc-target) (and linux (or cmu sbcl acl-50))) " -lm -lc"
		;;; in linux this -lc can cause "ld: internal error ldlang.c 3088"
		#+(or macosx mac-osx) " -framework CoreAudio "
		#+(and sun (not x86-64)) " -lgcc"
		#+(and clozure darwinx8632-target) " -arch i386"
		)))
      (format t ";;~A~%" str)
      (force-output)
      #+excl (excl:shell str)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" str) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" str) :output t)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" str) :output t)
      #+clisp (ext::shell str)
      #+lispworks (sys::run-shell-command str)

      (if (not (probe-file shared-name))
	  (format t "~S was not created?  Perhaps there was a C compiler or loader error.~%~
                     You might try the following command in a terminal to see what happened: ~S~%"
		  shared-name
		  str))
      ;; gcc creates a library file even if some error occurs!  So, this check won't usually help.
      )))

    

;;; --------------------------------
;;; code walker, package definitions, etc

#-(or excl sbcl)
  (if (not (find-package :walker))
      (progn
	(compile-and-load "walk")
	;; now check for later versions that do not automatically expand macros during the walk (taken from Common Music's build.lisp)
	(when (find-symbol "WALK-FORM-EXPAND-MACROS-P" :walker)
	  (set (find-symbol "WALK-FORM-EXPAND-MACROS-P" :walker) t))
	#+(and (or clisp cmu) ansi-cl) (load (concatenate 'string clm-directory "special-form-for-cmu.cl"))
	))

#+cmu (setf walker:walk-form-expand-macros-p t)
;;; in sbcl it's in the SB-WALKER package (&#%@^$)

(compile-and-load "clm-package")
(compile-and-load "initmus")

(setf clm::*clm-binary-directory* (namestring (truename clm-bin-directory)))
(setf clm::*clm-source-directory* (namestring (truename clm-directory)))

(setf clm::*clm-compiler-name* c-compiler)
#+(or excl cmu sbcl openmcl clisp lispworks) (setf clm::*so-ext* *shared-object-extension*)

#+excl (setf clm::*use-chcon* use-chcon)
#+excl (if use-chcon (excl:shell (format nil "chcon -t textrel_shlib_t ~A" (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))))
#+excl (load (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+lispworks (fli:register-module (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+cmu (ext:load-foreign (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+sbcl (sb-alien:load-shared-object (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+(and clozure darwin)
;;; workaround for a nasty Mac OS X 10.6 "misfeature"
;;; the CoreFoundation libary is loaded indirectly when libclm.dylib is loaded
;;; 10.6 disallows loading CoreFoundation outside the main thread
;;; Clozure CL uses the main thread for housekeeping and spawns an additional listener thread
;;; so we must force CoreFoundation to load from the main thread
;;; see http://www.clozure.com/pipermail/openmcl-devel/2010-February/011118.html
(let ((core-foundation "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
  (when (not (ccl::shared-library-with-name core-foundation))
	(let* ((s (make-semaphore)))
	  (process-interrupt
	   ccl::*initial-process*
	   (lambda ()
		 (open-shared-library core-foundation)
		 (ccl:signal-semaphore s)))
	  (ccl:wait-on-semaphore s))))

#+openmcl (ccl:open-shared-library (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))

#+openmcl (compile-and-load "mcl-doubles")

(compile-and-load "sndlib2clm")
(compile-and-load "defaults")


;;; -------- sndplay
;;; make a program named sndplay (and sndinfo and audinfo?) that can play most sound files 
#+windoze
  (when (or (not (probe-file "sndplay.exe"))
	    (> (file-write-date "sndplay.c") (file-write-date "sndplay.exe")))
    #+excl (excl:shell (format nil "cl sndplay.c -I. -DMUS_WINDOZE libclm.lib~%"))
    )

#-windoze
  (let ((prog-name (concatenate 'string clm::*clm-binary-directory* "sndplay"))
	(source-name (concatenate 'string clm::*clm-source-directory* "sndplay.c")))
    (when (or (not (probe-file prog-name))
	      (> (file-write-date source-name) (file-write-date prog-name)))
      (let ((dacstr (concatenate 'string
				 c-compiler
				 *cflags*
				 " "
				 clm::*clm-binary-directory* "xen.o "
				 clm::*clm-binary-directory* "headers.o "
				 clm::*clm-binary-directory* "audio.o "
				 clm::*clm-binary-directory* "io.o "
				 clm::*clm-binary-directory* "sound.o "
				 clm::*clm-binary-directory* "clm.o "
				 clm::*clm-binary-directory* "vct.o "
				 source-name
				 " -o "
				 prog-name
				 #+sgi " -laudio"
				 #+alsa " -lasound"
				 #+jack " -ljack -lsamplerate -lasound"
				 ;#+sb-thread " -lpthread"
				 #+(or macosx mac-osx) " -framework CoreAudio"
				 " -lm"
				 )))
	(princ (format nil "; Building sndplay program: ~S~%" prog-name)) (force-output)
	#+excl (excl:shell dacstr)
	#+cmu (extensions:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+lispworks (sys::run-shell-command dacstr)
	#+openmcl (ccl:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+clisp (ext::shell dacstr)
	)))


;;; --------------------------------
;;; CLM proper

(compile-and-load "ffi")

#+(and (not (or big-endian little-endian)) clisp)
  (if (= (clm::clm-little-endian) 1)
      (pushnew :little-endian *features*)
    (pushnew :big-endian *features*))
;; too late for defaults.lisp but what can I do?

(compile-and-load "mus")
(compile-and-load "run")
(compile-and-load "sound")
(compile-and-load "defins")
(compile-and-load "env")
(compile-and-load "export")
(compile-and-load "clm1") ; added 15-Apr-02

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
