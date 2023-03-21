(in-package :clm)
(export '(clm-seek-bytes clm-seek-floats
	  clm-read-floats clm-write-floats
	  clm-read-ints clm-write-ints
	  c-open-input-file c-open-output-file c-create-file c-close c-seek
	  clm-swap-ints clm-swap-doubles
	  ))

;; see ug3.ins and ffi-test.lisp for examples
#+clisp (ffi:default-foreign-language :stdc)

;;; ---------------- lseek ----------------

#+excl (ff:def-foreign-call (c-seek "lseek") ((fd :int) (loc :int) (type :int)) :returning :void)
#+cmu (def-alien-routine ("lseek" c-seek) c-call:void (fd c-call:int) (loc c-call:int) (type c-call:int))
#+sbcl (sb-alien:define-alien-routine ("lseek" c-seek) sb-alien:void (fd sb-alien:int) (loc sb-alien:int) (type sb-alien:int))
#+openmcl (defun c-seek (fd loc type) (ccl::external-call (clm_ffi_name "lseek") :signed fd :signed loc :signed type :void))
#+clisp (ffi:def-call-out c-seek (:name "clm_clisp_lseek") (:library (full-lib-name)) (:return-type nil) 
			  (:arguments (fd ffi:int) (loc ffi:int) (type ffi:int)))
#+lispworks (fli:define-foreign-function (c-seek "lseek") ((fd :int) (loc :int) (type :int)) :result-type nil)

#|
;;; ---------------- read ----------------
#+excl (ff:def-foreign-call (c-read "read") ((fd :int) (buf (* :char) (simple-array (unsigned-byte 8) (*))) (bytes :int)) :returning :int)
;;; ---------------- write ----------------
#+excl (ff:def-foreign-call (c-write "write") ((fd :int) (buf (* :char) (simple-array (unsigned-byte 8) (*))) (bytes :int)) :returning :int)
|#

;;; ---------------- c-seek-bytes ----------------

(defmacro <clm-seek-bytes> (fd n)
  (format *c-file* "  lseek(~A, ~A, SEEK_SET);~80,1T/* (clm-seek-bytes ~A ~A) */~%"
	  (lc-num-ref fd :integer)
	  (lc-num-ref n :integer)
	  (clean-arg fd)
	  (clean-arg n))
  nil)

(defun clm-seek-bytes (fd n) (c-seek fd n 0)) ; SEEK_SET=0

(def-clm-fun 'clm-seek-bytes #'(lambda (var x) (declare (ignore var)) (package-op '<clm-seek-bytes> nil x :clm-integer)))


;;; ---------------- c-seek-floats ----------------

(defmacro <clm-seek-floats> (fd n)
  (format *c-file* "  lseek(~A, ~A * sizeof(double), SEEK_SET);~80,1T/* (clm-seek-floats ~A ~A) */~%"
	  (lc-num-ref fd :integer)
	  (lc-num-ref n :integer)
	  (clean-arg fd)
	  (clean-arg n))
  nil)

(defun clm-seek-floats (fd n) (c-seek fd (* n 8) 0)) ; SEEK_SET=0, 8=sizeof(double)

(def-clm-fun 'clm-seek-floats #'(lambda (var x) (declare (ignore var)) (package-op '<clm-seek-floats> nil x :clm-integer)))


;;;---------------- clm-read-bytes ----------------




;;; ---------------- clm-read-floats ----------------

(defmacro <clm-read-floats> (result fd arr n)
  (format *c-file* "  ~A = read(~A, (char *)(~A), ~A * sizeof(double)) / sizeof(double);~%"
	  (lc (second result))
	  (lc-num-ref fd :integer)
	  (lc-arr-ref arr)
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-read-floats #'(lambda (var x) (package-op '<clm-read-floats> var x :clm-integer)))

#+excl (ff:def-foreign-call (c-read-floats "read") ((fd :int) (buf (* :double)) (bytes :int)) :returning :int)
#+cmu (def-alien-routine ("read" c-read-floats-1) c-call:int (fd c-call:int) (buf (* double-float)) (bytes c-call:int))
#+cmu (defun c-read-floats (fd arr n) (c-read-floats-1 fd (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("read" c-read-floats-1) sb-alien:int (fd sb-alien:int) (buf (* double-float)) (bytes sb-alien:int))
#+sbcl (defun c-read-floats (fd arr n) (c-read-floats-1 fd (array-data-address arr) n))
#+openmcl (defun c-read-floats (fd arr n)
	    (ccl::with-foreign-double-float-array-to-c-and-lisp (p arr)
	      (ccl::external-call (clm_ffi_name "read") :signed fd :address p :signed n :signed)))

#+clisp (ffi:def-call-out clm-clisp-doubles-init (:name "clm_clisp_doubles_init") (:library (full-lib-name)) (:return-type ffi:int)
			  (:arguments (fd ffi:int) (n ffi:int)))
#+clisp (ffi:def-call-out clm-clisp-double (:name "clm_clisp_double") (:library (full-lib-name)) (:return-type ffi:double-float)
			  (:arguments (i ffi:int)))

#+clisp (defun clm-read-floats (fd buf n)
	  (clm-clisp-doubles-init fd n)
	  (do ((i 0 (1+ i)))
	      ((= i n))
	    (setf (aref buf i) (clm-clisp-double i)))
	  n)

#+lispworks (fli:define-foreign-function (c-read-floats "read") ((fd :int) (buf :pointer) (bytes :int)) :result-type :int)

#-clisp (defun clm-read-floats (fd arr n) (/ (c-read-floats fd arr (* n 8)) 8))


;;; ---------------- clm-read-ints ----------------

(defmacro <clm-read-ints> (result fd arr n)
  (format *c-file* "  ~A = read(~A, (char *)(~A), ~A * sizeof(int)) / sizeof(int);~%"
	  (lc (second result))
	  (lc-num-ref fd :integer)
	  (lc-arr-ref arr)
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-read-ints #'(lambda (var x) (package-op '<clm-read-ints> var x :clm-integer)))

#+excl (ff:def-foreign-call (c-read-ints "read") ((fd :int) (buf (* :int)) (bytes :int)) :returning :int)
#+cmu (def-alien-routine ("read" c-read-ints-1) c-call:int (fd c-call:int) (buf (* c-call:int)) (bytes c-call:int))
#+cmu (defun c-read-ints (fd arr n) (c-read-ints-1 fd (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("read" c-read-ints-1) sb-alien:int (fd sb-alien:int) (buf (* sb-alien:int)) (bytes sb-alien:int))
#+sbcl (defun c-read-ints (fd arr n) (c-read-ints-1 fd (array-data-address arr) n))
#+openmcl (defun c-read-ints (fd arr n) (ccl::external-call (clm_ffi_name "read") :signed fd :address (heap-int* arr) :signed n :signed))

#+clisp (ffi:def-call-out clm-clisp-ints-init (:name "clm_clisp_ints_init") (:library (full-lib-name)) (:return-type ffi:int)
			  (:arguments (fd ffi:int) (n ffi:int)))
#+clisp (ffi:def-call-out clm-clisp-int (:name "clm_clisp_int") (:library (full-lib-name)) (:return-type ffi:int)
			  (:arguments (i ffi:int)))

#+clisp (defun clm-read-ints (fd buf n)
	  (clm-clisp-ints-init fd n)
	  (do ((i 0 (1+ i)))
	      ((= i n))
	    (setf (aref buf i) (clm-clisp-int i)))
	  n)
#+lispworks (fli:define-foreign-function (c-read-ints "read") ((fd :int) (buf (:pointer :int)) (bytes :int)) :result-type :int)

#-clisp (defun clm-read-ints (fd arr n) (/ (c-read-ints fd arr (* n 4)) 4))


;;;---------------- clm-write-bytes ----------------

;;; ---------------- clm-write-floats ----------------

(defmacro <clm-write-floats> (result fd arr n)
  (format *c-file* "  ~A = write(~A, (char *)(~A), ~A * sizeof(double)) / sizeof(double);~%"
	  (lc (second result))
	  (lc-num-ref fd :integer)
	  (lc-arr-ref arr) 
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-write-floats #'(lambda (var x) (package-op '<clm-write-floats> var x :clm-integer)))

#+excl (ff:def-foreign-call (c-write-floats "write") ((fd :int) (buf (* :double)) (bytes :int)) :returning :int)
#+cmu (def-alien-routine ("write" c-write-floats-1) c-call:int (fd c-call:int) (buf (* double-float)) (bytes c-call:int))
#+cmu (defun c-write-floats (fd arr n) (c-write-floats-1 fd (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("write" c-write-floats-1) sb-alien:int (fd sb-alien:int) (buf (* double-float)) (bytes sb-alien:int))
#+sbcl (defun c-write-floats (fd arr n) (c-write-floats-1 fd (array-data-address arr) n))
#+openmcl (defun c-write-floats (fd arr n)
	    (ccl::with-foreign-double-float-array-to-c-and-lisp (p arr)
	      (ccl::external-call (clm_ffi_name "write") :signed fd :address p :signed n :signed)))
#+clisp (ffi:def-call-out c-write-floats (:name "clm_clisp_write_floats") (:library (full-lib-name)) (:return-type ffi:int) 
			  (:arguments (fd ffi:int) (buf (ffi:c-array-ptr ffi:double-float)) (n ffi:int)))
#+lispworks (fli:define-foreign-function (c-write-floats "write") ((fd :int) (buf :lisp-array) (bytes :int)) :result-type :int)


(defun clm-write-floats (fd arr n) (/ (c-write-floats fd arr (* n 8)) 8))


;;; ---------------- clm-write-ints ----------------

(defmacro <clm-write-ints> (result fd arr n)
  (format *c-file* "  ~A = write(~A, (char *)(~A), ~A * sizeof(int)) / sizeof(int);~%"
	  (lc (second result))
	  (lc-num-ref fd :integer)
	  (lc-arr-ref arr)
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-write-ints #'(lambda (var x) (package-op '<clm-write-ints> var x :clm-integer)))

#+excl (ff:def-foreign-call (c-write-ints "write") ((fd :int) (buf (* :int)) (bytes :int)) :returning :int)
#+cmu (def-alien-routine ("write" c-write-ints-1) c-call:int (fd c-call:int) (buf (* c-call:int)) (bytes c-call:int))
#+cmu (defun c-write-ints (fd arr n) (c-write-ints-1 fd (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("write" c-write-ints-1) sb-alien:int (fd sb-alien:int) (buf (* sb-alien:int)) (bytes sb-alien:int))
#+sbcl (defun c-write-ints (fd arr n) (c-write-ints-1 fd (array-data-address arr) n))
#+openmcl (defun c-write-ints (fd arr n) (ccl::external-call (clm_ffi_name "write") :signed fd :address (heap-int* arr) :signed n :signed))
#+clisp (ffi:def-call-out c-write-ints (:name "clm_clisp_write_ints") (:library (full-lib-name)) (:return-type ffi:int) 
			  (:arguments (fd ffi:int) (buf (ffi:c-array-ptr ffi:int)) (n ffi:int)))
#+lispworks (fli:define-foreign-function (c-write-ints "write") ((fd :int) (buf (:pointer :int)) (bytes :int)) :result-type :int)
(defun clm-write-ints (fd arr n) (/ (c-write-ints fd arr (* n 4)) 4))


;;; ---------------- c-close ----------------

(defmacro <c-close> (arg)
  (format *c-file* "  close(~A);~80,1T/* (c-close ~A) */~%"
	  (lc-num-ref arg :integer)
	  (clean-arg arg))
  nil)

#+excl (ff:def-foreign-call (c-close "close") ((fd :int)) :returning :void)
#+cmu (def-alien-routine ("close" c-close) c-call:void (fd c-call:int))
#+sbcl (sb-alien:define-alien-routine ("close" c-close) sb-alien:void (fd sb-alien:int))
#+openmcl (defun c-close (fd) (ccl::external-call (clm_ffi_name "close") :signed fd :void))
#+clisp (ffi:def-call-out c-close (:name "clm_clisp_close") (:library (full-lib-name)) (:return-type nil) (:arguments (fd ffi:int)))
#+lispworks (fli:define-foreign-function (c-close "close") ((fd :int)) :result-type :void)

(def-clm-fun 'c-close #'(lambda (var x) (declare (ignore x)) (package-op '<c-close> var nil :clm-integer)))



;;; ---------------- c-open-input-file ----------------

(defmacro <c-open-input-file> (result arg)
  (if (stringp arg)
      (format *c-file* "  ~A = mus_file_open_read(\"~A\");~80,1T/* (c-open-input-file ~A) */~%"
	      (lc (second result))
	      arg arg)
    (format *c-file* "  ~A = mus_file_open_read(~A);~80,1T/* (c-open-input-file ~A) */~%"
	    (lc (second result))
	    (lc (second arg))
	    (clean-arg arg)))
  nil)

#+excl (ff:def-foreign-call (c-open-input-file "mus_file_open_read")
			    ((arg (* :char) string))
			    #+acl-61 :strings-convert #+acl-61 t 
			    :returning :int)
#+cmu (def-alien-routine ("mus_file_open_read" c-open-input-file) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_file_open_read" c-open-input-file) sb-alien:int (arg sb-alien:c-string))
#+openmcl (defun c-open-input-file (file)
	    (ccl:with-cstrs ((f file))
	      (ccl::external-call (clm_ffi_name "mus_file_open_read") :address f :signed)))
#+clisp (ffi:def-call-out c-open-input-file (:name "mus_file_open_read") (:return-type ffi:int)
			  (:library (full-lib-name)) (:arguments (arg ffi:c-string)))
#+lispworks (fli:define-foreign-function (c-open-input-file "mus_file_open_read") ((arg (:reference-pass :ef-mb-string))) :result-type :int)

(def-clm-fun 'c-open-input-file #'(lambda (var x) (package-op '<c-open-input-file> var x :clm-integer)))


;;; ---------------- c-open-output-file ----------------

(defmacro <c-open-output-file> (result arg)
  ;; opens existing file read-alter and sets to write at end, I think -- does not delete existing file
  (if (stringp arg)
      (format *c-file* "  ~A = mus_file_open_write(\"~A\");~80,1T/* (c-open-output-file ~A) */~%"
	      (lc (second result))
	      arg arg)
    (format *c-file* "  ~A = mus_file_open_write(~A);~80,1T/* (c-open-output-file ~A) */~%"
	    (lc (second result))
	    (lc (second arg))
	    (clean-arg arg)))
  nil)

#+excl (ff:def-foreign-call (c-open-output-file "mus_file_open_write")
			    ((arg (* :char) string))
			    #+acl-61 :strings-convert #+acl-61 t 
			    :returning :int)
#+cmu (def-alien-routine ("mus_file_open_write" c-open-output-file) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_file_open_write" c-open-output-file) sb-alien:int (arg sb-alien:c-string))
#+openmcl (defun c-open-output-file (file)
	    (ccl:with-cstrs ((f file))
	      (ccl::external-call (clm_ffi_name "mus_file_open_write") :address f :signed)))
#+clisp (ffi:def-call-out c-open-output-file (:name "mus_file_open_write") (:return-type ffi:int)
			  (:library (full-lib-name)) (:arguments (arg ffi:c-string)))
#+lispworks (fli:define-foreign-function (c-open-output-file "mus_file_open_write") ((arg (:reference-pass :ef-mb-string))) :result-type :int)


(def-clm-fun 'c-open-output-file #'(lambda (var x) (package-op '<c-open-output-file> var x :clm-integer)))


;;; ---------------- c-create-file ----------------

(defmacro <c-create-file> (result arg)
  (if (stringp arg)
      (format *c-file* "  ~A = mus_file_create(\"~A\");~80,1T/* (c-create-file ~A) */~%"
	      (lc (second result))
	      arg arg)
    (format *c-file* "  ~A = mus_file_create(~A);~80,1T/* (c-create-file ~A) */~%"
	    (lc (second result))
	    (lc (second arg))
	    (clean-arg arg)))
  nil)

#+excl (ff:def-foreign-call (c-create-file "mus_file_create")
			    ((arg (* :char) string))
			    #+acl-61 :strings-convert #+acl-61 t 
			    :returning :int)
#+cmu (def-alien-routine ("mus_file_create" c-create-file) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_file_create" c-create-file) sb-alien:int (arg sb-alien:c-string))
#+openmcl (defun c-create-file (file)
	    (ccl:with-cstrs ((f file))
	      (ccl::external-call (clm_ffi_name "mus_file_create") :address f :signed)))
#+clisp (ffi:def-call-out c-create-file (:name "mus_file_create") (:library (full-lib-name)) (:return-type ffi:int)
			  (:arguments (arg ffi:c-string)))
#+lispworks (fli:define-foreign-function (c-create-file "mus_file_create") ((arg (:reference-pass :ef-mb-string))) :result-type :int)

(def-clm-fun 'c-create-file #'(lambda (var x) (package-op '<c-create-file> var x :clm-integer)))


#|
(definstrument write-floats ()
  (let ((file (c-create-file "test.data"))
	(arr (make-array 16 :initial-element 0.0)))
    (run
     (loop for i from 0 to 10 do
       (setf (aref arr i) (* i .1))
       (if (= i 9) (clm-write-floats file arr 10))))
    (c-close file)))

(definstrument read-floats ()
  (let ((file (c-open-input-file "test.data"))
	(arr (make-array 16 :initial-element 0.0)))
    (run
     (loop for i from 0 to 10 do
       (if (= i 0) (clm-read-floats file arr 10))      
       (clm-print "arr[~D] = ~F~%" i (aref arr i))))
    (c-close file)))
|#


;;; ---------------- clm-swap-ints ----------------

(defmacro <clm-swap-ints> (arr n)
  (format *c-file* "  swap_int_array((char *)(~A), ~A * 4);~%"
	  (lc-arr-ref arr)
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-swap-ints #'(lambda (var x) (declare (ignore var)) (package-op '<clm-swap-ints> nil x :clm-integer)))

#+excl (ff:def-foreign-call (clm-swap-ints "swap_int_array") ((buf (* :int)) (bytes :int)) :returning :void)
#+cmu (def-alien-routine ("swap_int_array" clm-swap-ints-1) c-call:void (buf (* c-call:int)) (bytes c-call:int))
#+cmu (defun clm-swap-ints (arr n) (clm-swap-ints-1 (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("swap_int_array" clm-swap-ints-1) sb-alien:void (buf (* sb-alien:int)) (bytes sb-alien:int))
#+sbcl (defun clm-swap-ints (arr n) (clm-swap-ints-1 (array-data-address arr) n))
#+openmcl (defun clm-swap-ints (arr n) (ccl::external-call (clm_ffi_name "swap_int_array") :address (heap-int* arr) :signed n :void))
#+clisp (ffi:def-call-out clm-swap-ints (:name "swap_int_array") (:library (full-lib-name)) (:return-type ffi:nil)
			  (:arguments (buf (ffi:c-array-ptr ffi:int)) (bytes ffi:int)))
#+lispworks (fli:define-foreign-function (clm-swap-ints "swap_int_array") ((buf (:pointer :int)) (bytes :int)) :result-type :void)

;;; ---------------- clm-swap-doubles ----------------

(defmacro <clm-swap-doubles> (arr n)
  (format *c-file* "  swap_double_array((char *)(~A), ~A * 8);~%"
	  (lc-arr-ref arr) 
	  (lc-num-ref n :integer))
  nil)

(def-clm-fun 'clm-swap-doubles #'(lambda (var x) (declare (ignore var)) (package-op '<clm-swap-doubles> nil x :clm-integer)))

#+excl (ff:def-foreign-call (clm-swap-doubles "swap_double_array") ((buf (* :double)) (bytes :int)) :returning :void)
#+cmu (def-alien-routine ("swap_double_array" clm-swap-doubles-1) c-call:void (buf (* double-float)) (bytes c-call:int))
#+cmu (defun clm-swap-doubles (arr n) (clm-swap-doubles-1 (array-data-address arr) n))
#+sbcl (sb-alien:define-alien-routine ("swap_double_array" clm-swap-doubles-1) sb-alien:void (buf (* double-float)) (bytes sb-alien:int))
#+sbcl (defun clm-swap-doubles (arr n) (clm-swap-doubles-1 (array-data-address arr) n))
#+openmcl (defun clm-swap-doubles (arr n)
	    (ccl::with-foreign-double-float-array-to-c-and-lisp (p arr)
   	      (ccl::external-call (clm_ffi_name "swap_double_array") :address p :signed n :void)))
;#+clisp (ffi:def-call-out clm-swap-doubles (:name "swap_double_array") (:library (full-lib-name)) (:return-type ffi:nil)
;			  (:arguments (buf (ffi:c-array-ptr ffi:double-float)) (bytes ffi:int)))
#+clisp (defun clm-swap-doubles (arr n) (declare (ignore arr n)) (warn "clm-swap-doubles doesn't work in clisp") -1)
#+lispworks (progn
	      (defun clm-swap-doubles (arr n)
		(fli:with-dynamic-lisp-array-pointer (p arr)
		  (clm-swap-doubles-1 p n)))
	      (fli:define-foreign-function (clm-swap-doubles-1 "swap_double_array") ((buf :pointer) (bytes :int)) :result-type :void))
