(in-package :ccl)

#+openmcl
(defmacro with-foreign-double-float-array ((ptr-var lisp-array) &body body)
  (let* ((length (gensym)) 
	 (size (gensym)))
   `(let* ((,length (length ,lisp-array))
           (,size (* ,length 8)))
     (%stack-block ((,ptr-var ,size))
       (dotimes (i ,length)
         (setf (%get-double-float ,ptr-var (* i 8)) (aref ,lisp-array i)))
       ,@body))))

#+openmcl
(defmacro with-foreign-double-float-array-to-lisp ((ptr-var lisp-array) &body body)
  (let* ((length (gensym)) 
	 (size (gensym))
	 (val (gensym)))
   `(let* ((,length (length ,lisp-array))
           (,size (* ,length 8)))
      (%stack-block ((,ptr-var ,size))
	(let ((,val (progn ,@body)))
	  (dotimes (i ,length)
	    (setf (aref ,lisp-array i) (%get-double-float ,ptr-var (* i 8))))
	  ,val)))))

#+openmcl
(defmacro with-foreign-double-float-array-to-c-and-lisp ((ptr-var lisp-array) &body body)
  (let* ((length (gensym)) 
	 (size (gensym))
	 (val (gensym)))
   `(let* ((,length (length ,lisp-array))
           (,size (* ,length 8)))
      (%stack-block ((,ptr-var ,size))
        (dotimes (i ,length)
	  (setf (%get-double-float ,ptr-var (* i 8)) (aref ,lisp-array i)))
	(let ((,val (progn ,@body)))
	  (dotimes (i ,length)
	    (setf (aref ,lisp-array i) (%get-double-float ,ptr-var (* i 8))))
	  ,val)))))

		

