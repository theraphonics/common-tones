(in-package :common-tones)

;;; Apr-14 -- clm-5 (remove framples and mixers)

;;; Feb-08 -- clm-4 (new gens, etc)

;;; Jul-04 -- clm-3 (use clm.c)

;;; Mar-00 -- sndlib internal sample representation is now configurable, so clm has to reflect that.

;;; Jul-99 -- C side uses doubles throughout, Lisp side uses CLOS

;;; Jun-99 -- clm-2 name changes, run-time types -- everything changed!

;;; Jul-98 -- serious bug in readin forced me to change its layout, rippling through several other gens.

;;; Jun-98 -- _c names removed from cmus0.lisp, and Mac calloc/free replaced by NewPtr/DisposePtr

;;; Apr-98 -- many MCL changes

;;; Mar-98 -- higher precision clm_env causes layout changes

;;; Jul-97 -- no-rld linker support removed -- many prototype changes

;;; Oct-95 -- many changes for n-channel IO and run-block connections to src et al

;;; Jul-95 -- array allocation changed for SGI port

;;; Apr-95 -- boolean handling completely revised.

;;; Oct-94 -- parallel instrument scheduling option.

;;; Mar-94 -- full C output, rather than heavy use of foreign function interface

;;; Nov-93 -- array version

;;; Mar-93 -- struct version

;;; 1990   -- initial clm implementation from mus10/sambox


(defvar *clm-report-untyped-vars* nil)

(defconstant +setf+ 0)
(defconstant +incf+ 1)
(defconstant +decf+ 2)

(defconstant +as-needed-input+ 0)
(defconstant +as-needed-edit+ 1)
(defconstant +as-needed-analyze+ 2)
(defconstant +as-needed-synthesize+ 3)

(defvar new-prog nil)
(defvar loop-var nil)

(defmacro wf (x)
  `(walk-form ,x #+clisp walker:*toplevel-environment* #-clisp nil 'lisp->c))

(defmacro fully-expand (x)
  `(walk-form ,x #+clisp walker:*toplevel-environment* #-clisp nil #'(lambda (x y z) x)))

(defvar unhappy nil)
(defvar old-prog nil)
(defvar loop-beg nil)
(defvar loop-end nil)
(defvar loop-label-stack nil)
(defvar return-stack nil)
(defvar clm-functions nil)
(defvar vars nil)
(defvar function-ctr 0)
(defvar lambda-args nil)
(defvar *gen-type* nil)
(defvar *as-needed* nil)
(defvar *as-needed-functions* nil)
(defvar *as-needed-function-types* nil)
(defvar *global* nil)

(defstruct varinfo name (depth 0) (max-depth 1)
  refd parallel type iloc rloc initialize temp shadowed
  gen-type arr-gen-type
  global ctr loaded
  ref-chain)

(defun clm-function (x) (gethash x clm-functions))

(defun def-clm-fun (x y)
  (if (null clm-functions) (setf clm-functions (make-hash-table)))
  (setf (gethash x clm-functions) y))

(defmacro run (run-time-code)
  `(macroexpand-1 (run-1 ,run-time-code)))

(defvar outer-environment nil)

(defun make-var (var &optional type)
  (if (and var (not (eq var t)))
      (when (symbolp var)
	(if (null vars) (setf vars (make-hash-table)))
	(let ((lst (gethash var vars)))
	  (if (not lst)
	      (setf (gethash var vars)
		    (make-varinfo :name var :type type :temp t :global *global*)) ; if as-needed, we're declared globally
	    (if type
		(setf (varinfo-type lst) type))))))
  var)

(defun make-user-var (var &optional type)
  ;; (if type (format t "make-user-var ~A: ~A~%" var type))
  (let ((lv (and lambda-args (find var lambda-args :key #'first))))
    (if lv
	(list 'lambda-arg (second lv))
      (progn
	(if (null vars) (setf vars (make-hash-table)))
	(if (not (symbolp var)) (error "setf ~A?" var))
	(if (not (gethash var vars))
	    (setf (gethash var vars) (make-varinfo :name var :type type :initialize t :global *global*)))
	var))))

(defun make-local-var (var &optional parallel type)
  (let ((lst (gethash var vars)))
    (if (not lst)
	(setf (gethash var vars) (make-varinfo :name var :parallel parallel :type type :global *global*))
      (progn
	(incf (varinfo-depth lst))
	(setf (varinfo-shadowed lst) t)
	(if (>= (varinfo-depth lst) (varinfo-max-depth lst))
	    (setf (varinfo-max-depth lst) (1+ (varinfo-depth lst))))
	(setf (varinfo-parallel lst) parallel))))
  var)

(defun current-var (var &optional do-init)
  (if (and var (not (eq var t)))
      (if (symbolp var)
	  (let ((lst (gethash var vars)))
	    (if (not lst)
		(error "reference to undefined variable: ~A" var)
	      (if (< (varinfo-depth lst) 0)
		  (format t "reference to undefined variable ~A: ~A" var lst)
		(progn
		  (setf (varinfo-refd lst) t)

		  (when (and *global*
			     (not (varinfo-global lst))
			     (not (varinfo-temp lst)))
		    (setf (varinfo-global lst) *global*))

		  (if (and (varinfo-parallel lst) (not (eq do-init var)))
		      (progn
			(if (<= (varinfo-depth lst) 0) (warn "reference to undefined variable ~A" var))
			(list :var var (1- (varinfo-depth lst))))
		    (progn
		      (if (< (varinfo-depth lst) 0) (warn "reference to undefined variable ~A" var))
		      (list :var var (varinfo-depth lst))))))))
	var)
    var))

(defun provisional-var (var)
  (if (and var (not (eq var t)))
      (if (symbolp var)
	  (let ((lst (gethash var vars)))
	    (if (not lst)
		(error "reference to undefined variable: ~A" var)
	      (if (or (< (varinfo-depth lst) 0)
		      (and (varinfo-parallel lst) (= (varinfo-depth lst) 0)))
		  (format t "reference to undefined variable ~A: ~A" var lst)
		(list :var var (varinfo-depth lst)))))
	var)
    var))

(defun chained-var (var ref)
  (if (and var (not (eq var t)))
      (if (symbolp var)
	  (let ((lst (gethash var vars)))
	    (if (not lst)
		(error "reference to undefined variable: ~A" var)
	      (if (or (< (varinfo-depth lst) 0)
		      (and (varinfo-parallel lst)
			   (= (varinfo-depth lst) 0)))
		  (format t "reference to undefined variable ~A: ~A" var lst)
		(let ((info (gethash ref vars)))
		  (when info
		    (setf (varinfo-ref-chain lst) (cons info (varinfo-ref-chain lst))))
		  (list :var var (varinfo-depth lst))))))
	var)
    var))

(defun make-compiler-temp (&optional type) (make-var (gensym "_clm_") type))
(defun new-label (&optional name) (if name name (gensym "L-")))

(defmacro run-1 (run-time-code &environment env)
  ;;Lisp expands this twice unless we take other action (i.e. run and run-1 two-step)
  (setf new-prog nil
	unhappy nil
	loop-beg nil
	loop-end nil
	loop-var nil
	loop-label-stack nil
	vars nil
	function-ctr 0
	lambda-args nil
	return-stack nil
	*safety* *clm-safety*
	*debug* *clm-debug*
	*gen-type* nil
	*as-needed* nil
	*global* nil
	*as-needed-functions* nil
	*as-needed-function-types* nil
	outer-environment env)		;needed by macrolet?

  (wf run-time-code) ; builds new-prog

  (if *clm-debug*
      (progn
	(pprint (append (list 'progn) (reverse new-prog)))
	(terpri)
	(print-hash vars)
	(terpri)
	(pprint (append (list 'progn) (reverse *as-needed-functions*)))))

  (if unhappy				;we hit something we can't deal with
      run-time-code
    #-openmcl
    `(progn
       (<start> ,loop-end)
       ,@(nreverse new-prog)
       (<end-1>)
       ,@(or (nreverse *as-needed-functions*) '())
       (<end-2> ,(or loop-beg 0) ,(or loop-end 0) ,loop-end)
       )
    #+openmcl
    `(progn
       (<start> ,loop-end)
       ,@(let* ((lst (nreverse new-prog))
		(len (length lst)))
	   (loop for i from 0 by 50 below len
	     collect (append (list 'progn) (subseq lst i (min len (+ i 50))))))
       (<end-1>)
       ,@(or (nreverse *as-needed-functions*) '())
       (<end-2> ,(or loop-beg 0) ,(or loop-end 0) ,loop-end)
       )
    ))

(defun give-up (x)
  (setf unhappy t)
  (error "the run macro can't handle ~S in ~S" (if (listp x) (car x) x) x))

(defun arith-type (args &optional (restyp :clm-integer))
  ;; nil result type unpredictable, :integer known to be int result, :real known to be float
  (if (not args)
      restyp
    (let ((arg (car args)))
      (if (listp arg)
	  (let ((argv (gethash (second arg) vars)))
	    (if argv
		(if (member (varinfo-type argv) '(:integer :clm-integer :clm-boolean :boolean))
		    (arith-type (cdr args) restyp)
		  (if (member (varinfo-type argv) '(:real :clm-real))
		      :clm-real
		    (arith-type (cdr args) nil)))
	      (if (member arg '(pi *srate*))
		  :clm-real
		(arith-type (cdr args) nil))))
	(if (integerp arg)
	    (arith-type (cdr args) restyp)
	  :clm-real)))))

(defun package-op (clm-name var x &optional type-func spec provisional-result arg-func)
  (let* ((argnum 0)
	 (args (mapcar #'(lambda (arg)
			   (let ((val (current-var (wf arg))))
			     (setf *gen-type* nil)
			     (if arg-func
				 (progn
				   (funcall arg-func val argnum) ; gad CL doesn't have closures!
				   (incf argnum)))
			     val))
		       (cdr x))))
    (let ((result-type (if type-func
			   (if (member type-func '(:clm-integer :clm-real :clm-boolean :real :integer :boolean
						   :string :float-array :double-array :integer-array))
			       type-func
			     (funcall type-func args)))))
      (if clm-name
	  (if var
	      (if (or (not (numberp var)) (not (zerop var)))
		  (progn
		    (make-var var result-type)
		    (if provisional-result
			(if spec
			    (push `(,clm-name ,spec ,(provisional-var var) ,@args) new-prog)
			  (push `(,clm-name ,(provisional-var var) ,@args) new-prog))
		      (if spec
			  (push `(,clm-name ,spec ,(current-var var) ,@args) new-prog)
			(push `(,clm-name ,(current-var var) ,@args) new-prog))))
		(push `(,clm-name 0 ,@args) new-prog))
	    (if spec
		(push `(,clm-name ,spec ,@args) new-prog)
	      (push `(,clm-name ,@args) new-prog))))
      (if clm-name var args))))

(defun set-gen-type (gen gen-type)
  (when (and gen
	   (not (varinfo-temp gen)))
    (if (or (not (varinfo-gen-type gen))
	    (eq (varinfo-gen-type gen) :mus-any))
	(setf (varinfo-gen-type gen) gen-type)
      ;; var's gen-type must already be set
      (if (and (not (eq (varinfo-gen-type gen) gen-type))
	       (not (eq gen-type :mus-any)))
	  ;; var has several possible gen types, current type might be another
	  (if (symbolp (varinfo-gen-type gen))
	      (setf (varinfo-gen-type gen) (list (varinfo-gen-type gen) gen-type))
	    (if (not (member gen-type (varinfo-gen-type gen)))
		(setf (varinfo-gen-type gen) (append (varinfo-gen-type gen) (list gen-type)))))))))

(defun gen-package-op (clm-name var x &optional type-func gen-type arg-func)
  (setf *gen-type* gen-type)
  (let ((result (package-op clm-name var x type-func nil nil arg-func)))
    (setf *gen-type* nil)
    (let ((gen (gethash (cadr x) vars)))
      (if gen
	  (set-gen-type gen gen-type)))
    result))

(defun gen2-package-op (clm-name var x &optional type-func gen-type gen2-type)
  (setf *gen-type* gen-type)
  (let ((result (package-op clm-name var x type-func)))
    (setf *gen-type* nil)
    (let ((gen (gethash (cadr x) vars)))
      (set-gen-type gen gen-type))
    (let ((gen (gethash (caddr x) vars)))
      (set-gen-type gen gen2-type))
    result))

(defun bank-package-op (clm-name var x &optional type-func gen-type)
  (let ((result (package-op clm-name var x type-func)))
    ;(format t "look for ~A~%" (caddr x))
    (let ((gen (gethash (caddr x) vars)))
      (if gen
	  (progn
	    ;(format t "set ~A fields to ~A~%" gen gen-type)
	    (setf (varinfo-arr-gen-type gen) gen-type)
	    (setf (varinfo-gen-type gen) :mus-any-array))))
    result))

(defun find-loop-control-info (var x)
  (declare (ignore var))
  ;; We save the name of the loop control variable and check it against the OUTA or OUTB call, if any.
  ;;
  ;; 29-Mar-01
  ;;  this is no longer needed as an optimization for speed.  If the run macro sees
  ;;    (progn (loop for i fixnum from ...))
  ;;  the code it generates is just as fast as
  ;;    (loop for i from...)
  ;;
  (if (and (not loop-end)
	   (not new-prog)
	   (not (string-equal (cadr x) "WHILE"))
	   (not (string-equal (cadr x) "UNTIL")))
      (do* ((i 1 (1+ i))
	    (loop-e nil)
	    (loop-b 0)
	    (loop-v nil)
	    (all-done nil)
	    (val (nth i x) (nth i x)))
	  ((or all-done
	       (= i (length x)))
	   (if (and (not loop-e) (not loop-v))
	       (error "Can't find control variable or end point of outer loop")
	     (progn
	       ;;; (setf loop-end (or loop-e (list '+ loop-b 1234567890)))
	       (setf loop-end loop-e)
	       (setf loop-beg loop-b)
	       (setf loop-var loop-v)
	       (make-var loop-v)
	       `(progn ,@(nthcdr i x)))))
	(if (symbolp val)
	    (if (or (string-equal val "DO")
		    (string-equal val "AND"))
		(setf all-done (string-equal val "DO"))
	      (if (or (string-equal val "FOR")
		      (string-equal val "AS"))
		  (setf loop-v (nth (+ i 1) x))
		(if (or (string-equal val "FROM")
			(string-equal val "UPFROM")
			(string-equal val "="))
		    (setf loop-b (nth (+ i 1) x))
		  (if (or (string-equal val "TO")
			  (string-equal val "UPTO"))
		      (setf loop-e (nth (+ i 1) x))
		    (if (string-equal val "BELOW")
			(setf loop-e (list '- (nth (+ i 1) x) 1)))))))))
    (let ((last-var nil))
      ;; the problem here is that we might have nested loops within the outer loop, and all of
      ;; them have the identical label names LOOP::NEXT-LOOP and LOOP::END-LOOP.  It is only
      ;; at this level that we have block structure info, so we can't put off the label disambiguation
      ;; until later -- END-LOOPs are all at the end, but references within the I-code triples
      ;; to END-LOOP have no way of telling which END-LOOP.  So we expand the loop block, then
      ;; run through it, making sure all occurences of END-LOOP and NEXT-LOOP are fixed up so
      ;; no confusion can happen later.  The next problem is that in ACL 4.1, which has loop built-in,
      ;; these labels have different names (EXCL::END-LOOP).
      (push (gentemp "L_") loop-label-stack)
      (setf last-var (wf (macroexpand x)))
      (pop loop-label-stack)
      (if last-var (make-var last-var)))))

(defun lisp->c (x y env)
  (declare (ignore y env))		;env is the local environment that the tree walker keeps track of
  (block sbcl-stupidity
    (if (and x (listp x))
	(if (eq (car x) :var)
	    x
	  (let ((var (make-compiler-temp)))
	    #+sbcl (if (and (symbolp (car x))
			    (string-equal "SRC" (symbol-name (car x))))  ; sbcl apparently exports SRC but it's in about 40 packages...
		       (return-from sbcl-stupidity (apply (clm-function 'COMMON-TONES::SRC) (list var x))))
	    (if (clm-function (car x))
		(apply (clm-function (car x)) (list var x))
	      (if (and (listp (car x)) (eq (caar x) 'lambda))
		  (lambda-branch var x)
		(let ((xx (macroexpand-1 x outer-environment))) ;else full expansion => trouble in struct field setfs (system::rplaca-nthcdr)
		  (if (eq x xx)
		      (give-up x)
		    xx))))))
      (if (symbolp x)
	  (if #-clisp (constantp x)               ;PI is not a constant in clisp!
	      #+clisp (or (constantp x) (eq x 'pi))
	      (if (eq x nil)
		  nil
		(if (eq x t)
		    t
		  (eval x)))
	    (make-user-var x))
	x))))

;;; return has to thread its way through block lists and get its value returned

;;; from the correct place, so it's not enough to just dump the value in A and

;;; jump to the end.  The return stack values is a list of lists, pushed anew

;;; as blocks are opened, and popped as they close.  The inner list is of the

;;; form (block-name block-variable block-end-label 0) -- the block-variable is

;;; what we return (either via return or by falling through the bottom), block-label

;;; is the location of the end of the block, and the block-name is either nil

;;; for an unnamed block or the name provided by the caller.


(defun return-branch (var x)		;return [result]
  (declare (ignore var))
  (if return-stack
      (let ((our-block-data (find nil return-stack :key #'first)))
	(if (not our-block-data)
	    (error "return not inside a block named NIL")
	  (let ((block-lab (third our-block-data))
		(block-var (second our-block-data)))
	    (incf (fourth our-block-data))
	    (push `(<comment> return ,(cadr x)) new-prog)
	    (if (cdr x)
		(let ((val (wf (cadr x))))
		  (push `(<setf> ,+setf+ ,(provisional-var block-var) ,(chained-var val block-var)) new-prog))
	      (push `(<setf> ,+setf+ ,(provisional-var block-var) nil) new-prog))
	    (push `(<jump> ,block-lab) new-prog)
	    nil)))
    (error "return outside any block")))

(defun return-from-branch (var x)
  (declare (ignore var))
  (if return-stack
      (let ((our-block-data (find (cadr x) return-stack :key #'first)))
	(if (not our-block-data)
	    (error "return-from ~S not inside the specified block" (cadr x))
	  (let ((block-lab (third our-block-data))
		(block-var (second our-block-data)))
	    (incf (fourth our-block-data))
	    (push `(<comment> return-from ,(cadr x)) new-prog)
	    (if (cddr x)
		(let ((val (wf (caddr x))))
		  (push `(<setf> ,+setf+ ,(provisional-var block-var) ,(chained-var val block-var)) new-prog))
	      (push `(<setf> ,+setf+ ,(provisional-var block-var) nil) new-prog))
	    (push `(<jump> ,block-lab) new-prog)
	    nil)))
    (error "return-from outside any block")))

(defun block-branch (var x)
  (let ((end-label (new-label))
	(last-var nil))
    (if (not new-prog) (push :block new-prog))
    (push (list (cadr x) (make-var var) end-label 0) return-stack)
    (loop for i in (cddr x) do (setf last-var (wf i)))
    (if (zerop (fourth (first return-stack)))
	(setf end-label nil))
    ;; zero=no return so block var need not be saved if we're at top level
    (push `(<comment> block ,(cadr x)) new-prog)
    (when last-var
      (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var last-var var)) new-prog))
    (if end-label (push `(<label> ,end-label) new-prog))
    (pop return-stack)
    var))

(defun tagbody-branch (var x)
  ;; need check for duplicate tag within one scope, and shadowing
  ;; 17-May-95 -- make sure tagbody returns nil
  (declare (ignore var))
  (let ((inner-labs nil))
    (if (not new-prog) (push :tagbody new-prog)) ; (see progn)
    (push `(<comment> tagbody) new-prog)
    (loop for i in (cdr x) do
      (if (listp i)
	  (wf i)
	(let ((lab (if (and loop-label-stack
			    #-(or openmcl cmu sbcl clisp (and excl cltl2)) (member i '(loop::next-loop loop::end-loop))
			    #+(and excl cltl2) (member i '(excl::next-loop excl::end-loop))
			    #+(or openmcl cmu) (member i '(ansi-loop::next-loop ansi-loop::end-loop))
			    #+sbcl (member i '(sb-loop::next-loop sb-loop::end-loop))
			    #+clisp (member i '(system::next-loop system::end-loop))
			    )
		       (intern (concatenate 'string (symbol-name i) "-" (symbol-name (first loop-label-stack))))
		     i)))
	  (push lab inner-labs)
	  (push `(<label> ,lab) new-prog))))
    nil))

(defun if-branch (var x)
  (let* ((flabel (new-label))
	 (false-too (cadddr x))
	 (end-label (new-label))
	 (ifvar nil))
    (make-var var)
    (push `(<comment> if ,(cadr x)) new-prog)
    (setf ifvar (wf (cadr x)))
    (make-var ifvar)

    (if (and ifvar (symbolp ifvar) (not (eq ifvar t)))
	(let ((lst (gethash ifvar vars)))
	  (if (not lst)
	      (format t "~A has no info?~%" ifvar))
	  (when (and lst
		     (not (varinfo-type lst))
		     (varinfo-temp lst))
	    (if *clm-debug* (format t "if set ~A to boolean~%" ifvar))
	    (setf (varinfo-type lst) :clm-boolean))))

    (push `(<jump-if-not> ,(current-var ifvar) ,flabel) new-prog)
    (if (caddr x)
	(let ((val (wf (caddr x))))
	  (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var val var)) new-prog)
	  (push `(<jump> ,end-label) new-prog)
	  (push `(<label> ,flabel) new-prog))
      ;; actually a malformed if statement if no caddr
      (progn
	(push `(<setf> ,+setf+ ,(provisional-var var) nil) new-prog)
	(push `(<jump> ,end-label) new-prog)
	(push `(<label> ,flabel) new-prog)))
    (if false-too
	(let ((val (wf (cadddr x))))
	  (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var val var)) new-prog)
	  (push `(<label> ,end-label) new-prog))
      (progn
	(push `(<setf> ,+setf+ ,(provisional-var var) nil) new-prog)
	(push `(<label> ,end-label) new-prog)))
    var))

(defun cond-branch (var x)
  (let ((flabel nil)
	(cond-last-var nil)
	(end-label (new-label))
	(ifvar nil))
    (push `(<comment> cond) new-prog)
    (make-var var)
    (loop for i in (cdr x) do
      (setf flabel (new-label))
      (setf ifvar (wf (car i)))
      (make-var ifvar)

      (if (and ifvar (symbolp ifvar) (not (eq ifvar t)))
	  (let ((lst (gethash ifvar vars)))
	  (if (not lst)
	      (format t "~A has no info?~%" ifvar))
	    (when (and lst
		       (not (varinfo-type lst))
		       (varinfo-temp lst))
	      (if *clm-debug* (format t "cond set ~A to boolean~%" ifvar))
	      (setf (varinfo-type lst) :clm-boolean))))

      (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var ifvar var)) new-prog)
      (if (not (eq ifvar t))
	  (push `(<jump-if-not> ,(current-var ifvar) ,flabel) new-prog))
      (if (cdr i)
	  (progn
	    (loop for j in (cdr i) do
	      (setf cond-last-var (wf j)))
	    (make-var cond-last-var)
	    (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var cond-last-var var)) new-prog)))
      (push `(<jump> ,end-label) new-prog)
      (push `(<label> ,flabel) new-prog))
    (push `(<setf> ,+setf+ ,(provisional-var var) nil) new-prog)
    (push `(<label> ,end-label) new-prog)
    var))

(defun case-branch (var x)
  (declare (ignore var))
  ;; CASE is of this form:
  ;;     eval index expr => t
  ;;     goto test
  ;; L1: code for S1
  ;;     goto next
  ;; L2 ...
  ;; ...
  ;; test: case V1 L1 (V1=index of that branch)
  ;;       case V2 L2 (i.e. if t=V2 goto L2 etc)
  ;; ...
  ;;     goto Ln (i.e. case-else branch, if any)
  ;; next:
  (push `(<comment> case ,(cadr x)) new-prog)
  (let ((test-lab (new-label))
	(next-lab (new-label))
	(nvar (make-compiler-temp))
	(selectors nil)
	(labels nil)
	(last-var nil)
	(v-lab nil)
	(var1 (wf (cadr x))))
    ;; can this return an integer constant? (case 1 ...)
    (make-var var1)
    (push `(<jump> ,test-lab) new-prog)
    (loop for i in (cddr x) do
      (setf v-lab (new-label))
      (push `(<label> ,v-lab) new-prog)
      (push (car i) selectors)
      (push v-lab labels)
      (setf last-var nil)
      (loop for j in (cdr i) do
	(setf last-var (wf j)))
      (make-var last-var)
      (if last-var (push `(<setf> ,+setf+ ,(provisional-var nvar) ,(chained-var last-var nvar)) new-prog))
      (push `(<jump> ,next-lab) new-prog))
    (push `(<label> ,test-lab) new-prog)
    (push `(<case> ,(current-var var1) ,(nreverse selectors) ,(nreverse labels)) new-prog)
    (push `(<setf> ,+setf+ ,(provisional-var nvar) nil) new-prog)
    (push `(<label> ,next-lab) new-prog)
    nvar))

(defun dotimes-branch (var x)
  (declare (ignore var))
  (let ((cmp-lab (new-label))
;	(res-lab (new-label))
	(body-lab (new-label))
	(end-lab (new-label))
	(end-var (make-compiler-temp :clm-integer))
	(done (make-compiler-temp :clm-boolean))
	(res-var (make-compiler-temp))
	(inner-labs nil)
	(endnum (second (second x)))
	(do-var (make-local-var (first (second x)) nil :clm-integer)))
    (if (and (symbolp endnum) (or (not vars) (not (gethash endnum vars)))) (make-user-var endnum :integer))
    (push `(<comment> dotimes ,(cadr x)) new-prog)
    (push (list nil res-var end-lab) return-stack)
    (push `(<setf> ,+setf+ ,(current-var do-var) 0) new-prog)
    (push `(<setf> ,+setf+ ,(provisional-var end-var) ,(chained-var (wf endnum) end-var)) new-prog)
    (push `(<label> ,cmp-lab) new-prog)
    (push `(<compare> ">" ,(current-var done) ,(current-var end-var) ,(current-var do-var)) new-prog)
    (push `(<jump-if> ,(current-var done) ,body-lab) new-prog) ;i.e. if leq drop through into result block
;    (push `(<label> ,res-lab) new-prog)
    (if (third (second x))
	(let ((val (wf (third (second x)))))
	  (push `(<setf> ,+setf+ ,(provisional-var res-var) ,(chained-var val res-var)) new-prog))
      (push `(<setf> ,+setf+ ,(provisional-var res-var) nil) new-prog))
    (push `(<jump> ,end-lab) new-prog)
    (push `(<label> ,body-lab) new-prog)
    (push (list (cadr x) end-lab) return-stack)
    (loop for i in (cddr x) do
      (if (listp i)
	  (wf i)
	(progn
	  (push i inner-labs)
	  (push `(<label> ,i) new-prog))))
    (push `(<setf> ,+incf+ ,(current-var do-var) 1) new-prog)
    (push `(<jump> ,cmp-lab) new-prog)
    (pop return-stack)			;cadr x?
    (pop return-stack)			;res-var?
    (push `(<label> ,end-lab) new-prog)
    (let ((lst (gethash do-var vars)))
      (decf (varinfo-depth lst)))
    res-var))

(defun do-branch (var x starred)
  (declare (ignore var))
  (let* ((body-lab (new-label))		;start of body (i.e. tagbody)
	 (test-lab (new-label))		;start of end test expr
;	 (res-lab (new-label))		;result expr
	 (step-lab nil)			;start of stepping exprs (are threaded through init code) (keep original intact)
	 (next-step-lab (new-label))
	 (next-init-lab nil)		;these two are for threading through initialization and stepping segments
	 (end-lab (new-label))		;end of statement (for return, etc)
	 (end-var (make-compiler-temp))	;result (if any) of do
	 (lvars (cadr x))		;list of (var init step) lists
	 (end-test (caaddr x))		;car of caddr I hope
	 (result (cadr (caddr x)))	;cadr of caddr?
	 (body (cdddr x))
	 (inner-labs nil))		;keep track of labels created on the fly
    (push `(<comment> do) new-prog)
    (let ((need-next-init nil)
	  (need-next-step nil))
      ;; variables (list of (var init step) lists)
      (loop for j on lvars do
	(let* ((i (car j))
	       (var-i (make-local-var (car i) (not starred))))
	  ;; I knew there was something really peculiar about do -- the local variable is not declared
	  ;; within the block of variables, yet it is declared within the step statement in its declaration!!!
	  (when next-init-lab
	    (push next-init-lab inner-labs)
	    (push `(<label> ,next-init-lab) new-prog))
	  (setf next-init-lab (new-label))
	  (setf need-next-init nil)
	  (if (cdr i)
	      (push `(<setf> ,+setf+ ,(current-var var-i var-i) ,(current-var (wf (cadr i)))) new-prog)
	    (push `(<setf> ,+setf+ ,(current-var var-i var-i) nil) new-prog))
	  (when (cddr i)		;is there a step expr?
	    (push `(<jump> ,next-init-lab) new-prog)
	    (setf need-next-init t)
	    (if (null step-lab) (setf step-lab next-step-lab))
	    (push `(<label> ,next-step-lab) new-prog)
	    (push next-step-lab inner-labs)
	    (setf next-step-lab (new-label))
	    (setf need-next-step nil)
	    ;; now make sure we get the current (not-yet-defined) variable if it is referenced in step expression
	    (let ((lst (gethash var-i vars)))
	      (if lst
		  (if (not starred)
		      (progn
			(incf (varinfo-depth lst))
			(setf (varinfo-shadowed lst) t)))
		(warn "step expr of ~A is screwed up" var-i))
	      (let ((val (current-var (wf (caddr i)))))
		(if (and lst (not starred)) (decf (varinfo-depth lst)))
		(push `(<setf> ,+setf+ ,(current-var var-i var-i) ,val) new-prog)))
	    (when (cdr j)
	      (setf need-next-step t)
	      (push `(<jump> ,next-step-lab) new-prog)))))
      (if (not starred)
	  (loop for j on lvars do
	    (let ((lst (gethash (caar j) vars)))
	      (if lst
		  (setf (varinfo-parallel lst) nil)
		(warn "failed to find ~A" j)))))
      (if (null step-lab)		;no step exprs found
	  (progn
	    (setf step-lab (new-label))
	    (push step-lab inner-labs)
	    (push `(<label> ,step-lab) new-prog))
	(when need-next-step
	  (push next-step-lab inner-labs)
	  (push `(<label> ,next-step-lab) new-prog)))
      (when need-next-init
	(push next-init-lab inner-labs)
	(push `(<label> ,next-init-lab) new-prog))
      ;; end test
      (push `(<label> ,test-lab) new-prog)
      (if end-test
	  (push `(<jump-if-not> ,(current-var (wf end-test)) ,body-lab) new-prog)
	(if result (push `(<jump> ,body-lab) new-prog)))
      ;; result
;      (push `(<label> ,res-lab) new-prog)
      (if result
	  (push `(<setf> ,+setf+ ,(provisional-var end-var) ,(chained-var (wf result) end-var)) new-prog)
	(push `(<setf> ,+setf+ ,(provisional-var end-var) nil) new-prog))
      (push `(<jump> ,end-lab) new-prog)
      (push `(<label> ,body-lab) new-prog)
      ;; DO body (a tagbody)
      (push (list nil end-var end-lab) return-stack)
      (loop for i in body do
	(if (listp i)
	    (wf i)
	  (progn
	    (push i inner-labs)
	    (push `(<label> ,i) new-prog))))
      (pop return-stack)
      (push `(<jump> ,step-lab) new-prog)
      (push `(<label> ,end-lab) new-prog)
      (loop for j on lvars do
	(let ((lst (gethash (caar j) vars)))
	  (if lst
	      (decf (varinfo-depth lst))
	    (warn "can't find ~A" j))))
      end-var)))

(defvar setf-functions (list (list 'mus-frequency '<mus-set-frequency> nil :mus-any)
			     ;; :mus-any as fourth declares that arg is a generator, (nil as third is aref constant for def-clm-struct)
			     ;; if fifth exists, it overrides :mus-any as particular gen type
			     (list 'mus-phase '<mus-set-phase> nil :mus-any)
			     (list 'mus-scaler '<mus-set-scaler> nil :mus-any)
			     (list 'mus-increment '<mus-set-increment> nil :mus-any)
			     (list 'mus-a0 '<mus-set-a0> nil :mus-any)
			     (list 'mus-a1 '<mus-set-a1> nil :mus-any)
			     (list 'mus-a2 '<mus-set-a2> nil :mus-any)
			     (list 'mus-b1 '<mus-set-b1> nil :mus-any)
			     (list 'mus-b2 '<mus-set-b2> nil :mus-any)
			     (list 'aref '<setf-aref> nil)
			     (list 'mus-feedforward '<mus-set-feedforward> nil :mus-any)
			     (list 'mus-feedback '<mus-set-feedback> nil :mus-any)
			     (list 'mus-hop '<mus-set-hop> nil :mus-any)
			     (list 'mus-ramp '<mus-set-ramp> nil :mus-any)
			     (list 'mus-location '<mus-set-location> nil :mus-any)
			     (list 'mus-length '<mus-set-length> nil :mus-any)
			     (list 'mus-width '<mus-set-width> nil :mus-any)
			     (list 'mus-xcoeff '<mus-set-xcoeff> nil :mus-any)
			     (list 'mus-ycoeff '<mus-set-ycoeff> nil :mus-any)
			     (list 'locsig-ref '<locsig-set!> nil :mus-any 'locsig)
			     (list 'locsig-reverb-ref '<locsig-reverb-set!> nil :mus-any 'locsig)
			     ;(list 'frame-ref '<frame-set!> nil :mus-any 'frame)
			     ;(list 'mixer-ref '<mixer-set!> nil :mus-any 'mixer)
			     ;(list 'mus-name '<mus-set-name> nil :mus-any)
			     ))

(defun setf-branch (var x &optional (set-type +setf+))
  (declare (ignore var))
  (when (> (length x) 1)			;i.e. (setq) which should return nil
    (let ((last-var nil))
      (if (evenp (length x))
	  (if (or (= set-type +setf+) (> (length x) 2))
	      (error "~D args to ~A in ~A?" (1- (length x)) (car x) x)
	    ;; here we have (incf <place>) etc
	    (setf x (append x (list 1)))))

      (loop for svar in (cdr x) by #'cddr and
                sval in (cddr x) by #'cddr do
	(let ((setf-data (and (listp svar) (find (car svar) setf-functions :key #'first)))
	      (evalled-sval (setf last-var (current-var (wf sval)))))

	  (if *clm-debug* (format t "~A ~A: set ~A~%" svar sval setf-data))

	  (if setf-data
	      ;; create <setf...> (var adr ...) val
	      ;; svar is either a symbol or a list (<accessor> name [args...])
	      (let* ((setf-name (second setf-data))
		     (setf-added-args (third setf-data)) ; aref index for def-clm-struct
		     (args (cdr svar))
		     (evalled-args (and args (loop for arg in args collect (current-var (wf arg)))))
		     (aref-type (gethash (second (first evalled-args)) vars)))

		(if *clm-debug* (format t "evalled: ~A~%" evalled-args))
		(if (and (fourth setf-data)
			 (= (length evalled-args) 1)
			 (listp (first evalled-args)))
		    (let ((info (gethash (second (first evalled-args)) vars)))
		      (when (and info
				 (not (varinfo-type info))
				 (not (varinfo-gen-type info)))

			(if *clm-debug* (format t "set ~A to :mus-any~%" (second (first evalled-args))))

			(if (eq (fourth setf-data) :mus-any)
			    (if (= (length setf-data) 5)
				(setf (varinfo-gen-type info) (fifth setf-data)) ; a particular gen type ('locsig etc)
			      (setf (varinfo-gen-type info) :mus-any))
			  (setf (varinfo-type info) (fourth setf-data))))))

		(if *clm-debug* (format t "aref-type: ~A, setf-added-args: ~A, data: ~A~%" aref-type setf-added-args setf-data))
		(if (and aref-type
			 (member (varinfo-type aref-type) '(:double-array :integer-array :float-array))
			 (or (eq (first setf-data) 'aref) ; trying to handle def-clm-float-struct
			     (not (third setf-data))))
		    (if (eq (varinfo-type aref-type) :float-array)
			(push `(<setf-float-aref> ,set-type ,(first evalled-args) ,(second evalled-args) ,evalled-sval) new-prog)
		      (if (eq (varinfo-type aref-type) :double-array)
			  (push `(<setf-double-aref> ,set-type ,@evalled-args ,evalled-sval) new-prog)
			(if (eq (varinfo-type aref-type) :integer-array)
			    (push `(<setf-integer-aref> ,set-type ,@evalled-args ,evalled-sval) new-prog))))
		  (if setf-added-args
		      (push `(,setf-name ,set-type ,@(append evalled-args setf-added-args) ,evalled-sval) new-prog)
		    (push `(,setf-name ,set-type ,@evalled-args ,evalled-sval) new-prog))))
	    (if (listp svar)
		(error "setf: unknown accessor: ~A in ~A" (car svar) svar)
	      (push `(<setf> ,set-type ,(current-var (make-user-var svar)) ,evalled-sval) new-prog)))
	  ))

      last-var)))

(defun aref-branch (var x)
  (make-var var)
  (let ((saved-gen-type *gen-type*))
    (let ((args (loop for form in (cdr x) by #'cdr collect (current-var (wf form)))))
      (setf *gen-type* saved-gen-type)

      (if *clm-debug* (format t "aref: ~A~%" *gen-type*))

      (let ((arrvar (gethash (cadr (car args)) vars))
	    (lst (gethash var vars)))
	(if *gen-type*
	    (progn
	      (if (and lst
		       (not (varinfo-gen-type lst)))
		  (setf (varinfo-gen-type lst) :mus-any))
	      (if (and arrvar
		       (or (not (varinfo-gen-type arrvar))
			   (not (varinfo-arr-gen-type arrvar))))
		  (progn
		    (setf (varinfo-gen-type arrvar) :mus-any-array)
		    (if (not (eq *gen-type* :mus-any))
			(setf (varinfo-arr-gen-type arrvar) *gen-type*)))))
#|
	  (if (and (not (varinfo-type arrvar))
		   (not (varinfo-gen-type arrvar)))
	      (progn
		(if (and lst
			 (not (varinfo-gen-type lst)))
		    (setf (varinfo-type lst) :clm-real))
		(if arrvar
		    (setf (varinfo-type arrvar) :float-array))))
|#
	  )

	(if (and lst
		 (not (varinfo-gen-type lst))
		 (not (varinfo-type lst)))
	    (if (eq (varinfo-gen-type arrvar) :mus-any-array)
		(setf (varinfo-gen-type lst) :mus-any)
	      (if (member (varinfo-type lst) '(:float-array :double-array))
		  (setf (varinfo-type lst) :clm-real)
		(if (eq (varinfo-type lst) :integer-array)
		    (setf (varinfo-type lst) :clm-integer)))))

	(if *clm-debug* (format t "aref: ~A is ~A~%" (cadr (car args)) (or (varinfo-gen-type arrvar) (varinfo-type arrvar) :unknown)))

	(loop for arg in (cdr args) do
	  (let ((arg-info (and (listp arg)
			       (gethash (second arg) vars))))
	    (if (and arg-info
		     (not (varinfo-type arg-info)))
		(if (varinfo-temp arg-info)
		    (setf (varinfo-type arg-info) :clm-integer)
		  (setf (varinfo-type arg-info) :integer)))))

	(if arrvar
	    (if (eq (varinfo-gen-type arrvar) :mus-any-array)
		(push `(<gen-aref> ,(current-var var) ,@args) new-prog)
	      (if (eq (varinfo-type arrvar) :float-array)
		  (progn
		    (setf (varinfo-type lst) :clm-real)
		    (push `(<float-aref> ,(current-var var) ,@args) new-prog))
		(if (eq (varinfo-type arrvar) :double-array)
		    (progn
		      (setf (varinfo-type lst) :clm-real)
		      (push `(<double-aref> ,(current-var var) ,@args) new-prog))
		  (if (eq (varinfo-type arrvar) :integer-array)
		      (progn
			(setf (varinfo-type lst) :clm-integer)
			(push `(<integer-aref> ,(current-var var) ,@args) new-prog))
		    (push `(<aref> ,(current-var var) ,@args) new-prog)))))
	  (push `(<aref> ,(current-var var) ,@args) new-prog)))))
  var)

(defun let-branch (var x starred)
  (let ((locals nil)
	(last-var nil))
    (make-var var)
    (when (cadr x)			;(LET NIL ...) ?
      (loop for svar in (cadr x) by #'cdr do
	;; the following is legal lisp: (let* ((hi 3) (hi (+ hi 1))) ...) so we have to walk the expression first
	(if (listp svar)		;(LET ((A 1) ...))
	    (let* ((form (current-var (wf (cadr svar))))
		   (sig (make-local-var (car svar)
					(not starred)
					(if (floatp form)
					    :clm-real
					  (let ((info (and (listp form)
							   (gethash (second form) vars))))
					    (and info
						 (or (varinfo-type info)
						     (and (varinfo-gen-type info)
							  :mus-any))))))))
	      (push sig locals)
	      (push `(<setf> ,+setf+ ,(current-var sig sig) ,form) new-prog))
	  (let ((sig (make-local-var svar (not starred))))
	    (push sig locals)))))
    (if (and locals (not starred))
	(loop for loc in locals do
	  (let ((lst (gethash loc vars)))
	    (setf (varinfo-parallel lst) nil))))
    (loop for i in (cddr x) do (setf last-var (wf i)))
    (push `(<setf> ,+setf+ ,(provisional-var var) ,(chained-var last-var var)) new-prog)
    (if locals
	(loop for loc in locals do
	  (let ((lst (gethash loc vars)))
	    (decf (varinfo-depth lst)))))
    var))

(defun lambda-branch (var x)
  ;; x is the outer (enclosing) list ((LAMBDA (args) body) args)
  (declare (ignore var))
  (let* ((passed-args (cdr x))
	 (lambda-args (second (first x)))
	 (lambda-body (cddr (first x))))
    (if (eq (car (car lambda-body)) 'declare) (setf lambda-body (cdr lambda-body)))
    ;; try to get rid of bogus LOOP-generated type declarations
    `(let (,@(loop for xx in lambda-args and yy in passed-args collect (list xx yy)))
       ,@lambda-body)))

;;; numbering is 0-based here(!)

(defun mark-as-string (val num)
  (declare (ignore num))
  (if (listp val) ; might be string constant
      (let ((info (gethash (second val) vars)))
	(if info (setf (varinfo-type info) :string)))))

(defun mark-as-frample2frample (val num)
  (declare (ignore num))
  (if (listp val)
      (let ((info (gethash (second val) vars)))
	(when (and info
		   (not (varinfo-type info))
		   (not (varinfo-gen-type info)))
	  (setf (varinfo-type info) :double-array)))))

(defun mark-arg2-as-integer (val num)
  (if (= num 1)
      (if (listp val)
	  (let ((info (gethash (second val) vars)))
	    (when (and info
		     (not (varinfo-type info))
		     (varinfo-temp info))
	      (if *clm-debug* (format t "set ~A as integer~%" (second val)))
	      (setf (varinfo-type info) :clm-integer))))))

(defun mark-arg1-as-integer (val num)
  (if (= num 0)
      (if (listp val)
	  (let ((info (gethash (second val) vars)))
	    (when (and info
		     (not (varinfo-type info))
		     (varinfo-temp info))
	      (if *clm-debug* (format t "set ~A as integer~%" (second val)))
	      (setf (varinfo-type info) :clm-integer))))))


(defvar function-type +as-needed-input+)

(def-clm-fun 'rem               #'(lambda (var x) (package-op '<rem> var x #'arith-type)))
(def-clm-fun 'numerator         #'(lambda (var x) (package-op '<numerator> var x :clm-integer)))
(def-clm-fun 'denominator       #'(lambda (var x) (package-op '<denominator> var x :clm-integer)))
(def-clm-fun 'max               #'(lambda (var x) (package-op '<max> var x #'arith-type)))
(def-clm-fun 'min               #'(lambda (var x) (package-op '<min> var x #'arith-type)))
(def-clm-fun 'abs               #'(lambda (var x) (package-op '<abs> var x #'arith-type)))
(def-clm-fun 'mod               #'(lambda (var x) (package-op '<mod> var x #'arith-type)))
(def-clm-fun 'floor             #'(lambda (var x) (package-op '<floor> var x :clm-integer)))        ;toward -inf (largest int not larger)
(def-clm-fun 'ceiling           #'(lambda (var x) (package-op '<ceiling> var x :clm-integer)))      ;toward +inf (smallest int not smaller)
(def-clm-fun 'round             #'(lambda (var x) (package-op '<round> var x :clm-integer)))        ;+.5 (round to even if .5)
(def-clm-fun 'truncate          #'(lambda (var x) (package-op '<truncate> var x :clm-integer)))     ;toward 0
(def-clm-fun 'signum            #'(lambda (var x) (package-op '<signum> var x #'arith-type)))
(def-clm-fun 'sqrt              #'(lambda (var x) (package-op '<sqrt> var x :clm-real)))
(def-clm-fun 'random            #'(lambda (var x) (package-op '<random> var x :clm-real)))
(def-clm-fun 'centered-random   #'(lambda (var x) (package-op '<centered-random> var x :clm-real)))
(def-clm-fun 'sin               #'(lambda (var x) (package-op '<sin> var x :clm-real)))
(def-clm-fun 'cos               #'(lambda (var x) (package-op '<cos> var x :clm-real)))
(def-clm-fun 'tan               #'(lambda (var x) (package-op '<tan> var x :clm-real)))
(def-clm-fun 'log               #'(lambda (var x) (package-op '<log> var x :clm-real)))
(def-clm-fun 'expt              #'(lambda (var x) (package-op '<expt> var x :clm-real)))
(def-clm-fun 'exp               #'(lambda (var x) (package-op '<exp> var x :clm-real)))
(def-clm-fun 'asin              #'(lambda (var x) (package-op '<asin> var x :clm-real)))
(def-clm-fun 'acos              #'(lambda (var x) (package-op '<acos> var x :clm-real)))
(def-clm-fun 'atan              #'(lambda (var x) (package-op '<atan> var x :clm-real)))
(def-clm-fun 'cosh              #'(lambda (var x) (package-op '<cosh> var x :clm-real)))
(def-clm-fun 'sinh              #'(lambda (var x) (package-op '<sinh> var x :clm-real)))
(def-clm-fun 'tanh              #'(lambda (var x) (package-op '<tanh> var x :clm-real)))
(def-clm-fun 'asinh             #'(lambda (var x) (package-op '<asinh> var x :clm-real)))
(def-clm-fun 'acosh             #'(lambda (var x) (package-op '<acosh> var x :clm-real)))
(def-clm-fun 'atanh             #'(lambda (var x) (package-op '<atanh> var x :clm-real)))
(def-clm-fun 'ash               #'(lambda (var x) (package-op '<ash> var x :clm-integer)))

(def-clm-fun 'erf               #'(lambda (var x) (package-op '<erf> var x :clm-real)))
(def-clm-fun 'erfc              #'(lambda (var x) (package-op '<erfc> var x :clm-real)))
(def-clm-fun 'lgamma            #'(lambda (var x) (package-op '<lgamma> var x :clm-real)))
(def-clm-fun 'bes-j0            #'(lambda (var x) (package-op '<bes-j0> var x :clm-real)))
(def-clm-fun 'bes-j1            #'(lambda (var x) (package-op '<bes-j1> var x :clm-real)))
(def-clm-fun 'bes-jn            #'(lambda (var x) (package-op '<bes-jn> var x :clm-real)))
(def-clm-fun 'bes-y0            #'(lambda (var x) (package-op '<bes-y0> var x :clm-real)))
(def-clm-fun 'bes-y1            #'(lambda (var x) (package-op '<bes-y1> var x :clm-real)))
(def-clm-fun 'bes-yn            #'(lambda (var x) (package-op '<bes-yn> var x :clm-real)))
(def-clm-fun 'bes-i0            #'(lambda (var x) (package-op '<bes-i0> var x :clm-real)))

(def-clm-fun 'oscil             #'(lambda (var x) (gen-package-op '<oscil> var x :clm-real 'oscil)))
(def-clm-fun 'oscil?            #'(lambda (var x) (gen-package-op '<oscil?> var x :clm-boolean :mus-any)))
(def-clm-fun 'asymmetric-fm     #'(lambda (var x) (gen-package-op '<asymmetric-fm> var x :clm-real 'asymmetric-fm)))
(def-clm-fun 'asymmetric-fm?    #'(lambda (var x) (gen-package-op '<asymmetric-fm?> var x :clm-boolean :mus-any)))
(def-clm-fun 'env               #'(lambda (var x) (gen-package-op '<env> var x :clm-real 'seg)))
(def-clm-fun 'env?              #'(lambda (var x) (gen-package-op '<env?> var x :clm-boolean :mus-any)))
(def-clm-fun 'restart-env       #'(lambda (var x) (declare (ignore var)) (gen-package-op '<restart-env> nil x nil 'seg)))

(def-clm-fun 'in-any		#'(lambda (var x)
				    (let ((res (package-op '<in-any> var x :clm-real)))
				      ;; third arg (fourth in x) is file->sample, presumably
				      (if (fourth x)
					  (let ((gen (gethash (fourth x) vars)))
					    (if gen (setf (varinfo-gen-type gen) 'file->sample))))
				      res)))

(def-clm-fun 'out-any           #'(lambda (var x)
				    ;; try to make *output* and channel 0 the defaults
				    (if (= (length x) 3)
					(setf x (append x (list 0 '*output*)))
				      (if (= (length x) 4)
					  (setf x (append x (list '*output*)))))
				    (let ((res (package-op '<out-any> var x :clm-real nil nil
							   #'(lambda (val num)
							       ;; third is data
							       (if (and (= num 1)
									(listp val))
								   (let ((data (gethash (second val) vars)))
								     (if (and data
									      (varinfo-temp data))
									 (setf (varinfo-type data) :clm-real))) ; data
								 (if (and (= num 2)
									  (listp val))
								     (let ((data (gethash (second val) vars)))
								       (if (and data
										(varinfo-temp data))
									   (setf (varinfo-type data) :clm-integer))) ; channel
								   (if (= num 3)
								       (let ((data (gethash (second val) vars)))
									 (if data
									     (setf (varinfo-gen-type data) 'frample->file))))))

							       (if *clm-debug* (format t "~A (~D): ~A~%"
								       val num
								       (let* ((sym (if (listp val) (second val) val))
									      (info (if (symbolp sym) (gethash sym vars))))
									 (if info (or (varinfo-type info) (varinfo-gen-type info))
									   "no info"))
								       ))
							       ))))
				      res)))

(def-clm-fun 'locsig            #'(lambda (var x) (gen-package-op '<locsig> var x :clm-real 'locsig)))
(def-clm-fun 'move-locsig       #'(lambda (var x) (declare (ignore var)) (gen-package-op '<move-locsig> nil x nil 'locsig)))
(def-clm-fun 'move-sound        #'(lambda (var x) (gen-package-op '<move-sound> var x :clm-real 'move-sound)))
(def-clm-fun 'rand              #'(lambda (var x) (gen-package-op '<rand> var x :clm-real 'rand)))
(def-clm-fun 'rand?             #'(lambda (var x) (gen-package-op '<rand?> var x :clm-boolean :mus-any)))
(def-clm-fun 'rand-interp       #'(lambda (var x) (gen-package-op '<rand-interp> var x :clm-real 'rand-interp)))
(def-clm-fun 'rand-interp?      #'(lambda (var x) (gen-package-op '<rand-interp?> var x :clm-boolean :mus-any)))
(def-clm-fun 'sawtooth-wave     #'(lambda (var x) (gen-package-op '<sawtooth-wave> var x :clm-real 'sawtooth-wave)))
(def-clm-fun 'sawtooth-wave?    #'(lambda (var x) (gen-package-op '<sawtooth-wave?> var x :clm-boolean :mus-any)))
(def-clm-fun 'triangle-wave     #'(lambda (var x) (gen-package-op '<triangle-wave> var x :clm-real 'triangle-wave)))
(def-clm-fun 'triangle-wave?    #'(lambda (var x) (gen-package-op '<triangle-wave?> var x :clm-boolean :mus-any)))
(def-clm-fun 'pulse-train       #'(lambda (var x) (gen-package-op '<pulse-train> var x :clm-real 'pulse-train)))
(def-clm-fun 'pulse-train?      #'(lambda (var x) (gen-package-op '<pulse-train?> var x :clm-boolean :mus-any)))
(def-clm-fun 'square-wave       #'(lambda (var x) (gen-package-op '<square-wave> var x :clm-real 'square-wave)))
(def-clm-fun 'square-wave?      #'(lambda (var x) (gen-package-op '<square-wave?> var x :clm-boolean :mus-any)))
(def-clm-fun 'table-lookup      #'(lambda (var x) (gen-package-op '<table-lookup> var x :clm-real 'table-lookup)))
(def-clm-fun 'table-lookup?     #'(lambda (var x) (gen-package-op '<table-lookup?> var x :clm-boolean :mus-any)))
(def-clm-fun 'delay             #'(lambda (var x) (gen-package-op '<delay> var x :clm-real 'delay)))
(def-clm-fun 'delay-tick        #'(lambda (var x) (gen-package-op '<delay-tick> var x :clm-real 'delay)))
(def-clm-fun 'delay?            #'(lambda (var x) (gen-package-op '<delay?> var x :clm-boolean :mus-any)))
(def-clm-fun 'tap               #'(lambda (var x) (gen-package-op '<tap> var x :clm-real :mus-any)))
(def-clm-fun 'comb              #'(lambda (var x) (gen-package-op '<comb> var x :clm-real 'comb)))
(def-clm-fun 'comb?             #'(lambda (var x) (gen-package-op '<comb?> var x :clm-boolean :mus-any)))
(def-clm-fun 'filtered-comb     #'(lambda (var x) (gen-package-op '<filtered-comb> var x :clm-real 'filtered-comb)))
(def-clm-fun 'filtered-comb?    #'(lambda (var x) (gen-package-op '<filtered-comb?> var x :clm-boolean :mus-any)))
(def-clm-fun 'notch             #'(lambda (var x) (gen-package-op '<notch> var x :clm-real 'notch)))
(def-clm-fun 'notch?            #'(lambda (var x) (gen-package-op '<notch?> var x :clm-boolean :mus-any)))
(def-clm-fun 'all-pass          #'(lambda (var x) (gen-package-op '<all-pass> var x :clm-real 'all-pass)))
(def-clm-fun 'all-pass?         #'(lambda (var x) (gen-package-op '<all-pass?> var x :clm-boolean :mus-any)))
(def-clm-fun 'moving-average    #'(lambda (var x) (gen-package-op '<moving-average> var x :clm-real 'moving-average)))
(def-clm-fun 'moving-average?   #'(lambda (var x) (gen-package-op '<moving-average?> var x :clm-boolean :mus-any)))
(def-clm-fun 'dot-product       #'(lambda (var x) (package-op '<dot-product> var x :clm-real)))
(def-clm-fun 'one-pole          #'(lambda (var x) (gen-package-op '<one-pole> var x :clm-real 'one-pole)))
(def-clm-fun 'one-pole?         #'(lambda (var x) (gen-package-op '<one-pole?> var x :clm-boolean :mus-any)))
(def-clm-fun 'one-zero          #'(lambda (var x) (gen-package-op '<one-zero> var x :clm-real 'one-zero)))
(def-clm-fun 'one-zero?         #'(lambda (var x) (gen-package-op '<one-zero?> var x :clm-boolean :mus-any)))
(def-clm-fun 'two-pole          #'(lambda (var x) (gen-package-op '<two-pole> var x :clm-real 'two-pole)))
(def-clm-fun 'two-pole?         #'(lambda (var x) (gen-package-op '<two-pole?> var x :clm-boolean :mus-any)))
(def-clm-fun 'two-zero          #'(lambda (var x) (gen-package-op '<two-zero> var x :clm-real 'two-zero)))
(def-clm-fun 'two-zero?         #'(lambda (var x) (gen-package-op '<two-zero?> var x :clm-boolean :mus-any)))
(def-clm-fun 'firmant           #'(lambda (var x) (gen-package-op '<firmant> var x :clm-real 'firmant)))
(def-clm-fun 'firmant?          #'(lambda (var x) (gen-package-op '<firmant?> var x :clm-boolean :mus-any)))
(def-clm-fun 'formant           #'(lambda (var x) (gen-package-op '<formant> var x :clm-real 'formant)))
(def-clm-fun 'formant?          #'(lambda (var x) (gen-package-op '<formant?> var x :clm-boolean :mus-any)))
(def-clm-fun 'filter            #'(lambda (var x) (gen-package-op '<filter> var x :clm-real 'filter)))
(def-clm-fun 'filter?           #'(lambda (var x) (gen-package-op '<filter?> var x :clm-boolean :mus-any)))
(def-clm-fun 'fir-filter        #'(lambda (var x) (gen-package-op '<fir-filter> var x :clm-real 'fir-filter)))
(def-clm-fun 'fir-filter?       #'(lambda (var x) (gen-package-op '<fir-filter?> var x :clm-boolean :mus-any)))
(def-clm-fun 'iir-filter        #'(lambda (var x) (gen-package-op '<iir-filter> var x :clm-real 'iir-filter)))
(def-clm-fun 'iir-filter?       #'(lambda (var x) (gen-package-op '<iir-filter?> var x :clm-boolean :mus-any)))
(def-clm-fun 'fft               #'(lambda (var x) (declare (ignore var)) (package-op '<fft> nil x)))
(def-clm-fun 'autocorrelate     #'(lambda (var x) (declare (ignore var)) (package-op '<autocorrelate> nil x)))
(def-clm-fun 'correlate         #'(lambda (var x) (declare (ignore var)) (package-op '<correlate> nil x)))
(def-clm-fun 'convolution       #'(lambda (var x) (declare (ignore var)) (package-op '<convolution> nil x)))
(def-clm-fun 'spectrum          #'(lambda (var x) (declare (ignore var)) (package-op '<spectrum> nil x)))
(def-clm-fun 'multiply-arrays   #'(lambda (var x) (declare (ignore var)) (package-op '<multiply-arrays> nil x)))
(def-clm-fun 'polar->rectangular #'(lambda (var x) (declare (ignore var)) (package-op '<polar2rectangular> nil x)))
(def-clm-fun 'rectangular->polar #'(lambda (var x) (declare (ignore var)) (package-op '<rectangular2polar> nil x)))
(def-clm-fun 'rectangular->magnitudes #'(lambda (var x) (declare (ignore var)) (package-op '<rectangular2magnitudes> nil x)))
(def-clm-fun 'clear-array       #'(lambda (var x) (declare (ignore var)) (package-op '<clear-array> nil x)))

(defvar arg-funcs nil)

(def-clm-fun 'convolve          #'(lambda (var x)
				    (let ((ctr function-ctr)
					  (old-as-needed *as-needed*))
				      (setf *as-needed* nil)
				      (setf *global* old-as-needed)
				      (setf function-type +as-needed-input+)
				      (let ((res (gen-package-op '<convolve> var x :clm-real 'convolve)))

					(if *clm-debug* (format t "convolve: ~A ~A~%" old-as-needed *as-needed*))

					(if *as-needed*
					    (let ((gen (gethash (second x) vars)))
					      (if gen
						  (setf (varinfo-ctr gen) (list :input ctr)))
					      (push (list :input ctr) *as-needed-function-types*)))
					(setf *as-needed* old-as-needed)
					(setf *global* *as-needed*)
					res))))

(def-clm-fun 'convolve?         #'(lambda (var x) (gen-package-op '<convolve?> var x :clm-boolean :mus-any)))

(def-clm-fun 'src               #'(lambda (var x)
				    (let ((ctr function-ctr)
					  (old-as-needed *as-needed*))
				      (setf *as-needed* nil)
				      (setf *global* old-as-needed)
				      (setf function-type +as-needed-input+)
				      (let ((res (gen-package-op '<src> var x :clm-real 'src)))
					(if *as-needed*
					    (let ((gen (gethash (second x) vars)))
					      (if gen
						  (setf (varinfo-ctr gen) (list :input ctr)))
					      (push (list :input ctr) *as-needed-function-types*)))
					(setf *as-needed* old-as-needed)
					(setf *global* *as-needed*)
					res))))

(def-clm-fun 'src?              #'(lambda (var x) (gen-package-op '<src?> var x :clm-boolean :mus-any)))

(def-clm-fun 'granulate         #'(lambda (var x)
				    (let ((ctr function-ctr)
					  (data nil)
					  (old-as-needed *as-needed*))
				      (setf *as-needed* nil)
				      (setf *global* old-as-needed)
				      (setf function-type +as-needed-input+)
				      (let ((res (gen-package-op '<granulate> var x :clm-real 'granulate
								 #'(lambda (val argctr)
								     (if *clm-debug* (format t "arg ~A ~A (~A ~A ~A)~%"
											     argctr val ctr function-ctr function-type))
								     (if (> function-ctr ctr)
									 (let ((tag (list
										     (if (= function-type +as-needed-edit+)
											 :edit
										       :input)
										     ctr)))
									   (push tag data)
									   (push tag *as-needed-function-types*)
									   (setf ctr function-ctr)))
								     (if (= argctr 1)
									 (setf function-type +as-needed-edit+))))))
;					(if *clm-debug* (format t "data: ~A -> ~A~%" data *as-needed-function-types*))
					(setf function-type +as-needed-input+)
					(if data
					    (let ((gen (gethash (second x) vars)))
					      (if gen
						  (setf (varinfo-ctr gen) data))))
					(setf *as-needed* old-as-needed)
					(setf *global* *as-needed*)
					res))))

(def-clm-fun 'granulate?        #'(lambda (var x) (gen-package-op '<granulate?> var x :clm-boolean :mus-any)))

(def-clm-fun 'phase-vocoder     #'(lambda (var x)
				    (let ((ctr function-ctr)
					  (data nil)
					  (old-as-needed *as-needed*))
				      (setf *as-needed* nil)
				      (setf *global* old-as-needed)
				      (setf function-type +as-needed-input+)
				      (let ((res (gen-package-op '<phase-vocoder> var x :clm-real 'phase-vocoder
								 #'(lambda (val argctr)
								     (declare (ignore val))
								     (if (> function-ctr ctr)
									 (let ((tag (list
										     (if (= function-type +as-needed-edit+)
											 :edit
										       (if (= function-type +as-needed-analyze+)
											   :analyze
											 (if (= function-type +as-needed-synthesize+)
											     :synthesize
											   :input)))
										     ctr)))
									   (push tag data)
									   (push tag *as-needed-function-types*)
									   (setf ctr function-ctr)))
								     (if (= argctr 1)
									 (setf function-type +as-needed-analyze+)
								       (if (= argctr 2)
									   (setf function-type +as-needed-edit+)
									 (if (= argctr 3)
									     (setf function-type +as-needed-synthesize+))))))))
;					(if *clm-debug* (format t "data: ~A -> ~A~%" data *as-needed-function-types*))
					(setf function-type +as-needed-input+)
					(if data
					    (let ((gen (gethash (second x) vars)))
					      (if gen
						  (setf (varinfo-ctr gen) data))))
					(setf *as-needed* old-as-needed)
					(setf *global* *as-needed*)
					res))))

(def-clm-fun 'phase-vocoder?    #'(lambda (var x) (gen-package-op '<phase-vocoder?> var x :clm-boolean :mus-any)))

(def-clm-fun 'function #'(lambda (var x)
			   (let ((ctr function-ctr)
				 (old-prog new-prog))
			     (incf function-ctr)
			     (setf new-prog nil)
			     (setf *as-needed* (cons ctr *as-needed*)) ; push lambda stack
			     (setf *global* t)
			     ;; this is the "as-needed" input function for src, granulate, pv, and convolve

			     (let* ((its-return-type
				     (if (= function-type +as-needed-input+)
					 :clm-real
				       (if (= function-type +as-needed-edit+)
					   :clm-integer
					 (if (= function-type +as-needed-analyze+)
					     :clm-boolean
					   :clm-real))))) ; pv synthesize

			       (push `(<start-function> ,ctr ,function-type) new-prog)
			       (push (list (first (second (second x))) ctr) lambda-args)
			       (loop for form in (cddr (second x)) do (setf var (wf form)))
			       (push `(<end-function> ,var ,ctr) new-prog)
			       (setf *as-needed-functions* (append new-prog *as-needed-functions*))
			       (setf new-prog old-prog)
			       (pop lambda-args)
			       (make-var var its-return-type)
			       ))))

(def-clm-fun 'lambda-arg #'(lambda (var x)
			     (make-var var (if (= function-type +as-needed-input+)
					       :clm-integer
					     :mus-any))
			     (push `(<lambda-arg> ,(current-var var) ,(second x) ,function-type) new-prog)
			     var))


				;contrast-enhancement is handled as a macro in mus.lisp
(def-clm-fun 'readin            #'(lambda (var x) (gen-package-op '<readin> var x :clm-real 'readin)))
(def-clm-fun 'readin?           #'(lambda (var x) (gen-package-op '<readin?> var x :clm-boolean :mus-any)))

(def-clm-fun 'mus-feedforward   #'(lambda (var x) (gen-package-op '<mus-feedforward> var x :clm-real :mus-any)))
(def-clm-fun 'mus-feedback      #'(lambda (var x) (gen-package-op '<mus-feedback> var x :clm-real :mus-any)))
(def-clm-fun 'mus-hop           #'(lambda (var x) (gen-package-op '<mus-hop> var x :clm-real :mus-any)))
(def-clm-fun 'mus-ramp          #'(lambda (var x) (gen-package-op '<mus-ramp> var x :clm-real :mus-any)))
(def-clm-fun 'mus-order         #'(lambda (var x) (gen-package-op '<mus-order> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-location      #'(lambda (var x) (gen-package-op '<mus-location> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-increment     #'(lambda (var x) (gen-package-op '<mus-increment> var x :clm-real :mus-any)))
(def-clm-fun 'mus-channel       #'(lambda (var x) (gen-package-op '<mus-channel> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-channels      #'(lambda (var x) (gen-package-op '<mus-channels> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-interp-type   #'(lambda (var x) (gen-package-op '<mus-interp-type> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-length        #'(lambda (var x) (gen-package-op '<mus-length> var x :clm-integer :mus-any)))
(def-clm-fun 'mus-width         #'(lambda (var x) (gen-package-op '<mus-width> var x :clm-real :mus-any)))
(def-clm-fun 'mus-frequency     #'(lambda (var x) (gen-package-op '<mus-frequency> var x :clm-real :mus-any)))
(def-clm-fun 'mus-phase         #'(lambda (var x) (gen-package-op '<mus-phase> var x :clm-real :mus-any)))
(def-clm-fun 'mus-scaler        #'(lambda (var x) (gen-package-op '<mus-scaler> var x :clm-real :mus-any)))
(def-clm-fun 'mus-offset        #'(lambda (var x) (gen-package-op '<mus-offset> var x :clm-real :mus-any)))
(def-clm-fun 'mus-a0            #'(lambda (var x) (gen-package-op '<mus-a0> var x :clm-real :mus-any)))
(def-clm-fun 'mus-a1            #'(lambda (var x) (gen-package-op '<mus-a1> var x :clm-real :mus-any)))
(def-clm-fun 'mus-a2            #'(lambda (var x) (gen-package-op '<mus-a2> var x :clm-real :mus-any)))
(def-clm-fun 'mus-b1            #'(lambda (var x) (gen-package-op '<mus-b1> var x :clm-real :mus-any)))
(def-clm-fun 'mus-b2            #'(lambda (var x) (gen-package-op '<mus-b2> var x :clm-real :mus-any)))
(def-clm-fun 'mus-describe      #'(lambda (var x) (gen-package-op '<mus-describe> var x :string :mus-any)))
(def-clm-fun 'mus-name          #'(lambda (var x) (gen-package-op '<mus-name> var x :string :mus-any)))
(def-clm-fun 'mus-file-name     #'(lambda (var x) (gen-package-op '<mus-file-name> var x :string :mus-any)))
(def-clm-fun 'mus-reset         #'(lambda (var x) (gen-package-op '<mus-reset> var x :clm-real :mus-any)))

(def-clm-fun 'mus-data          #'(lambda (var x) (gen-package-op '<mus-data> var x :float-array :mus-any)))
(def-clm-fun 'mus-xcoeffs       #'(lambda (var x) (gen-package-op '<mus-xcoeffs> var x :float-array :mus-any)))
(def-clm-fun 'mus-ycoeffs       #'(lambda (var x) (gen-package-op '<mus-ycoeffs> var x :float-array :mus-any)))
(def-clm-fun 'mus-xcoeff        #'(lambda (var x) (gen-package-op '<mus-xcoeff> var x :clm-real :mus-any #'mark-arg2-as-integer)))
(def-clm-fun 'mus-ycoeff        #'(lambda (var x) (gen-package-op '<mus-ycoeff> var x :clm-real :mus-any #'mark-arg2-as-integer)))
(def-clm-fun 'mus-equal         #'(lambda (var x) (gen-package-op '<mus-equal> var x :clm-boolean :mus-any)))

(def-clm-fun 'phase-vocoder-amp-increments #'(lambda (var x) (gen-package-op '<phase-vocoder-amp-increments> var x :float-array 'phase-vocoder)))
(def-clm-fun 'phase-vocoder-amps           #'(lambda (var x) (gen-package-op '<phase-vocoder-amps> var x :float-array 'phase-vocoder)))
(def-clm-fun 'phase-vocoder-freqs          #'(lambda (var x) (gen-package-op '<phase-vocoder-freqs> var x :float-array 'phase-vocoder)))
(def-clm-fun 'phase-vocoder-phases         #'(lambda (var x) (gen-package-op '<phase-vocoder-phases> var x :float-array 'phase-vocoder)))
(def-clm-fun 'phase-vocoder-phase-increments #'(lambda (var x) (gen-package-op '<phase-vocoder-phase-increments> var x :float-array 'phase-vocoder)))

(def-clm-fun 'probe-file          #'(lambda (var x) (package-op '<probe-file> var x :clm-boolean nil nil #'mark-as-string)))

(def-clm-fun 'sound-duration      #'(lambda (var x) (package-op '<sound-duration> var x :clm-real nil nil #'mark-as-string)))
(def-clm-fun 'sound-chans         #'(lambda (var x) (package-op '<sound-chans> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-data-format   #'(lambda (var x) (package-op '<sound-data-format> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-data-location #'(lambda (var x) (package-op '<sound-data-location> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-datum-size    #'(lambda (var x) (package-op '<sound-datum-size> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-header-type   #'(lambda (var x) (package-op '<sound-header-type> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-length        #'(lambda (var x) (package-op '<sound-length> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-samples       #'(lambda (var x) (package-op '<sound-samples> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-framples        #'(lambda (var x) (package-op '<sound-framples> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-srate         #'(lambda (var x) (package-op '<sound-srate> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'sound-maxamp        #'(lambda (var x) (package-op '<sound-maxamp> var x nil nil nil #'mark-as-string)))
(def-clm-fun 'sound-comment       #'(lambda (var x) (package-op '<sound-comment> var x :string nil nil #'mark-as-string)))

(def-clm-fun 'mus-sound-duration      #'(lambda (var x) (package-op '<sound-duration> var x :clm-real nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-chans         #'(lambda (var x) (package-op '<sound-chans> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-data-format   #'(lambda (var x) (package-op '<sound-data-format> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-data-location #'(lambda (var x) (package-op '<sound-data-location> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-datum-size    #'(lambda (var x) (package-op '<sound-datum-size> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-header-type   #'(lambda (var x) (package-op '<sound-header-type> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-length        #'(lambda (var x) (package-op '<sound-length> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-samples       #'(lambda (var x) (package-op '<sound-samples> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-framples        #'(lambda (var x) (package-op '<sound-framples> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-srate         #'(lambda (var x) (package-op '<sound-srate> var x :clm-integer nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-maxamp        #'(lambda (var x) (package-op '<sound-maxamp> var x nil nil nil #'mark-as-string)))
(def-clm-fun 'mus-sound-comment       #'(lambda (var x) (package-op '<sound-comment> var x :string nil nil #'mark-as-string)))

(def-clm-fun 'mus-header-type-name #'(lambda (var x) (package-op '<mus-header-type-name> var x :string nil nil #'mark-arg1-as-integer)))
(def-clm-fun 'mus-data-format-name #'(lambda (var x) (package-op '<mus-data-format-name> var x :string nil nil #'mark-arg1-as-integer)))
(def-clm-fun 'string-equal        #'(lambda (var x) (package-op '<string-equal> var x :clm-boolean nil nil #'mark-as-string)))
(def-clm-fun 'string=             #'(lambda (var x) (package-op '<string=> var x :clm-boolean nil nil #'mark-as-string)))

(def-clm-fun 'snd-memo            #'(lambda (var x) (package-op '<snd-memo> var x :clm-integer)))

(def-clm-fun 'wave-train        #'(lambda (var x) (gen-package-op '<wave-train> var x :clm-real 'wave-train)))
(def-clm-fun 'wave-train?       #'(lambda (var x) (gen-package-op '<wave-train?> var x :clm-boolean :mus-any)))
(def-clm-fun 'polyshape         #'(lambda (var x) (gen-package-op '<polyshape> var x :clm-real 'polyshape)))
(def-clm-fun 'polyshape?        #'(lambda (var x) (gen-package-op '<polyshape?> var x :clm-boolean :mus-any)))
(def-clm-fun 'polywave          #'(lambda (var x) (gen-package-op '<polywave> var x :clm-real 'polywave)))
(def-clm-fun 'polywave?         #'(lambda (var x) (gen-package-op '<polywave?> var x :clm-boolean :mus-any)))
(def-clm-fun 'array-interp      #'(lambda (var x) (package-op '<array-interp> var x :clm-real)))
(def-clm-fun 'mus-interpolate   #'(lambda (var x) (package-op '<mus-interpolate> var x :clm-real)))
(def-clm-fun 'polynomial        #'(lambda (var x) (package-op '<polynomial> var x :clm-real)))
(def-clm-fun 'chebyshev-u-sum   #'(lambda (var x) (package-op '<chebyshev-u-sum> var x :clm-real)))
(def-clm-fun 'chebyshev-t-sum   #'(lambda (var x) (package-op '<chebyshev-t-sum> var x :clm-real)))
(def-clm-fun 'chebyshev-tu-sum  #'(lambda (var x) (package-op '<chebyshev-tu-sum> var x :clm-real)))
(def-clm-fun 'hz->radians       #'(lambda (var x) (package-op '<hz2radians> var x :clm-real)))
(def-clm-fun 'radians->hz       #'(lambda (var x) (package-op '<radians2hz> var x :clm-real)))
(def-clm-fun 'degrees->radians  #'(lambda (var x) (package-op '<degrees2radians> var x :clm-real)))
(def-clm-fun 'radians->degrees  #'(lambda (var x) (package-op '<radians2degrees> var x :clm-real)))
(def-clm-fun 'db->linear        #'(lambda (var x) (package-op '<db2linear> var x :clm-real)))
(def-clm-fun 'linear->db        #'(lambda (var x) (package-op '<linear2db> var x :clm-real)))
(def-clm-fun 'seconds->samples  #'(lambda (var x) (package-op '<seconds2samples> var x :clm-integer)))
(def-clm-fun 'samples->seconds  #'(lambda (var x) (package-op '<samples2seconds> var x :clm-real)))
(def-clm-fun 'ncos              #'(lambda (var x) (gen-package-op '<ncos> var x :clm-real 'ncos)))
(def-clm-fun 'ncos?             #'(lambda (var x) (gen-package-op '<ncos?> var x :clm-boolean :mus-any)))
(def-clm-fun 'nrxycos           #'(lambda (var x) (gen-package-op '<nrxycos> var x :clm-real 'nrxycos)))
(def-clm-fun 'nrxycos?          #'(lambda (var x) (gen-package-op '<nrxycos?> var x :clm-boolean :mus-any)))
(def-clm-fun 'nsin              #'(lambda (var x) (gen-package-op '<nsin> var x :clm-real 'nsin)))
(def-clm-fun 'nsin?             #'(lambda (var x) (gen-package-op '<nsin?> var x :clm-boolean :mus-any)))
(def-clm-fun 'nrxysin           #'(lambda (var x) (gen-package-op '<nrxysin> var x :clm-real 'nrxysin)))
(def-clm-fun 'nrxysin?          #'(lambda (var x) (gen-package-op '<nrxysin?> var x :clm-boolean :mus-any)))
(def-clm-fun 'ssb-am            #'(lambda (var x) (gen-package-op '<ssb-am> var x :clm-real 'ssb-am)))
(def-clm-fun 'ssb-am?           #'(lambda (var x) (gen-package-op '<ssb-am?> var x :clm-boolean :mus-any)))
(def-clm-fun '=                 #'(lambda (var x) (package-op '<compare> var x :clm-boolean "==")))
(def-clm-fun '/=                #'(lambda (var x) (package-op '<neq> var x :clm-boolean)))
(def-clm-fun '<                 #'(lambda (var x) (package-op '<compare> var x :clm-boolean "<")))
(def-clm-fun '>                 #'(lambda (var x) (package-op '<compare> var x :clm-boolean ">")))
(def-clm-fun '<=                #'(lambda (var x) (package-op '<compare> var x :clm-boolean "<=")))
(def-clm-fun '>=                #'(lambda (var x) (package-op '<compare> var x :clm-boolean ">=")))
(def-clm-fun 'zerop             #'(lambda (var x) (package-op '<zerop> var x :clm-boolean)))
(def-clm-fun 'plusp             #'(lambda (var x) (package-op '<plusp> var x :clm-boolean)))
(def-clm-fun 'minusp            #'(lambda (var x) (package-op '<minusp> var x :clm-boolean)))
(def-clm-fun 'oddp              #'(lambda (var x) (package-op '<oddp> var x :clm-boolean)))
(def-clm-fun 'evenp             #'(lambda (var x) (package-op '<evenp> var x :clm-boolean)))
(def-clm-fun 'quote             #'(lambda (var x) (declare (ignore var)) (if (numberp (cadr x)) (cadr x) (give-up x))))
                                ;; '2 = 2 in all cases, I think.
(def-clm-fun 'integerp          #'(lambda (var x) (package-op '<integer?> var x :clm-boolean)))
(def-clm-fun 'bignump           #'(lambda (var x) (package-op '<bignum?> var x :clm-boolean)))
(def-clm-fun 'numberp           #'(lambda (var x) (package-op '<number?> var x :clm-boolean)))
(def-clm-fun 'floatp            #'(lambda (var x) (package-op '<float?> var x :clm-boolean)))
(def-clm-fun 'realp             #'(lambda (var x) (package-op '<number?> var x :clm-boolean)))
(def-clm-fun 'stringp           #'(lambda (var x) (package-op '<string?> var x :clm-boolean)))
(def-clm-fun 'eq                #'(lambda (var x) (package-op '<eq> var x :clm-boolean)))
(def-clm-fun 'eql               #'(lambda (var x) (package-op '<eql> var x :clm-boolean)))
(def-clm-fun 'identity          #'(lambda (var x) (declare (ignore var)) (second x)))
(def-clm-fun 'double            #'(lambda (var x) (declare (ignore var)) (second x)))
(def-clm-fun 'double-float      #'(lambda (var x) (declare (ignore var)) (second x)))
(def-clm-fun 'float             #'(lambda (var x) (declare (ignore var)) (second x)))
;(def-clm-fun 'float             #'(lambda (var x) (package-op '<float> var x :clm-real)))
;(def-clm-fun 'double-float      #'(lambda (var x) (package-op '<double> var x :clm-real)))
;(def-clm-fun 'double            #'(lambda (var x) (package-op '<double> var x :clm-real)))
;(def-clm-fun 'equal             #'(lambda (var x) (package-op '<equal> var x :clm-boolean)))
;(def-clm-fun 'equalp            #'(lambda (var x) (package-op '<equalp> var x :clm-boolean)))
(def-clm-fun 'arrayp            #'(lambda (var x) (package-op '<array?> var x :clm-boolean)))

(def-clm-fun 'file->frample       #'(lambda (var x)
				    (declare (ignore var))
				    (gen-package-op '<file2frample> nil x nil 'file->frample
						    #'(lambda (val num)
							(if (and (= num 2)
								 (listp val))
							    (let ((info (gethash (second val) vars)))
							      (when (and info
									 (not (varinfo-type info))
									 (not (varinfo-gen-type info)))
								(if *clm-debug* (format t "set ~A as frame~%" (second val)))
								(setf (varinfo-type info) :double-array))))))
				    (fourth x)))

(def-clm-fun 'file->frample?      #'(lambda (var x) (gen-package-op '<file2frample?> var x :clm-boolean :mus-any)))

(def-clm-fun 'frample->file       #'(lambda (var x) (declare (ignore var)) (gen-package-op '<frample2file> nil x nil 'frample->file)))
(def-clm-fun 'frample->file?      #'(lambda (var x) (gen-package-op '<frample2file?> var x :clm-boolean :mus-any)))

;(def-clm-fun 'sample->frame     #'(lambda (var x) (declare (ignore var)) (gen-package-op '<sample2frame> nil x nil 'frame) (fourth x)))
;(def-clm-fun 'frame->sample     #'(lambda (var x) (gen2-package-op '<frame2sample> var x :clm-real 'frame 'frame)))
(def-clm-fun 'file->array       #'(lambda (var x) (declare (ignore var)) (package-op '<file2array> nil x)))
(def-clm-fun 'array->file       #'(lambda (var x) (declare (ignore var)) (package-op '<array2file> nil x)))

(def-clm-fun 'file->sample      #'(lambda (var x) (gen-package-op '<file2sample> var x :clm-real 'file->sample)))
(def-clm-fun 'file->sample?     #'(lambda (var x) (gen-package-op '<file2sample?> var x :clm-boolean :mus-any)))
(def-clm-fun 'sample->file?     #'(lambda (var x) (gen-package-op '<sample2file?> var x :clm-boolean :mus-any)))
(def-clm-fun 'sample->file      #'(lambda (var x) (gen-package-op '<sample2file> var x :clm-real 'sample->file)))
(def-clm-fun 'mus-input?        #'(lambda (var x) (gen-package-op '<mus-input?> var x :clm-boolean :mus-any)))
(def-clm-fun 'mus-output?       #'(lambda (var x) (gen-package-op '<mus-output?> var x :clm-boolean :mus-any)))

(def-clm-fun 'locsig-ref        #'(lambda (var x) (gen-package-op '<locsig-ref> var x :clm-real 'locsig #'mark-arg2-as-integer)))
(def-clm-fun 'locsig-reverb-ref #'(lambda (var x) (gen-package-op '<locsig-reverb-ref> var x :clm-real 'locsig #'mark-arg2-as-integer)))
(def-clm-fun 'locsig-set!       #'(lambda (var x) (declare (ignore var)) (gen-package-op '<locsig-set!> 0 x nil 'locsig)))
(def-clm-fun 'locsig-reverb-set! #'(lambda (var x) (declare (ignore var)) (gen-package-op '<locsig-reverb-set!> 0 x nil 'locsig)))

(def-clm-fun 'frample->frample      #'(lambda (var x) (declare (ignore var)) (gen-package-op '<frample2frample> nil x nil nil #'mark-as-frample2frample) (fourth x)))

(def-clm-fun 'null              #'(lambda (var x) (package-op '<null> var x :clm-boolean)))
(def-clm-fun 'not               #'(lambda (var x) (package-op '<not> var x :clm-boolean)))
(def-clm-fun 'the               #'(lambda (var x) (declare (ignore var)) (third x)))

;;; incf and decf are macros, but they can expand into calls that are specific to a given lisp

(def-clm-fun 'incf              #'(lambda (var x) (setf-branch var x +incf+)))
(def-clm-fun 'decf              #'(lambda (var x) (setf-branch var x +decf+)))
(def-clm-fun '-                 #'(lambda (var x)
				   (if (cddr x)
				       (package-op '<subtract> var x #'arith-type)
				     (package-op '<negate> var x #'arith-type))))
(def-clm-fun '*                 #'(lambda (var x) (package-op '<multiply> var x #'arith-type)))
(def-clm-fun '+                 #'(lambda (var x) (package-op '<add> var x #'arith-type)))
(def-clm-fun '/                 #'(lambda (var x) (package-op '<divide> var x :clm-real)))
(def-clm-fun '1+                #'(lambda (var x) (package-op '<add-1> var x #'arith-type)))
(def-clm-fun '1-                #'(lambda (var x) (package-op '<sub-1> var x #'arith-type)))
(def-clm-fun 'if                'if-branch)
(def-clm-fun 'cond              'cond-branch)
(def-clm-fun 'case              'case-branch)
(def-clm-fun 'dotimes           'dotimes-branch)
(def-clm-fun 'do                #'(lambda (var x) (do-branch var x nil)))
(def-clm-fun 'do*               #'(lambda (var x) (do-branch var x t)))
(def-clm-fun 'setf              'setf-branch)
(def-clm-fun 'setq              'setf-branch)
(def-clm-fun 'tagbody           'tagbody-branch)
(def-clm-fun 'block             'block-branch)
(def-clm-fun 'return            'return-branch)
(def-clm-fun 'return-from       'return-from-branch)
(def-clm-fun 'loop              'find-loop-control-info)

(def-clm-fun 'warn              #'(lambda (var x) (declare (ignore var)) (package-op '<warn> nil x :clm-boolean) nil))
(def-clm-fun 'error             #'(lambda (var x) (declare (ignore var)) (package-op '<error> nil x) nil))
(def-clm-fun 'print             #'(lambda (var x) (declare (ignore var)) (package-op '<print> nil x :clm-boolean nil t)))
(def-clm-fun 'clm-print         #'(lambda (var x) (declare (ignore var)) (package-op '<clm-print> nil x :clm-boolean nil t)))
(def-clm-fun 'princ             #'(lambda (var x) (declare (ignore var)) (package-op '<princ> nil x :clm-boolean nil t)))
(def-clm-fun 'terpri            #'(lambda (var x) (declare (ignore var)) (package-op '<terpri> nil x :clm-boolean nil t)))
(def-clm-fun 'apply             #'(lambda (var x) (package-op '<apply> var x)))
(def-clm-fun 'aref              'aref-branch)
(def-clm-fun 'elt               'aref-branch)
(def-clm-fun 'svref             'aref-branch)
(def-clm-fun 'env-interp        #'(lambda (var x) (package-op '<env-interp> var x :clm-real)))

(def-clm-fun 'array-rank        #'(lambda (var x) (package-op '<array-rank> var x :clm-integer)))
(def-clm-fun 'array-dimension   #'(lambda (var x) (package-op '<array-dimension> var x :clm-integer)))
(def-clm-fun 'adjustable-array-p #'(lambda (var x) (declare (ignore var x)) 'nil))
(def-clm-fun 'array-total-size  #'(lambda (var x) (package-op '<array-total-size> var x :clm-integer)))
(def-clm-fun 'length            #'(lambda (var x) (package-op '<length> var x :clm-integer)))
(def-clm-fun 'array-in-bounds-p #'(lambda (var x) (package-op '<array-in-bounds-p> var x :clm-boolean)))

;;; array-element-type


           ;; no progv prog prog*
(def-clm-fun 'progn  #'(lambda (var x)
			 (declare (ignore var))
			 (if (not new-prog) (push :prog new-prog))
			 ;; needed in (run (progn (loop ...))) because the check for the
			 ;; special case initial loop (find-loop-control-info above) uses
			 ;; the empty new-prog to see if it's the first thing run sees.
			 ;; 18-June-98
			 (push `(<comment> progn) new-prog)
			 (let ((last-var nil))
			   (loop for i in (cdr x) do (setf last-var (wf i)))
			   last-var)))

(def-clm-fun 'prog1  #'(lambda (var x)
			 (declare (ignore var))
			 ;; (let ((x 3)) (prog1 x (setf x 32))) -- 9-June-98
			 (push `(<comment> prog1) new-prog)
			 (if (not new-prog) (push :prog1 new-prog))
			 (let ((saved-value (make-compiler-temp))
			       (val (wf (cadr x))))
			   (if val
			       (push `(<setf> ,+setf+ ,(provisional-var saved-value) ,(chained-var val saved-value)) new-prog)
			     (push `(<setf> ,+setf+ ,(provisional-var saved-value) nil) new-prog))
			   (loop for i in (cddr x) do (wf i))
			   saved-value)))

(def-clm-fun 'prog2  #'(lambda (var x)
			 (declare (ignore var))
			 (push `(<comment> prog2) new-prog)
			 (if (not new-prog) (push :prog2 new-prog))
			 (wf (cadr x))
			 (let ((saved-value (make-compiler-temp))
			       (val (wf (caddr x))))
			   (if val
			       (push `(<setf> ,+setf+ ,(provisional-var saved-value) ,(chained-var val saved-value)) new-prog)
			     (push `(<setf> ,+setf+ ,(provisional-var saved-value) nil) new-prog))
			   (loop for i in (cdddr x) do (wf i))
			   saved-value)))

(def-clm-fun 'go     #'(lambda (var x)
			 (declare (ignore var))
			 (push `(<comment> go ,(cadr x)) new-prog)
			 (let ((lab (if (and loop-label-stack
					     #-(or openmcl cmu sbcl clisp (and excl cltl2)) (member (cadr x) '(loop::next-loop loop::end-loop))
					     #+(and excl cltl2) (member (cadr x) '(excl::next-loop excl::end-loop))
					     #+(or cmu openmcl) (member (cadr x) '(ansi-loop::next-loop ansi-loop::end-loop))
					     #+sbcl (member (cadr x) '(sb-loop::next-loop sb-loop::end-loop))
					     #+clisp (member (cadr x) '(system::next-loop system::end-loop))
					     )
					(intern (concatenate 'string (symbol-name (cadr x)) "-" (symbol-name (first loop-label-stack))))
				      (cadr x))))
			   (if (and (not loop-label-stack)
				    ;; outer loop perhaps here and (loop-finish) is a macro that expands into (go end-loop)
				    ;; so  we need to trap that special case here.
				    #-(or openmcl cmu sbcl clisp (and excl cltl2)) (eq (cadr x) 'loop::end-loop)
				    #+(and excl cltl2) (eq (cadr x) 'excl::end-loop)
				    #+(or cmu openmcl) (eq (cadr x) 'ansi-loop::end-loop)
				    #+sbcl (eq (cadr x) 'sb-loop::end-loop)
				    #+clisp (eq (cadr x) 'system::end-loop)
				    )
			       (push '(<loop-finish>) new-prog)
			     (push `(<jump> ,lab) new-prog))
			   nil)))

(def-clm-fun 'let    #'(lambda (var x) (let-branch var x nil)))
(def-clm-fun 'let*   #'(lambda (var x) (let-branch var x t)))

(def-clm-fun 'declare			;this would work in general in run, but mit-loop is buggy (it generates bogus type declarations)
  #'(lambda (var x)
      (declare (ignore var))
      (loop for decl in (cdr x) by #'cdr do
	(if (eq (car decl) 'optimize)
	    (loop for opt in (cdr decl) by #'cdr do
	      (if (eq (car opt) 'safety)
		  (setf *safety* (cadr opt))
		(if (eq (car opt) 'debug)
		    (setf *debug* (cadr opt)))))
	  (if (eq (car decl) 'type)
	      (let* ((arr-typ nil)
		     (typ (if (listp (second decl))
			      (progn
				(if (eq (car (second decl)) 'and) ; sbcl new loop junk
				    (if (or (eq (cadr (second decl)) 'real)
					    (and (cddr (second decl)) (eq (caddr (second decl)) 'real)))
					:real
				      :integer)
				  (progn
				    (setf arr-typ (second (second decl)))
				    ;; (format t "arr-gen: ~A~%" arr-typ)
				    :mus-any-array)))
			    (if (member (second decl) '(float :float :double))
				:real
			      (if (member (second decl) '(integer fixnum bignum :integer))
				  :integer
				(if (or (eq (second decl) 'string)
					(eq (second decl) :string))
				    :string
				  (if (eq (second decl) :mus-any)
				      :mus-any
				    (if (or (eq (second decl) :mus-any*)
					    (eq (second decl) :mus-any-array))
					:mus-any-array
				      (if (or (eq (second decl) :double*)
					      (eq (second decl) :float*))
					  :double-array
					(if (or (eq (second decl) :int*)
						(eq (second decl) :integer*))
					    :integer-array
					  (if (eq (second decl) :boolean)
					      :boolean
					    )))))))))))

		;(format t "declare ~A from ~A~%" typ decl)
		(when typ
		  (loop for var-decl in (cddr decl) by #'cdr do
		    (let* ((varlst (gethash var-decl vars)))
		      (if varlst
			  (if (member typ '(:mus-any :mus-any-array))
			      (progn
				(if (null (varinfo-gen-type varlst))
				    (setf (varinfo-gen-type varlst) typ))
				(if (and (eq typ :mus-any-array)
					 (null (varinfo-arr-gen-type varlst)))
				    (setf (varinfo-arr-gen-type varlst) arr-typ)))
			    (if (null (varinfo-type varlst))
				(setf (varinfo-type varlst) typ)))
			(if (member typ '(:mus-any :mus-any-array))
			    (let* ((var (make-user-var var-decl nil))
				   (info (gethash var vars)))
			      (setf (varinfo-gen-type info) typ)
			      (if (and (eq typ :mus-any-array)
				       (null (varinfo-arr-gen-type info)))
				  (setf (varinfo-arr-gen-type info) arr-typ)))
			  (make-user-var var-decl typ))))))))))
      nil))


;;; the following is just a stop-gap for users of cmu-loop -- it generates

;;; macrolets within the lambda form that loop expands into, but these

;;; macrolets appear to me to be very simple.


(defun submac (name body code)
  (if (listp code)
      (if (eq (first code) name)
	  body
	(loop for x in code collect (submac name body x)))
    code))

(def-clm-fun 'macrolet #'(lambda (var x)
			   (declare (ignore var))
			   (submac (first (second x)) (third (second x)) (third x))))

;;; I suppose we could make a list of macrolet macros with args, then form

;;; the LETs that are equivalent thereto, then do the substitution with

;;; args in submac -- surely someone has already written such a function.


;;; ACL might use (it did in 4.3) multiple-value-setq during macroexpansion of psetf, so...


(def-clm-fun 'multiple-value-setq #'(lambda (var x)
				      ;; assume simplest case (sq (var) val) -- this is just a stop-gap
				      (setf-branch var `(setf ,(first (second x)) ,(third x)))))

;;; MCL similarly uses multiple-value-bind in psetf and others


(def-clm-fun 'multiple-value-bind #'(lambda (var x)
				      ;; assume simplest case (mb (var) val body) -- this is just a stop-gap
				      (let-branch var `(let ((,(first (second x)) ,(third x))) ,(fourth x)) nil)))

#+excl (def-clm-fun 'excl::loop-really-desetq #'(lambda (var x) (setf-branch var x)))
#+(or cmu openmcl) (def-clm-fun 'ansi-loop::loop-really-desetq #'(lambda (var x) (setf-branch var x)))
;;; #+sbcl (def-clm-fun 'sb-loop::loop-really-desetq #'(lambda (var x) (setf-branch var x)))

;;; this gets package lock error in version 1.4.3

;;; needed for sum keyword in loop (not needed in clisp?)



(defun to-bignum (num arr loc)
  ;; try to kludge up 48-bit numbers passed to C
  (let ((anum (abs num))
	(sign (if (< num 0) 1 0)))
    (setf (aref arr loc) (logand anum #xffffff))
    (setf (aref arr (+ loc 1)) (logior (ash anum -24) (ash sign 25)))))

(defconstant +aref-type+ 0)
(defconstant +aref-iblock+ 1)
(defconstant +aref-rblock+ 2)
(defconstant +aref-size+ 3)
(defconstant +aref-dims+ 4)

(defvar *c-file* nil) ; opened in defins.lisp
(defvar variable-name (make-hash-table))
(defvar c-names (make-hash-table))
(defvar ok-chars nil)
(defvar ok-numbers nil)

#+sbcl (defgeneric gen-load (gen i r datai datar))
#+sbcl (defgeneric gen-unload (gen i r datai datar))
#+sbcl (defgeneric gen-size (gen))
#+sbcl (defgeneric gen-make (gen stream result iloc indent ctr))
#+sbcl (defgeneric gen-reflect (gen key val))

(defun gen-make-1 (type-or-list stream result iloc indent ctr)
  (if (symbolp type-or-list)
      (gen-make (make-instance type-or-list) stream result iloc indent ctr)
    (loop for type in type-or-list do
      (if (not (eq type :mus-any))
	  (gen-make (make-instance type) stream result iloc indent ctr)))))

(defun gen-reflect-1 (type-or-list key val)
  (if (symbolp type-or-list)
      (gen-reflect (make-instance type-or-list) key val)
    (loop for type in type-or-list do
      (if (not (eq type :mus-any))
	  (gen-reflect (make-instance type) key val)))))

(defvar empty-name-ctr 0)

(defun lisp->c-name (n)
  ;; make sure n is a legal C variable name, and if not translate it.
  ;; Allow a..z A..Z 0..9 _.  Change all other characters to _.
  ;; save these changed names so we only do this tedious translation once.
  ;; In case the new name already exists (i.e. user has variables "sin<" and "sin>"),
  ;; keep adding decorative characters.
  (if (string-equal n "T")
      "true"
    (if (string-equal n "NIL")
	"false"
      (progn
	(when (and (stringp n)
		   (= (length n) 0))
	  (setf n (format nil "_clm_empty_~D" empty-name-ctr))
	  (incf empty-name-ctr))
	(if (null ok-chars)
	    (setf ok-chars '(#\A #\a #\B #\b #\C #\c #\D #\d #\E #\e #\F #\f
			     #\G #\g #\H #\h #\I #\i #\J #\j #\K #\k #\L #\l
			     #\M #\m #\N #\n #\O #\o #\P #\p #\Q #\q #\R #\r
			     #\S #\s #\T #\t #\U #\u #\V #\v #\W #\w #\X #\x
			     #\Y #\y #\Z #\z #\_)))
	(if (null ok-numbers)
	    (setf ok-numbers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
	(or (gethash n variable-name)
	    (let ((new-name (substitute-if #\_ #'(lambda (n) (and (not (member n ok-chars)) (not (member n ok-numbers)))) n)))
	      (if (member (elt new-name 0) ok-numbers)
		  ;; can't start with "_" or a number
		  (setf new-name (concatenate 'string "_ext_" new-name))
		)
	      #+(and excl cltl2) (if (eq excl:*current-case-mode* :case-sensitive-lower) (setf new-name (string-upcase new-name)))
	      (loop while (gethash new-name c-names) do
		    (setf new-name (concatenate 'string new-name "_")))
	      (setf (gethash n variable-name) new-name)
	      (setf (gethash new-name c-names) n)
	      new-name)))
      )))

(defun lc (n) (lisp->c-name (symbol-name n)))
(defun lc2 (n) (lisp->c-name (symbol-name (second n))))

(defun lc-num-ref (n &optional type)
  (if n
      (if (numberp n)
	  (if (integerp n)
	      n
	    (let ((*read-default-float-format* 'single-float)) ; I think this is the default

	      ;; this silliness is trying to get rid of the exponent d0 or S0 that confuses C.  'twould be nice
	      ;;  if lisp had a way to suppress the exponent within ~A processing.
	      ;; another gotcha that Michael Edwards found: you can set *read-default-float-format* to 'double-float
	      ;;   and get f0 as the exponent!

	      (format nil "~A" (coerce n 'single-float))
	      ))
	(if (eq n t)
	    1
	  (if (or (not (listp n)) (not (eq (car n) :var)))
	      (progn
		(warn "what's ~A? " n)
		n)
	    (let* ((l-name (second n))
		   (c-name (lc l-name))
		   (depth (third n))
		   (lst (gethash (cadr n) vars)))
	      ;; this is a value reference, so decide based on depth etc
	      (if (member (varinfo-type lst) '(:real :integer :bignum :boolean))
		  ;; user insists it's a float or int, so we've assumed that in the memory layout
		  (if (and (varinfo-shadowed lst)
			   (> depth 0))
		      (format nil "~A_~D" c-name depth)
		    (format nil "~A" c-name))
	        ;;i.e. var = ((int *)(clm_int+loc)) or ((double *)(clm_double+loc))
		(if (or (member (varinfo-type lst) '(:clm-integer :clm-boolean :clm-real :string))
			(varinfo-gen-type lst))
		    c-name
	          ;; val is declared float var or int var, is a temp, cannot have depth
		  (if (eq type :integer)
		      (format nil "clm_int[~A + ~D + 1]" c-name (* 2 depth))
		    (if (eq type :real)
			(format nil "clm_double[~A_r + ~D]" c-name depth)
		      (progn
			(if *clm-debug* (format t "untyped: ~A~%" l-name))
			(if (zerop depth)
			    (format nil "((clm_int[~A] == ~A) ? clm_double[~A_r] : (double)(clm_int[~A + 1]))"
				    c-name +real+ c-name c-name)
			  (format nil "((clm_int[~A + ~D] == ~A) ? clm_double[~A_r + ~D] : (double)(clm_int[~A + ~D + 1]))"
				  c-name (* 2 depth) +real+ c-name depth c-name (* 2 depth)))))))))))
	)
    0))

(defun setf-type->c-op (setf-type)
  (if (= setf-type +setf+) "="
    (if (= setf-type +incf+) "+="
      "-=")))

(defun setf-type->lisp-op (setf-type)
  (if (= setf-type +setf+) "setf"
    (if (= setf-type +incf+) "incf"
      "decf")))

(defun setf-op (var val)
  (format nil "if (type == ~D)~%    ~A = ~A;~%  else if (type == ~D)~%      ~A += (~A);~%    else ~A -= (~A)"
	  +setf+ var val +incf+ var val var val))

(defun pointer-var (var)
  (member (varinfo-type var) (list :string :float-array :double-array :integer-array :mus-any :mus-any-array)))

(defun lc-set (setf-type var val &optional val-type) ;var should be triple, val can be triple or constant (a string for example)
  (if (or (not var)
	  (not (listp var))
	  (not (eq (car var) :var)))
      (warn "adr ref of ~A?" var)
    (let* ((l-name (second var))
	   (c-name (lc l-name))
	   (depth (third var))
	   (lst (gethash l-name vars))
	   (op (setf-type->c-op setf-type)))
      (if (constantp val)
	  ;; val is often a string constant!
	  (if (member (varinfo-type lst) '(:clm-integer :clm-boolean :clm-real :string :float-array :double-array :integer-array :mus-any :mus-any-array))
	      (format nil "~A ~A ~A;" c-name op (if (or (stringp val) (numberp val)) val (if (eq val t) 1 0)))

	    (if (member (varinfo-type lst) '(:integer :real :bignum :boolean))
		(if (and (varinfo-shadowed lst)
			 (> depth 0))
		    (format nil "~A_~D ~A ~A;" c-name depth op (if (or (stringp val) (numberp val)) val (if (eq val t) 1 0)))
		  (format nil "~A ~A ~A;" c-name op (if (or (stringp val) (numberp val)) val (if (eq val t) 1 0))))

	      (if (integerp val)
		  (format nil "clm_int[~A + ~D] = ~A;~%  clm_int[~A + ~D + 1] ~A ~A;"
			  c-name (* 2 depth) (if (eq val-type :pboolean) +no-type+ +integer+) c-name (* 2 depth) op val)

		(if (or (eq val t) (eq val nil) (eq val-type :pboolean))

		    (if (varinfo-gen-type lst)
			(format nil "~A = NULL;~%" c-name)

		      (format nil "clm_int[~A + ~D] = ~A;~%  clm_int[~A + ~D + 1] ~A ~A;"
			      c-name (* 2 depth) +no-type+ c-name (* 2 depth) op (if (eq val t) 1 (if (eq val nil) 0 val))))

		  (if (and val-type (eq val-type :integer))
		      (format nil "clm_int[~A + ~D] = ~A;~%  clm_int[~A + ~D + 1] ~A ~A;"
			  c-name (* 2 depth) +integer+ c-name (* 2 depth) op val)

		    (format nil "~Aclm_int[~A + ~D] = ~A;~%  clm_double[~A_r+~D] ~A ~A;"
			    (if (= setf-type +setf+)
				""
			      ;; gotta float if currently int
			      (format nil "if (clm_int[~A + ~D] == ~A) clm_double[~A_r + ~D] = (double)clm_int[~A + ~D + 1]; "
				      c-name (* 2 depth) +integer+ c-name depth c-name (* 2 depth)))
			    c-name (* 2 depth) +real+ c-name depth op val))))))
	(let* ((l-val-name (second val))
	       (ldepth (third val))
	       (val-lst (gethash l-val-name vars))
	       (lcl (lc l-val-name)))

;	  (format t "~A: ~A, ~A: ~A~%" l-val-name (varinfo-type val-lst) l-name (varinfo-type lst))

	  (if (or (pointer-var lst)
		  (varinfo-gen-type lst))
	      (format nil "~A = ~A;" c-name lcl)

	  (if (member (varinfo-type lst) '(:clm-integer :clm-boolean :clm-real))
	      (format nil "~A ~A ~A;"
		      c-name op
		      (if (and (eq (varinfo-type lst) :clm-boolean) ; look for pointer treated as boolean
			       (or (varinfo-gen-type val-lst)
				   (pointer-var val-lst)))
			  (format nil "(bool)~A" (lc-bool val))
			(lc-num-ref val)))

	    (if (member (varinfo-type lst) '(:integer :real :bignum :boolean))
		(if (and (varinfo-shadowed lst)
			 (> depth 0))
		    (format nil "~A_~D ~A ~A;"
			    c-name depth op
			    (if (and (eq (varinfo-type lst) :clm-boolean)
				     (or (varinfo-gen-type val-lst)
					 (pointer-var val-lst)))
				(format nil "(bool)~A" (lc-bool val))
			      (lc-num-ref val)))
		  (format nil "~A ~A ~A;"
			  c-name op
			  (if (and (eq (varinfo-type lst) :clm-boolean)
				   (or (varinfo-gen-type val-lst)
				       (pointer-var val-lst)))
			      (format nil "(bool)~A" (lc-bool val))
			    (lc-num-ref val))))

	      (if (member (varinfo-type val-lst) '(:integer :clm-integer))
		  (format nil "clm_int[~A + ~D] = ~A;~%  clm_int[~A + ~D + 1] ~A ~A;"
			  c-name (* 2 depth) +integer+ c-name (* 2 depth) op (lc-num-ref val))

		(if (member (varinfo-type val-lst) '(:boolean :clm-boolean))
		    (format nil "clm_int[~A + ~D] = ~D;~%  clm_int[~A + ~D + 1] ~A ~A;"
			  c-name (* 2 depth) +no-type+ c-name (* 2 depth) op (lc-num-ref val))

		  (if (member (varinfo-type val-lst) '(:real :clm-real))
		      (format nil "clm_int[~A + ~D] = ~A;~%  clm_double[~A_r+~D] ~A ~A;"
			      c-name (* 2 depth) +real+ c-name depth op (lc-num-ref val))

		    ;; general case, copy entire val into var
		    ;; float here? (i.e. first copy int to real in val?
		    (progn
		      (if *clm-debug* (format t "untyped: ~A~%" l-val-name))

		  (if (or (pointer-var val-lst)
			  (varinfo-gen-type val-lst))
		      (format nil "clm_int[~A + ~D] = 0;~%  ~
                                   clm_int[~A + ~D + 1] = ((~A) ? 1 : 0);~%  ~
                                   clm_double[~A_r + ~D] = 0.0;"
			      c-name (* 2 depth)
			      c-name (* 2 depth) lcl
			      c-name depth)
		    (format nil "clm_int[~A + ~D] = clm_int[~A + ~D];~%  ~
                                 clm_int[~A + ~D + 1] ~A clm_int[~A + ~D + 1];~%  ~
                                 clm_double[~A_r + ~D] ~A clm_double[~A_r + ~D];"
			    c-name (* 2 depth) lcl (* 2 ldepth)
			    c-name (* 2 depth) op lcl (* 2 ldepth)
			    c-name depth op lcl ldepth)))))))))))))
  )

(defun lc-type (n)
  (if n
      (if (numberp n)
	  (if (integerp n)
	      :integer
	    :real)
	(if (listp n)
	    (varinfo-type (gethash (cadr n) vars))
	  n))))

(defun clean-arg (arg)
  (if (and arg (listp arg))
      (second arg)
    arg))

(defmacro <loop-finish> ()
  (format *c-file* "  goto RUN_ALL_DONE;~%")
  nil)

#+excl(defconstant SIGINT_C 12)
#+(and excl acl-50)
  (progn
    (ff:defun-foreign-callable c-sigint () (setf common-tones::*interrupted* 1))
    (ff:register-foreign-callable 'c-sigint SIGINT_C))

;;; sbcl traps sigint: doc/manual/ffi.texinfo

;;; C-C seem to work in clisp, cmucl, sbcl (linux) (can't test acl)


#+excl (defconstant CLM-FATAL-WRITE-ERROR 7)
#+(and excl acl-50)
  (progn
    (ff:defun-foreign-callable cwriteerror () (error "fatal write error"))
    (ff:register-foreign-callable 'cwriteerror CLM-FATAL-WRITE-ERROR))



(defmacro <setf> (setf-type result arg &optional restype)
  ;; setf-type is setf/incf/decf choice (not a variable type!)
  (let ((dat (gethash (second result) vars)))
    (if (varinfo-refd dat)
	(format *c-file* "  ~A~80,1T/* (~A ~A ~A) */~%"
		(lc-set setf-type result arg restype)
		(if (= setf-type +setf+) "setf"
		  (if (= setf-type +incf+) "incf"
		    "decf"))
		(clean-arg result)
		(clean-arg arg))))
  nil)

(defun lc-check-type (res arg run-type name &optional consf varf)
  (if (and arg (listp arg) (eq (first arg) :var))
      (let* ((var (gethash (second arg) vars))
	     (typ (varinfo-type var))
	     (depth (third arg)))
	(if typ
	    (format *c-file* "  ~A~80,1T/* (~A ~A) */~%"
		    (lc-set +setf+ res (funcall varf typ) :pboolean) name (clean-arg arg))
	  (if (listp run-type)
	      (format *c-file* "  ~A~80,1T/* (~A ~A) */~%"
		      (let ((args (loop for rt in run-type collect (list (lc2 arg) (* 2 depth) rt))))
			(lc-set +setf+ res
				(format nil "(~:{(clm_int[~A + ~D] == ~D)~:^ || ~})"
					args) :pboolean)) ;cltl2 p 603 and 606
		  name (clean-arg arg))
	    (format *c-file* "  ~A~80,1T/* (~A ~A) */~%"
		    (lc-set +setf+ res
			    (format nil "(clm_int[~A + ~D] == ~D)"
				    (lc2 arg) (* 2 depth) run-type) :pboolean)
		    name (clean-arg arg)))))
    (format *c-file* "  ~A~80,1T/* (~A ~A) */~%"
	    (lc-set +setf+ res (funcall consf arg) :pboolean) name (clean-arg arg))))

(defmacro <integer?> (res arg)
  (lc-check-type res arg +integer+ "integer?"
		 #'(lambda (arg) (if (and arg (integerp arg)) "true" "false"))
		 #'(lambda (typ) (if (member typ '(:clm-integer :integer)) "true" "false")))
  nil)

(defmacro <bignum?> (res arg)
  (lc-check-type res arg +integer+ "bignum?"
		 #'(lambda (arg) (if (and arg (integerp arg)) "true" "false"))
		 #'(lambda (typ) (if (member typ '(:clm-integer :integer :bignum)) "true" "false")))
  nil)

(defmacro <float?> (res arg)
  (lc-check-type res arg +real+ "float?"
		 #'(lambda (arg) (if (and arg (numberp arg) (not (integerp arg))) "true" "false"))
		 #'(lambda (typ) (if (member typ '(:clm-real :real)) "true" "false")))
  nil)

(defmacro <number?> (res arg)
  (lc-check-type res arg (list +real+ +integer+) "number?"
		 #'(lambda (arg) (if (and arg (numberp arg)) "true" "false"))
		 #'(lambda (typ) (if (and typ (not (eq typ :clm-boolean))) "true" "false")))
  nil)

(defmacro <eq> (res arg1 &optional arg2)
  (let ((arg1dat nil)
	(arg2dat nil)
	(possible-eq nil))
    (when (and arg1 arg2
	       (listp arg1) (listp arg2)
	       (eq (car arg1) :var) (eq (car arg2) :var))
      (setf arg1dat (gethash (second arg1) vars))
      (setf arg2dat (gethash (second arg2) vars))
      (setf possible-eq (and (null (varinfo-type arg1dat))
			     (null (varinfo-type arg2dat)))))
    (if possible-eq
	(let ((cname1 (lc2 arg1))
	      (cname2 (lc2 arg2))
	      (depth1 (* 2 (third arg1)))
	      (depth2 (* 2 (third arg2))))
	  (format *c-file* "  ~A = ((clm_int[~A + ~D] == clm_int[~A + ~D]) && (clm_int[~A + ~D + 1] == clm_int[~A + ~D + 1]));~80,1T/* (eq ~A ~A) */~%"
		  (lc2 res)
		  cname1 depth1 cname2 depth2 cname1 depth1 cname2 depth2
		  (second arg1) (second arg2)))
      (if (and (varinfo-gen-type arg1dat)
	       (varinfo-gen-type arg2dat))
	  (format *c-file* "  ~A = (~A == ~A);~%" (lc2 res) (lc2 arg1) (lc2 arg2))
	(format *c-file* "  ~A = ~A;~80,1T/* (eq ~A~A~A) */~%"
		(lc2 res) (if (or (not arg2) (eq arg1 arg2)) "true" "false") arg1 (if arg2 " " "") (or arg2 "")))))
  nil)

(defmacro <eql> (res arg1 arg2)
  (if (and arg1 arg2 (listp arg1) (listp arg2))
      (let ((cname1 (lc2 arg1))
	    (cname2 (lc2 arg2))
	    (depth1 (* 2 (third arg1)))
	    (depth2 (* 2 (third arg2)))
	    (arg1dat (gethash (second arg1) vars))
	    (arg2dat (gethash (second arg2) vars)))
	(if (and (varinfo-gen-type arg1dat)
		 (varinfo-gen-type arg2dat))
	    (format *c-file* "  ~A = (~A == ~A);~%" (lc2 res) (lc2 arg1) (lc2 arg2))
	  (progn
	    (format *c-file* "  ~A = ((clm_int[~A + ~D] == clm_int[~A + ~D]) &&~80,1T/* (eql ~A ~A) */~%"
		    (lc2 res) cname1 depth1 cname2 depth2 (second arg1) (second arg2))
	    (format *c-file* "        ((clm_int[~A + ~D + 1] == clm_int[~A + ~D + 1]) ||~%" cname1 depth1 cname2 depth2)
	    (format *c-file* "         ((clm_int[~A + ~D] == ~D) && (~A == ~A))));~%"
		    cname1 depth1 +real+ (lc-num-ref arg1) (lc-num-ref arg2)))))
    (format *c-file* "  ~A = ~A;~80,1T/* (eql ~A ~A) */~%"
	    (lc2 res)
	    (if (eql arg1 arg2) "true" "false")
	    arg1 arg2)))

;; equal checks for isomorphism, but not in arrays
;; equalp checks arrays too and apparently recurses through structs

;;; the arithmetic ops need to try to maintain the arg types


(defun checked-op1 (result opi opr arg)
  (if (or (integerp arg)
	  (and (listp arg)
	       (let ((lst (gethash (second arg) vars)))
		 (and lst (member (varinfo-type lst) '(:integer :clm-integer :clm-boolean :bignum :boolean))))))
      (format *c-file* "  ~A~%" (funcall opi result arg))
    (if (or (numberp arg)
	    (and (listp arg)
		 (let ((lst (gethash (second arg) vars)))
		   (and lst (member (varinfo-type lst) '(:real :clm-real))))))
	(format *c-file* "  ~A~%" (funcall opr result arg))
      (format *c-file* "  if (clm_int[~A + ~D] == ~D)~%    {~A}~%  else {~A}~%"
	      (lc2 arg) (* 2 (third arg)) +integer+
	      (funcall opi result arg)
	      (funcall opr result arg)))))

(defun checked-opall (result opi opr args)
  (if (every #'(lambda (arg) (or (integerp arg)
				 (and (listp arg)
				      (let ((lst (gethash (second arg) vars)))
					(and lst (member (varinfo-type lst) '(:integer :clm-integer :clm-boolean :bignum :boolean)))))))
	     args)
      (format *c-file* "  ~A~%" (funcall opi result args))
    (if (some #'(lambda (arg) (or (and (numberp arg) (not (integerp arg)))
				  (and (listp arg)
				       (let ((lst (gethash (second arg) vars)))
					 (and lst (member (varinfo-type lst) '(:real :clm-real)))))))
	       args)
	(format *c-file* "  ~A~%" (funcall opr result args))
      (format *c-file* "  if (~{(~A)~^ && ~})~%    {~A}~%  else {~A}~%"
	      (loop for arg in args collect
		(if (listp arg)
		    (let ((lst (gethash (second arg) vars)))
		      (if lst
			  (if (member (varinfo-type lst) '(:integer :clm-integer :clm-boolean :bignum :boolean))
			      "1"
			    (if (member (varinfo-type lst) '(:real :clm-real))
				"0"
			      (format nil "clm_int[~A + ~D] == ~D" (lc2 arg) (* 2 (third arg)) +integer+)))))
		  (if (integerp arg)
		      "1"
		    "0")))
	      (funcall opi result args)
	      (funcall opr result args)))))

(defmacro <negate> (result arg)
  (format *c-file* "~80,1T/* (- ~A) */~%" (clean-arg arg))
  (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "-(~A)" (lc-num-ref arg :integer)) :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "-(~A)" (lc-num-ref arg :real)) :real))
	       arg)
  nil)

(defmacro <abs> (result arg)
  (format *c-file* "~80,1T/* (abs ~A) */~%" (clean-arg arg))
  (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "abs(~A)" (lc-num-ref arg :integer)) :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "fabs(~A)" (lc-num-ref arg :real)) :real))
	       arg)
  nil)

(defun cleanse (args)
  (loop for arg in args collect
    (if (and arg (listp arg))
	(second arg)
      arg)))


(defmacro <add> (result &rest args)
  (format *c-file* "~80,1T/* (+~{ ~(~A~)~}) */~%" (cleanse args))
  (checked-opall result
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ + ~})" (loop for arg in evars collect (lc-num-ref arg :integer))) :integer))
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ + ~})" (loop for arg in evars collect (lc-num-ref arg)))))
		 args)
  nil)

(defmacro <subtract> (result &rest args)
  (format *c-file* "~80,1T/* (-~{ ~(~A~)~}) */~%" (cleanse args))
  (checked-opall result
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ - ~})" (loop for arg in evars collect (lc-num-ref arg :integer))) :integer))
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ - ~})" (loop for arg in evars collect (lc-num-ref arg)))))
		 args)
  nil)

(defmacro <multiply> (result &rest args)
  (format *c-file* "~80,1T/* (*~{ ~(~A~)~}) */~%" (cleanse args))
  (checked-opall result
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ * ~})" (loop for arg in evars collect (lc-num-ref arg :integer))) :integer))
		 #'(lambda (res evars) (lc-set +setf+ res (format nil "(~{(~A)~^ * ~})" (loop for arg in evars collect (lc-num-ref arg)))))
		 args)
  nil)

(defmacro <divide> (result val &rest args)
  ;; here we will assume real output in all cases
  (if args
      (let ((argl (format nil "((double)~A / (double)(~{(~A)~^ * ~}))" (lc-num-ref val) (loop for arg in args collect (lc-num-ref arg)))))
	(format *c-file* "  ~A~%~80,1T/* (/ ~A~{ ~(~A~)~}) */~%"
		(lc-set +setf+ result argl)
		(clean-arg val) (cleanse args)))
    (format *c-file* "  ~A~80,1T/* (/ ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "(1.0 / (double)~A)"
			    (lc-num-ref val)))
	    (clean-arg val)))
  nil)

(defmacro <max> (result &rest args)
  (if (= (length args) 1)
      (format *c-file* "  ~A~80,1T/* (max ~A) */~%"
	      (lc-set +setf+ result (car args))
	      (clean-arg (first args)))
    (if (= (length args) 2)
	(format *c-file* "  ~A~80,1T/* (max ~A ~A) */~%"
		(lc-set +setf+ result
			(format nil "((~A > ~A) ? ~A : ~A)"
				(lc-num-ref (first args)) (lc-num-ref (second args))
				(lc-num-ref (first args)) (lc-num-ref (second args))))
		(clean-arg (first args))
		(clean-arg (second args)))
      (progn
	(format *c-file* "  ~A~80,1T/* (max~{ ~A~}) */~%"
		(lc-set +setf+ result (first args))
		(cleanse args))
	(loop for arg in (cdr args) do
	  (format *c-file* "  if (~A > ~A) {~A}~%" (lc-num-ref arg) (lc-num-ref result) (lc-set +setf+ result arg))))))
  nil)

(defmacro <min> (result &rest args)
  (if (= (length args) 1)
      (format *c-file* "  ~A~80,1T/* (min ~A) */~%"
	      (lc-set +setf+ result (car args))
	      (clean-arg (first args)))
    (if (= (length args) 2)
	(format *c-file* "  ~A~80,1T/* (min ~A ~A) */~%"
		(lc-set +setf+ result
			(format nil "((~A < ~A) ? ~A : ~A)"
				(lc-num-ref (first args)) (lc-num-ref (second args))
				(lc-num-ref (first args)) (lc-num-ref (second args))))
		(clean-arg (first args))
		(clean-arg (second args)))
      (progn
	(format *c-file* "  ~A~80,1T/* (min~{ ~A~}) */~%"
		(lc-set +setf+ result (first args))
		(cleanse args))
	(loop for arg in (cdr args) do
	  (format *c-file* "  if (~A < ~A) {~A}~%" (lc-num-ref arg) (lc-num-ref result) (lc-set +setf+ result arg))))))
  nil)

(defmacro <log> (result arg &optional base)
  ;; assume base=e for "log" function
  (if base
      (if (constantp base)
	  (format *c-file* "  ~A~80,1T/* (log ~A ~A) */~%"
		  (lc-set +setf+ result
			  (format nil "log((double)(~A)) / ~A"
				  (lc-num-ref arg)
				  (lc-num-ref (log base))))
		  (clean-arg arg)
		  base)
	  (format *c-file* "  ~A~80,1T/* (log ~A ~A) */~%"
		  (lc-set +setf+ result
			  (format nil "log((double)(~A)) / log((double)(~A))"
				  (lc-num-ref arg)
				  (lc-num-ref base)))
		  (clean-arg arg)
		  (clean-arg base)))
    (format *c-file* "  ~A~80,1T/* (log ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "log((double)(~A))" (lc-num-ref arg)))
	    (clean-arg arg)))
  nil)

(defmacro <sin> (result arg)
  (format *c-file* "  ~A~80,1T/* (sin ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "sin((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <cos> (result arg)
  (format *c-file* "  ~A~80,1T/* (cos ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "cos((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <tan> (result arg)
  (format *c-file* "  ~A~80,1T/* (tan ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "tan((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <asin> (result arg)
  (format *c-file* "  ~A~80,1T/* (asin ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "asin((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <acos> (result arg)
  (format *c-file* "  ~A~80,1T/* (acos ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "acos((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <atan> (result arg &optional arg2)
  (if arg2
      (format *c-file* "  ~A~80,1T/* (atan ~A ~A) */~%"
	      (lc-set +setf+ result
		      (format nil "atan2((double)(~A),(double)(~A))" (lc-num-ref arg) (lc-num-ref arg2)))
	      (clean-arg arg) (clean-arg arg2))
    (format *c-file* "  ~A~80,1T/* (atan ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "atan((double)(~A))" (lc-num-ref arg)))
	    (clean-arg arg)))
  nil)

(defmacro <sinh> (result arg)
  (format *c-file* "  ~A~80,1T/* (sinh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "sinh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <cosh> (result arg)
  (format *c-file* "  ~A~80,1T/* (cosh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "cosh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <tanh> (result arg)
  (format *c-file* "  ~A~80,1T/* (tanh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "tanh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <asinh> (result arg)
  (format *c-file* "  ~A~80,1T/* (asinh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "asinh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <acosh> (result arg)
  (format *c-file* "  ~A~80,1T/* (acosh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "acosh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <atanh> (result arg)
  (format *c-file* "  ~A~80,1T/* (atanh ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "atanh((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <atan2> (result argy argx)
  (format *c-file* "  ~A~80,1T/* (atan2 ~A ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "atan2((double)(~A),(double)(~A))" (lc-num-ref argy) (lc-num-ref argx)))
	  (clean-arg argy) (clean-arg argx))
  nil)

(defmacro <erf> (result arg)
  (format *c-file* "  ~A~80,1T/* (erf ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "erf((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <erfc> (result arg)
  (format *c-file* "  ~A~80,1T/* (erfc ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "erfc((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <lgamma> (result arg)
  (format *c-file* "  ~A~80,1T/* (lgamma ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "lgamma((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <bes-j0> (result arg)
  (format *c-file* "  ~A~80,1T/* (bes-j0 ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "j0((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <bes-j1> (result arg)
  (format *c-file* "  ~A~80,1T/* (bes-j1 ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "j1((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <bes-jn> (result arg arg2)
  (format *c-file* "  ~A~80,1T/* (bes-jn ~A ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "jn((double)(~A),(double)(~A))" (lc-num-ref arg) (lc-num-ref arg2)))
	  (clean-arg arg) (clean-arg arg2))
  nil)

(defmacro <bes-y0> (result arg)
  (format *c-file* "  ~A~80,1T/* (bes-y0 ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "y0((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <bes-y1> (result arg)
  (format *c-file* "  ~A~80,1T/* (bes-y1 ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "y1((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <bes-yn> (result arg arg2)
  (format *c-file* "  ~A~80,1T/* (bes-yn ~A ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "yn((double)(~A),(double)(~A))" (lc-num-ref arg) (lc-num-ref arg2)))
	  (clean-arg arg) (clean-arg arg2))
  nil)

(defmacro <bes-i0> (result arg)
  (format *c-file* "  ~A~80,1T/* (bes-i0 ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "mus_bessi0((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <signum> (result arg)
  ;; result can be int or float as can arg
  (format *c-file* "~80,1T/* (signum ~A) */~%" (clean-arg arg))
  (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "((~A > 0) ? 1 : (~A == 0) ? 0 : -1)"
						 (lc-num-ref arg :integer)
						 (lc-num-ref arg :integer))
			   :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "((~A > 0.0) ? 1.0 : (~A == 0.0) ? 0.0 : -1.0)"
						 (lc-num-ref arg :real)
						 (lc-num-ref arg :real))
			   :real))
	       arg)
  nil)

(defmacro <random> (result arg)
  ;; changed 16-Mar-00 to try to retain arg type at request of Michael Edwards
  (format *c-file* "~80,1T/* (random ~A) */~%" (clean-arg arg))
  (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "(mus_long_t)(mus_frandom((double)(~A)))" (lc-num-ref arg :integer)) :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "mus_frandom((double)(~A))" (lc-num-ref arg :real)) :real))
	       arg)
  nil)

(defmacro <centered-random> (result arg)
  (format *c-file* "  ~A = mus_random((double)(~A));~80,1T/* (centered-random ~A) */~%"
	  (lc2 result)
	  (lc-num-ref arg)
	  (clean-arg arg))
  nil)

(defmacro <expt> (result base arg)
  (format *c-file* "  ~A~80,1T/* (expt ~A ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "pow((double)(~A), (double)(~A))" (lc-num-ref base) (lc-num-ref arg)))
	  (clean-arg base) (clean-arg arg))
  nil)

(defmacro <exp> (result arg)
  ;; (exp a) = e^a
  (format *c-file* "  ~A~80,1T/* (exp ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "exp((double)(~A))"
			  (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

#|
(defmacro <float> (result arg)
  (format *c-file* "  ~A~80,1T/* (float ~A) */~%"
	  (lc-set +setf+ result (format nil "(double)(~A)"(lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <double-float> (result arg)
  (format *c-file* "  ~A~80,1T/* (double ~A) */~%"
	  (lc-set +setf+ result (format nil "(double)(~A)"(lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <double> (result arg)
  (format *c-file* "  ~A~80,1T/* (double ~A) */~%"
	  (lc-set +setf+ result (format nil "(double)(~A)"(lc-num-ref arg)))
	  (clean-arg arg))
  nil)
|#

(defmacro <floor> (result arg &optional div-1)
  (if div-1
      (format *c-file* "  ~A~80,1T/* (floor ~A ~A) */~%"
	      (lc-set +setf+ result
		      (format nil "(mus_long_t)floor((double)(~A) / (double)(~A))" (lc-num-ref arg) (lc-num-ref div-1)))
	      (clean-arg arg) (clean-arg div-1))
    (format *c-file* "  ~A~80,1T/* (floor ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "(mus_long_t)floor((double)(~A))" (lc-num-ref arg)))
	    (clean-arg arg)))
  nil)

(defmacro <ceiling> (result arg &optional div-1)
  (if div-1
      (format *c-file* "  ~A~80,1T/* (ceiling ~A ~A) */~%"
	      (lc-set +setf+ result
		      (format nil "(mus_long_t)ceil((double)(~A) / (double)(~A))" (lc-num-ref arg) (lc-num-ref div-1)))
	      (clean-arg arg) (clean-arg div-1))
    (format *c-file* "  ~A~80,1T/* (ceiling ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "(mus_long_t)ceil((double)(~A))" (lc-num-ref arg)))
	    (clean-arg arg)))
  nil)

(defmacro <round> (result arg &optional div-1)
  ;; round to nearest even number if .5 (otherwise round to nearest -- sigh...)
  (if div-1
      (format *c-file* "  ~A~80,1T/* (round ~A ~A) */~%"
	      (lc-set +setf+ result
		      (format nil "(mus_long_t)floor((double)(~A) / (double)(~A) + .5)" (lc-num-ref arg) (lc-num-ref div-1)))
	      (clean-arg arg) (clean-arg div-1))
    (format *c-file* "  ~A~80,1T/* (round ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "(mus_long_t)floor((double)((~A) + .5))" (lc-num-ref arg)))
	    (clean-arg arg)))
  (format *c-file* "  if ( (((~A) - (~A)) == .5) && (((mus_long_t)(~A)) & 1) ) ~A;~%"
	  (lc-num-ref result)
	  (lc-num-ref arg)
	  (lc-num-ref result)
	  (lc-set +decf+ result "1"))
  nil)

(defmacro <truncate> (result arg &optional div-1)
  (if  div-1
      (format *c-file* "  ~A~80,1T/* (truncate ~A ~A) */~%"
	      (lc-set +setf+ result
		      (format nil "(mus_long_t)floor((double)(~A) / (double)(~A))" (lc-num-ref arg) (lc-num-ref div-1)))
	      (clean-arg arg) (clean-arg div-1))
    (format *c-file* "  ~A~80,1T/* (truncate ~A) */~%"
	    (lc-set +setf+ result
		    (format nil "(mus_long_t)floor((double)(~A))" (lc-num-ref arg)))
	    (clean-arg arg)))
  (format *c-file* "  if ((~A) < 0.0) ~A;~%"
	  (lc-num-ref result)
	  (lc-set +incf+ result "1"))
  nil)

(defmacro <add-1> (result arg)
  (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "~A + 1" (lc-num-ref arg :integer)) :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "~A + 1.0" (lc-num-ref arg :real)) :real))
	       arg)
  nil)

(defmacro <sub-1> (result arg)
    (checked-op1 result
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "~A - 1" (lc-num-ref arg :integer)) :integer))
	       #'(lambda (res var)
		   (declare (ignore res var))
		   (lc-set +setf+ result (format nil "~A - 1.0" (lc-num-ref arg :real)) :real))
	       arg)
  nil)

(defmacro <sqrt> (result arg)
  (format *c-file* "  ~A~80,1T/* (sqrt ~A) */~%"
	  (lc-set +setf+ result
		  (format nil "sqrt((double)(~A))" (lc-num-ref arg)))
	  (clean-arg arg))
  nil)

(defmacro <ash> (result arg1 arg2)
  ;; in C, we need a different operator for left and right shifts, so this has to be dependent on the sign of arg2!
  (let ((l1 (lc-num-ref arg1 :integer))
	(l2 (lc-num-ref arg2 :integer)))
    (if (constantp arg2)
	(if (= arg2 0)
	    (format *c-file* "  ~A~80,1T/* (ash ~A 0) */~%"
		    (lc-set +setf+ result arg1 :integer)
		    (clean-arg arg1))
	  (format *c-file*
		  "  ~A~80,1T/* (ash ~A ~A) */~%"
		  (lc-set +setf+ result (format nil "(~A ~A ~A)"
						      l1
						      (if (> arg2 0) "<<" ">>")
						      (if (> arg2 0) arg2 (abs arg2))) :integer)
		  (clean-arg arg1) (clean-arg arg2)))
      (format *c-file* "  if (~A >= 0) ~A else ~A~80,1T/* (ash ~A ~A) */~%"
	      l2
	      (lc-set +setf+ result (format nil "~A << ~A" l1 l2) :integer)
	      (lc-set +setf+ result (format nil "~A >> (-~A)" l1 l2) :integer)
	      (clean-arg arg1) (clean-arg arg2))))
  nil)

(defmacro <mod> (result arg1 arg2)
  (checked-opall result
		 #'(lambda (res evars)
		     (let ((lcarg1 (lc-num-ref (first evars) :integer))
			   (lcarg2 (lc-num-ref (second evars) :integer))
			   (resname (lc-num-ref res :integer)))
		       (format nil "~A~%  if (((~A > 0) && (~A < 0)) || ((~A < 0) && (~A > 0)))~%  {~A}~%"
			       (lc-set +setf+ res (format nil "~A % ~A" lcarg1 lcarg2) :integer)
			       resname lcarg2 resname lcarg2 (lc-set +incf+ res lcarg2 :integer))))
		 #'(lambda (res evars)
		     (let ((lcarg1 (lc-num-ref (first evars)))
			   (lcarg2 (lc-num-ref (second evars)))
			   (resname (lc-num-ref res :real)))
		       (format nil "~A~%  if (((~A > 0.0) && (~A < 0.0)) || ((~A < 0.0) && (~A > 0.0)))~%  {~A}~%~%"
			       (lc-set +setf+ res (format nil "fmod((double)~A,(double)~A)" lcarg1 lcarg2))
			       resname lcarg2 resname lcarg2 (lc-set +incf+ res lcarg2))))
		 (list arg1 arg2))
  nil)

(defmacro <rem> (result arg1 arg2)
  (format *c-file* "  ~A~80,1T/* (rem ~A ~A) */~%"
	  (lc-set +setf+ result
		   (format nil "~A~A((double)~A,(double)~A)"
			   (if (member (varinfo-type (gethash (second result) vars)) '(:integer :clm-integer :bignum))
			       "(mus_long_t)" "")
			   "remainder" ; drem is obsolete
			   (lc-num-ref arg1)
			   (lc-num-ref arg2)))
	  (clean-arg arg1) (clean-arg arg2))
  nil)

(defun lc-str-ref (arg)
  (if (stringp arg)
      (format nil "~S" arg)
    (lc2 arg)))

(defun lc-ref (arg typ)
  (if (eq typ :string)
      (if (stringp arg)
	  (format nil "~S" arg)
	(format nil "~A" (lc2 arg)))
    (lc-num-ref arg typ)))

(defun lisp-to-C-format (nl fstr &rest args)
  (let* ((argl (mapcar #'lc-type args))
	 (ns nil)
	 (fs -1)
	 (flen (length fstr))
	 (argtypes nil))
    (if (null ok-numbers)
	(setf ok-numbers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (loop while (< fs (1- flen)) do
      (let ((nc (elt fstr (incf fs))))
	(if (not (char= nc #\~)) (push nc ns)
	  (progn			;now for fUn WiTh fOrMaT
	    (setf nc (elt fstr (incf fs)))
	    (if (member nc '(#\% #\&)) (progn (push #\\ ns) (push #\n ns))
	      (progn
		(push #\% ns)
		(loop while (or (member nc ok-numbers) (char= nc #\,)) do (setf nc (elt fstr (incf fs))))
		(if (or (char= nc #\A) (char= nc #\a))
		    (let ((arg (pop argl)))
		      (push :ok argtypes)
		      (if (member arg '(:clm-boolean :boolean))
			  (push #\d ns)

			(if (member arg '(:bignum :integer :clm-integer))
			    (progn
			      ;; (push #\l ns) (push #\l ns) (push #\d ns) -- mus_long_t is now int64_t, not long long int, 31-Aug-17
			      (push #\" ns) (push #\Space ns)
			      (push #\P ns) (push #\R ns) (push #\I ns) (push #\d ns) (push #\6 ns) (push #\4 ns)
			      (push #\" ns) (push #\Space ns))

			  (if (eq arg :string)
			      (push #\s ns)
			    (if (member arg '(:float-array :double-array :integer-array))
				(push #\x ns)
			      (push #\f ns))))))
		  (if (member nc '(#\D #\d))
		      (progn
			;; (push #\l ns) (push #\l ns) (push #\d ns) -- see above
			(push #\" ns) (push #\Space ns)
			(push #\P ns) (push #\R ns) (push #\I ns) (push #\d ns) (push #\6 ns) (push #\4 ns)
			(push #\" ns) (push #\Space ns)
			(pop argl) (push :integer argtypes))

		    (if (member nc '(#\F #\f #\$)) (progn (push #\f ns) (pop argl) (push :real argtypes))
		      (if (member nc '(#\G #\g)) (progn (push #\g ns) (pop argl) (push :real argtypes))
			(if (member nc '(#\E #\e)) (progn (push #\e ns) (pop argl) (push :real argtypes))
			  (if (member nc '(#\O #\o)) (progn (push #\o ns) (pop argl) (push :integer argtypes))
			    (if (member nc '(#\X #\x)) (progn (push #\x ns) (pop argl) (push :integer argtypes))
			      (if (member nc '(#\S #\s)) (progn (push #\s ns) (pop argl) (push :string argtypes))
				(if (char= nc #\~) (push #\~ ns)
				  (warn "clm's dumb format translator can't handle ~~~C in ~A" nc fstr))))))))))))))))
    (let ((newstr (make-string (length ns))))
      (setf ns (nreverse ns))
      (setf argtypes (nreverse argtypes))
      (loop for i from 0 below (length ns) do
	(setf (elt newstr i) (pop ns)))
      (format nil "\"~A~A\"~A~{~^~%        ~A~A~^,~}"
	      newstr
	      (if nl "\\n" "")
	      (if args "," "")
	      (loop for arg in args and typ in argtypes
		collect (if (eq typ :integer) "(mus_long_t)"
			  (if (eq typ :real) "(double)"
			    (if (eq typ :string) "(char *)"
			      "")))
		collect
		 (if (eq typ :string)
		     (lc-str-ref arg)
		   (lc-num-ref arg)))))))

(defun lisp-to-C-print (nl name fstr1 &rest args)
  ;; translate lisp format to C printf, for the simplest cases
  ;; added file output case 30-Sep-96
  (let* ((stdout (or (not fstr1) (stringp fstr1)))
	 (cstr (apply #'lisp-to-C-format nl (if stdout fstr1 (first args)) (if stdout args (cdr args))))
	 (named-cstr (if (and (stringp name)
			      (> (length name) 0))
			 (concatenate 'string (string #\") name " " (subseq cstr 1))
		       cstr)))
    (if stdout
	(format nil "  mus_error(0, ~A);~%" named-cstr)
      (if (> (length cstr) 0)
	  (format nil "  {char *buf; buf = (char *)calloc(512, sizeof(char)); snprintf(buf, 512, ~A); write(~A, buf, strlen(buf)); free(buf);}~%"
		  named-cstr
		  (lc-num-ref fstr1 :integer))))))


(defmacro <warn> (&optional fstr &rest args)
  (format *c-file* "  ~A" (apply #'lisp-to-C-print nil "warning:" fstr args))
  nil)

(defmacro <error> (&optional fstr &rest args)
  (format *c-file* "  {~A" (apply #'lisp-to-C-print nil "error:" fstr args))
  (format *c-file* "   clm_int[~D] = -1;~%" +clm-interrupted+) ; normally SIGINT if interrupted else 0
  (format *c-file* "   goto RUN_ALL_DONE;}~%")
  nil)

(defmacro <print> (fstr &optional (nl t))
  (if (stringp fstr)
      (format *c-file* "~A" (apply #'lisp-to-C-print nl "" fstr nil))
    (if (and fstr (listp fstr) (eq (first fstr) :var))
	(let* ((lname (second fstr))
	       (lst (gethash lname vars)))
	  (if (member (varinfo-type lst) '(:integer :clm-integer))
	      (format *c-file* "  mus_error(0, \"%d\", (int)~A);~80,1T/* (print ~A) */~%"
		      (lc-num-ref fstr :integer) (clean-arg fstr))
	    (if (member (varinfo-type lst) '(:real :clm-real))
		(format *c-file* "  mus_error(0, \"%f\", ~A);~80,1T/* (print ~A) */~%"
			(lc-num-ref fstr :real) (clean-arg fstr))
	      (if (member (varinfo-type lst) '(:boolean :clm-boolean))
		  (format *c-file* "  mus_error(0, \"%s\", (~A == 0) ? \"nil\" : \"t\");~80,1T/* (print ~A) */~%"
			  (lc-num-ref fstr :integer) (clean-arg fstr))
		(if (varinfo-gen-type lst)
		    (format *c-file* "  mus_error(0, \"%s\", mus_describe(~A));~80,1T/* (print ~A) */~%"
			  (lc lname) (clean-arg fstr))
		  (format *c-file* "  mus_error(0, \"~A\");~%" fstr))))))))
  (if nl (format *c-file* "  mus_error(0, \"\\n\");~%"))
  nil)

(defmacro <princ> (fstr)
  `(<print> ,fstr nil))

(defmacro <clm-print> (&optional fstr &rest args)
  (format *c-file* "~80,1T/* (clm-print ~S ~A) */~%" (clean-arg fstr) (format nil "~{ ~A~}" (mapcar #'clean-arg args)))
  (format *c-file* "~A" (apply #'lisp-to-C-print nil "" fstr args))
  nil)

(defmacro <snd-memo> (result filename fstr &rest args)
  ;; filename is a string
  ;; need to open (create or append) .scm file of same name
  ;; get fd, write to it, and close
  (let ((fd (lc2 result)))
    (format *c-file* "  {char *memo_file; int addr;~1,80T/* (snd-memo ~A ~A ~A) */~%"
	    (clean-arg filename) (clean-arg fstr) (format nil "~{ ~A~}" (mapcar #'clean-arg args)))
    (format *c-file* "   memo_file = \"~A.scm\";~%" filename)
    (format *c-file* "   ~A = mus_file_open_write(memo_file);~%" fd)
    (format *c-file* "   if (~A != -1) {~%" fd)
    (format *c-file* "   ~A" (apply #'lisp-to-C-print nil "" result (append (list fstr) args)))
    (format *c-file* "     close(~A);}~%" fd)
    (format *c-file* "   else mus_error(0,\"can't open snd memo file\");~%")
    (if (not (stringp filename)) (format *c-file* "   FREE(memo_file);~%"))
    (format *c-file* "   }~%"))
  nil)

(defmacro <terpri> ()
  (format *c-file* "  mus_error(0, \"\\n\");~80,1T/* (terpri) */~%")
  nil)

(defmacro <label> (arg)
  (format *c-file* "~A:~%" (lc arg))
  nil)

(defun lc-bool (var)
  (if (not var)
      "false"
    (if (or (numberp var) (eq var t))
	"true"
      (let* ((l-name (second var))
	     (c-name (lc l-name))
	     (depth (third var))
	     (lst (gethash (cadr var) vars)))
	;; this is a value reference, so decide based on depth etc
	(if (member (varinfo-type lst) '(:real :integer :clm-real :clm-integer :bignum))
	    "true"
	  (if (member (varinfo-type lst) '(:boolean :clm-boolean :float-array :double-array :integer-array :mus-any :mus-any-array))
	      c-name
	    (if (varinfo-gen-type lst)
		(if (varinfo-arr-gen-type lst)
		    (format nil "~A_array" c-name)
		  c-name)
	      (if (zerop depth)
		  (format nil "((clm_int[~A] != ~D) || (clm_int[~A + 1]))"
			  c-name +no-type+ c-name)
		(format nil "((clm_int[~A + ~D] != ~D) || (clm_int[~A + ~D + 1]))"
			c-name (* 2 depth) +no-type+ c-name (* 2 depth))))))))))

(defun lc-bool-not (var)
  (if (not var)
      "true"
    (if (or (numberp var) (eq var t))
	"false"
      (let* ((l-name (second var))
	     (c-name (lc l-name))
	     (depth (third var))
	     (lst (gethash (cadr var) vars)))
	;; this is a value reference, so decide based on depth etc
	(if (member (varinfo-type lst) '(:real :integer :clm-real :clm-integer :bignum))
	    "false"
	  (if (member (varinfo-type lst) '(:boolean :clm-boolean :float-array :double-array :integer-array :mus-any :mus-any-array))
	      (format nil "(!~A)" c-name)
	    (if (varinfo-gen-type lst)
		(if (varinfo-arr-gen-type lst)
		    (format nil "(!~A_array)" c-name)
		  (format nil "(!~A)" c-name))
	      (if (zerop depth)
		  (format nil "((clm_int[~A] == ~D) && (clm_int[~A + 1] == 0))"
			  c-name +no-type+ c-name)
		(format nil "((clm_int[~A + ~D] == ~D) && (clm_int[~A + ~D + 1] == 0))"
			c-name (* 2 depth) +no-type+ c-name (* 2 depth))))))))))

(defmacro <jump> (arg)
  (format *c-file* "  goto ~A;~%" (lc arg))
  nil)

(defmacro <jump-if> (var arg)
  (format *c-file* "  if (~A) goto ~A;~%"
	  (lc-bool var)
	  (lc arg))
  nil)

(defmacro <jump-if-not> (var arg)
  (format *c-file* "  if (~A) goto ~A;~%"
	  (lc-bool-not var)
	  (lc arg))
  nil)

(defmacro <case> (index name-list label-list)
  (format *c-file* "  switch ((int)~A){~%" (lc-num-ref index))
  (loop for name in name-list and label in label-list do
	(format *c-file* "    ~A: goto ~A; break;~%"
		(if (not (member name '(t otherwise))) (format nil "case ~A" name) "default") (lc label)))
  (format *c-file* "    }~%")
  nil)

(defun lc-num-compare (res n cmp i &optional not-cmp argtype)
  ;; has to be integers here
  (let ((resname (lc2 res)))
    (if (integerp n)
	(format nil "~A = ~A(~D ~A ~A)" resname (if not-cmp "!" "") n cmp i)
      (if (or (not (listp n)) (not (eq (car n) :var)))
	  (progn
	  (warn "what's ~A? " n)
	  n)
	(let* ((l-name (second n))
	       (c-name (lc l-name))
	       (depth (third n))
	       (lst (gethash (cadr n) vars)))
	  (if (member (varinfo-type lst) (list :integer :bignum :booelan))
	      (if (and (varinfo-shadowed lst)
		       (> depth 0))
		  (format nil "~A = ~A(~A_~D ~A ~A)" resname (if not-cmp "!" "") c-name depth cmp i)
		(format nil "~A = ~A(~A ~A ~A)" resname (if not-cmp "!" "") c-name cmp i))
	    (if (eq (varinfo-type lst) :real)
		(if (and (varinfo-shadowed lst)
			 (> depth 0))
		    (format nil "~A = ~A(~A_~D ~A ~A)" resname (if not-cmp "!" "") c-name depth cmp (if (= i 0) "0.0" "2.0"))
		  (format nil "~A = ~A(~A ~A ~A)" resname (if not-cmp "!" "") c-name cmp (if (= i 0) "0.0" "2.0")))
	      (if (member (varinfo-type lst) '(:clm-integer :clm-boolean))
		  (format nil "~A = ~A(~A ~A ~A)" resname (if not-cmp "!" "") c-name cmp i)
		(if (eq (varinfo-type lst) :clm-real)
		    (format nil "~A = ~A(~A ~A ~A)" resname (if not-cmp "!" "") c-name cmp (if (= i 0) "0.0" "2.0"))
		  (if (eq argtype :integer)
		      (format nil "~A = ~A(clm_int[~A + ~D + 1] ~A ~A)" resname (if not-cmp "!" "") c-name (* 2 depth) cmp i)
		    ;; comparison arg depends on type
		    (progn
		      (if *clm-debug* (format t "untyped: ~A~%" l-name))
		    (format nil "  if (clm_int[~A + ~D]==~D) ~A = ~A(clm_int[~A + ~D + 1] ~A ~A); else ~A = ~A(clm_double[~A_r+~D] ~A ~A);~%"
			    c-name (* 2 depth) +integer+
			    resname (if not-cmp "!" "") c-name (* 2 depth) cmp i
			    resname (if not-cmp "!" "") c-name depth cmp (if (= i 0) "0.0" "2.0"))))))))))))
  )

(defmacro <zerop> (result arg)
  (format *c-file* "  ~A;~80,1T/* (zerop ~A) */~%" (lc-num-compare result arg "==" 0) (clean-arg arg))
  nil)

(defmacro <minusp> (result arg)
  (format *c-file* "  ~A;~80,1T/* (minusp ~A) */~%" (lc-num-compare result arg "<" 0) (clean-arg arg))
  nil)

(defmacro <plusp> (result arg)
  (format *c-file* "  ~A;~80,1T/* (plusp ~A) */~%" (lc-num-compare result arg ">" 0) (clean-arg arg))
  nil)

(defmacro <oddp> (result arg)
  (format *c-file* "  ~A;~80,1T/* (oddp ~A) */~%" (lc-num-compare result arg "%" 2 nil :integer) (clean-arg arg))
  nil)

(defmacro <evenp> (result arg)
  (format *c-file* "  ~A;~80,1T/* (evenp ~A) */~%" (lc-num-compare result arg "%" 2 t :integer) (clean-arg arg))
  nil)

(defmacro <null> (result arg)
  (format *c-file* "  ~A~80,1T/* (null ~A) */~%"
	  (lc-set +setf+ result (lc-bool-not arg) :pboolean)
	  (clean-arg arg))
  nil)

(defmacro <not> (result arg)
  (format *c-file* "  ~A~80,1T/* (null ~A) */~%"
	  (lc-set +setf+ result (lc-bool-not arg) :pboolean)
	  (clean-arg arg))
  nil)

(defmacro <neq> (result &rest args)
  ;; harder than the others because all must be pairwise unequal
  (if (= (length args) 1)
      (format *c-file* "  ~A~80,1T/* (neq ~A) */~%" (lc-set +setf+ result "true" :pboolean) (clean-arg (first args)))
    (if (= (length args) 2)
	(format *c-file* "  ~A~80,1T/* (neq ~A ~A) */~%"
		(lc-set +setf+ result
			(format nil "(~A != ~A)"
				(lc-num-ref (first args))
				(lc-num-ref (second args)))
			:pboolean)
		(clean-arg (first args))
		(clean-arg (second args)))
      (format *c-file* "  ~A~80,1T/* (neq~{ ~A~}) */~%"
	      (lc-set +setf+ result
		      (format nil "(~{(~A != ~A)~^ && ~})"
			      (loop for arg on args append
				(loop for arg1 in (cdr arg)
				  collect (lc-num-ref (first arg)) collect (lc-num-ref arg1))))
		      :pboolean)
	      (mapcar #'clean-arg args))))
  nil)

(defmacro <compare> (compare result &rest args)
  (if (= (length args) 1)
      (format *c-file* "  ~A~80,1T/* (~A ~A) */~%"
	      (lc-set +setf+ result "true" :pboolean) compare (clean-arg (first args)))
    (if (= (length args) 2)
	(format *c-file* "  ~A~80,1T/* (~A ~A ~A) */~%"
		(lc-set +setf+ result
			(format nil "(~A ~A ~A)"
				(lc-num-ref (first args))
				compare
				(lc-num-ref (second args)))
			:pboolean)
		compare
		(clean-arg (first args))
		(clean-arg (second args)))
      (format *c-file* "  ~A~80,1T/* (~A~{ ~A~}) */~%"
	      (lc-set +setf+ result
		      (format nil "(~{(~A ~A ~A)~^ && ~})"
			      (loop for arg in args and arg1 in (cdr args)
				collect (lc-num-ref arg) collect compare collect (lc-num-ref arg1)))
		      :pboolean)
	      compare (mapcar #'clean-arg args))))
  nil)



;;; ---- AREF ----


(defun array-header-size (arr)
  (+ 5 (length (array-dimensions arr))))

(defmacro <array?> (result arg)
  (lc-check-type result arg (list +real-array+ +integer-array+ +array+) "array?"
		 #'(lambda (arg) (declare (ignore arg)) "false")
		 #'(lambda (typ) (declare (ignore typ)) "false"))
  nil)

(defun load-real-array (arr i r datai datar &optional size)
  ;; place header table at i, data in datar at r
  ;; return new i r
  ;; this is assumed to be a 1-dim array
  (let ((len (if arr (length arr) size)))
    (setf (aref datai (+ i +aref-type+)) +real-array+)
    (setf (aref datai (+ i +aref-rblock+)) r)
    (setf (aref datai (+ i +aref-size+)) len)
    (setf (aref datai (+ i +aref-dims+)) 1)
    (setf (aref datai (+ i +aref-dims+ 1)) len)
    (if arr
	(loop for k from 0 below len do
	  (setf (aref datar (+ r k)) (double (aref arr k)))))
    (list (+ i 6) (+ r len))))

(defun unload-real-array (i datai datar)
  ;; assumed to be header at i, data at r (redundant)
  (let ((len (aref datai (+ i +aref-size+)))
	(rb (aref datai (+ i +aref-rblock+))))
    (let ((new-arr (make-double-array len)))
      (loop for i from 0 below len do
	(setf (aref new-arr i) (aref datar (+ rb i))))
      new-arr)))

(defun load-integer-array (arr i r datai datar &optional size)
  (declare (ignore datar))
  ;; place header at i, data in datai after header
  ;; return new i
  ;; this is assumed to be a 1-dim array
  (let ((len (if arr (length arr) size)))
    (setf (aref datai (+ i +aref-type+)) +integer-array+)
    (setf (aref datai (+ i +aref-iblock+)) (+ i 6))
    (setf (aref datai (+ i +aref-size+)) len)
    (setf (aref datai (+ i +aref-dims+)) 1)
    (setf (aref datai (+ i +aref-dims+ 1)) len)
    (if arr
	(loop for k from 0 below len do
	  (setf (aref datai (+ i 6 k)) (aref arr k))))
    (list (+ i 6 len) r)))

(defun unload-integer-array (i datai)
  ;; assumed to be header at i
  (let ((len (aref datai (+ i +aref-size+)))
	(ib (aref datai (+ i +aref-iblock+))))
    (let ((new-arr (make-integer-array len)))
      (loop for i from 0 below len do
	(setf (aref new-arr i) (aref datai (+ ib i))))
      new-arr)))

(defun load-scaled-array (arr i r datai datar scaler)
  ;; place header tableat i, data in datar at r
  ;; return new i r
  ;; this is assumed to be a 1-dim array
  (let ((len (length arr)))
    (setf (aref datai (+ i +aref-type+)) +real-array+)
    (setf (aref datai (+ i +aref-rblock+)) r)
    (setf (aref datai (+ i +aref-size+)) len)
    (setf (aref datai (+ i +aref-dims+)) 1)
    (setf (aref datai (+ i +aref-dims+ 1)) len)
    (loop for k from 0 below len do
      (setf (aref datar (+ r k)) (double (* scaler (aref arr k)))))
    (list (+ i 6) (+ r len))))

(defun unload-scaled-array (i datai datar scaler)
  ;; assumed to be header at i
  (let ((len (aref datai (+ i +aref-size+)))
	(rb (aref datai (+ i +aref-rblock+))))
    (let ((new-arr (make-double-array len)))
      (loop for i from 0 below len do
	(setf (aref new-arr i) (* scaler (aref datar (+ rb i)))))
      new-arr)))

(defmethod gen-load ((gen t) i r datai datar)
  (declare (ignore r datar))
  (if (null gen)
      (progn
        (setf (aref datai i) +no-type+)
	(setf (aref datai (+ i 1)) 0))
    (progn ;default fallback is to send 't' -- this is not going to work in general
      (setf (aref datai i) +no-type+)
      (setf (aref datai (+ i 1)) 1))))

(defmethod gen-load ((gen integer) i r datai datar)
  (declare (ignore r datar))
  (setf (aref datai i) +integer+)
  #-cmu (setf (aref datai (+ i 1)) gen)
  #+cmu (setf (aref datai (+ i 1)) (ldb (byte 32 0) gen))
  )

(defmethod gen-load ((gen number) i r datai datar)
  (setf (aref datai i) +real+)
  (setf (aref datar r) (double gen)))

(defmethod gen-unload ((gen t) i r datai datar)
  (if (or (null gen) (eq gen t))
      (if (zerop (aref datai (+ i 1))) nil t)
    (if (integerp gen)
	(aref datai (+ i 1))
      (if (numberp gen)
	  (aref datar r)
	(warn "can't unload ~A (~A, ~D) " gen (type-of gen) (aref datai i))))))

(defmethod gen-size ((gen t))
  ;; this is in addition to assumed var space
  nil)

(defun load-array (arr i r datai datar)
  ;; array elements are pointers to data blocks (each element is one int = addr of block)
  (let* ((len (array-total-size arr))
	 (dims (array-dimensions arr))
	 (hdr (array-header-size arr))
	 (diloc (+ i (* len 2) hdr))
	 (drloc (+ r len)))
    (if (= len 0) (warn "trying to load 0-length array? (~A)" arr))
    (setf (aref datai (+ i +aref-type+)) +array+)
    (setf (aref datai (+ i +aref-iblock+)) (+ i hdr))
    (setf (aref datai (+ i +aref-rblock+)) r)
    (setf (aref datai (+ i +aref-size+)) len)
    (setf (aref datai (+ i +aref-dims+)) (length dims))
    ;; (format t "array is start at ~D ~D~%" diloc drloc)
    (loop for k from 1 to (length dims) do
      (setf (aref datai (+ i +aref-dims+ k)) (nth (1- k) dims)))
    (loop for k from 0 below len and ji from (+ i hdr) by 2 and jr from r by 1 do
      (let* ((val (row-major-aref arr k))
	     (sizes (gen-size val)))
	(if sizes
	    (progn
	      (gen-load val diloc drloc datai datar)
	      (setf (aref datai ji) (aref datai diloc))
	      (setf (aref datai (1+ ji)) diloc)
	      (incf diloc (first sizes))
	      (incf drloc (second sizes)))
	  (gen-load val ji jr datai datar))))
    (list diloc drloc)))

(defun unload-array (i r datai datar)
  (declare (ignore r))
  ;; assumed to be header at i
  (let* ((len (aref datai (+ i +aref-size+)))
	 (ib (aref datai (+ i +aref-iblock+)))
	 (rb (aref datai (+ i +aref-rblock+)))
	 (dims (aref datai (+ i +aref-dims+)))
	 (dimlist (loop for k from 1 to dims collect (aref datai (+ i k +aref-dims+)))))
    (let ((new-arr (make-array dimlist)))
      (loop for i from 0 below len and iadr from ib by 2 and radr from rb by 1 do
	(setf (row-major-aref new-arr i)
	      (gen-unload (run-type->class (aref datai iadr)) iadr radr datai datar)))
      new-arr)))

(defun clm-array-element-type (n)
  (or (/= (array-rank n) 1)
      #-clisp (array-element-type n)
      #+clisp
      (if (every #'integerp n)
	  'integer
	(if (every #'numberp n)
	    'double-float
	  t))
      ))

(defmethod gen-load ((gen array) i r datai datar)
  (let ((eltype (clm-array-element-type gen)))
    (if (eq eltype 'double-float)
	(load-real-array gen i r datai datar)
      (if (or (member eltype '(integer bit fixnum)) (equal eltype '(signed-byte 32)))
	  (load-integer-array gen i r datai datar)
	(load-array gen i r datai datar)))))

(defmethod gen-size ((gen array))
  (let ((eltype (clm-array-element-type gen))
	(len (array-total-size gen)))
    (if (eq eltype 'double-float)
	(list (array-header-size gen) len)
      (if (or (member eltype '(integer fixnum bit)) (equal eltype '(signed-byte 32)))
	  (list (+ (array-header-size gen) len) 0)
	(let ((isize (+ (array-header-size gen) (* 2 len)))
	      (rsize len))
	  (loop for i from 0 below len do
	    (let ((el-sizes (gen-size (row-major-aref gen i))))
	      (when el-sizes
		(incf isize (first el-sizes))
		(incf rsize (second el-sizes)))))
	  (list isize rsize))))))

(defmethod gen-unload ((gen array) iadr r datai datar)
  (let ((eltype (clm-array-element-type gen))
	(i (aref datai (+ iadr 1))))
    (if (eq eltype 'double-float)
	(unload-real-array i datai datar)
      (if (or (member eltype '(integer fixnum)) (equal eltype '(signed-byte 32)))
	  (unload-integer-array i datai)
	(unload-array i r datai datar)))))

(defun array-header (tab)
  (let* ((depth (third tab))
	 (cvar (lc2 tab))
	 (info (gethash (second tab) vars)))
    (if (and info
	     (varinfo-iloc info)
	     (member (varinfo-type info) '(:double-array :integer-array)))
	(format nil "CLM_VAR_ADDR(~D)" (varinfo-iloc info))
      (format nil "clm_int[~A + ~D + 1]" cvar (* 2 depth)))))

(defun array-iblock (tab)
  (format nil "CLM_ARR_IBLOCK(~A)" (array-header tab)))

(defun array-rblock (tab)
  (format nil "CLM_ARR_RBLOCK(~A)" (array-header tab)))

(defun array-size (tab)
  (format nil "CLM_ARR_SIZE(~A)" (array-header tab)))

(defun array-type (tab)
  (format nil "CLM_ARR_TYPE(~A)" (array-header tab)))

(defun array-dims (tab)
  (format nil "CLM_ARR_DIMS(~A)" (array-header tab)))

(defun array-dim (tab dim)
  (format nil "CLM_ARR_DIM(~A, ~A)" (array-header tab) (lc-num-ref dim :integer)))

(defun real-array-ref (tab ind)
  (format nil "clm_double[~A + ~A]" (array-rblock tab) (if (stringp ind) ind (lc-num-ref ind :integer))))

(defun integer-array-ref (tab ind)
  (format nil "clm_int[~A + ~A]" (array-iblock tab) (if (stringp ind) ind (lc-num-ref ind :integer))))

(defun gen-array-ref (result tab ind)
  (format nil "~A = ~A_array[(int)(~A)];~%" (lc2 result) (lc2 tab) (if (stringp ind) ind (lc-num-ref ind :integer))))

(defun gen-array-make (key var)
  (format *c-file* "  if (CLM_ARRAY_P(~A))~%    {~%" (lc key))
  (format *c-file* "      ~A_array = (mus_any **)calloc(~A, sizeof(mus_any *));~%"
	  (lc key)
	  (format nil "CLM_ARR_SIZE(CLM_VAR_ADDR(~A))" (lc key)))
  (format *c-file* "      { /* load ~A array */~%       ~
                            int i;~%       ~
                            for (i = 0; i < CLM_ARR_SIZE(CLM_VAR_ADDR(~A)); i++)~%         ~
                              if (clm_int[CLM_ARR_IBLOCK(CLM_VAR_ADDR(~A)) + (i * 2) + 1] != 0)~%            ~
                                {~%"
	  (lc key)
	  (lc key)
	  (lc key))
  (gen-make-1 (varinfo-arr-gen-type var)
	      *c-file*
	      (format nil "~A_array[i]" (lc key))
	      (format nil "CLM_ARR_IBLOCK(CLM_VAR_ADDR(~A)) + (i * 2)" (lc key))
	      "              "
	      nil)
  (format *c-file* "    }}}~%"))

(defmacro <array-rank> (result arr)
  (format *c-file* "  ~A/* (array-rank ~A) */~%"
	  (lc-set +setf+ result (array-dims arr))
	  (clean-arg arr)))

(defmacro <array-total-size> (result arr)
  (format *c-file* "  ~A/* (array-total-size ~A) */~%"
	  (lc-set +setf+ result (array-size arr))
	  (clean-arg arr)))

(defmacro <length> (result arr)
  (if (and arr (listp arr) (eq (first arr) :var))
      (let ((info (gethash (second arr) vars)))
	(if (and info
		 (member (varinfo-type info) '(:double-array :integer-array)))
	    (format *c-file* "  ~A = ~A;~80,1T/* (length ~A) */~%"
		    (lc2 result)
		    (array-size arr)
		    (clean-arg arr))
	  (format *c-file* "  ~A = ((clm_int[~A + ~D] == ~D) ? clm_int[clm_int[~A + ~D + 1] + 1] : ~A);~80,1T/* (length ~A) */~%"
		  (lc2 result) (lc2 arr) (third arr) +string+ (lc2 arr) (third arr)
		  (array-size arr)
		  (clean-arg arr))))
    (if (stringp arr)
	(format *c-file* " ~A = ~D;~%" (lc2 result) (length arr))))
  nil)

(defmacro <array-dimension> (result arr axis)
  (format *c-file* "  ~A/* (array-dimension ~A ~A) */~%"
	  (lc-set +setf+ result (format nil "clm_int[~A + CLM_AREF_DIMS + 1 + ~A]" (array-header arr) (lc-num-ref axis :integer)))
	  (clean-arg arr) (clean-arg axis)))

(defun index->cell (arr indices)
  (let* ((indlen (length indices)))

    (when (> *safety* 0)
      (format *c-file* "  if ((~A) != ~D)~%    {~%      mus_error(MUS_ARG_OUT_OF_RANGE,~%               \"wrong number of subscripts: %d for array %s of rank: %d\",~%               ~D, ~S, ~A);~%"
	      (array-dims arr)
	      indlen
	      indlen
	      (lc2 arr)
	      (array-dims arr))
      (format *c-file* "      clm_int[~D] = -1;~%" +clm-interrupted+) ; normally SIGINT if interrupted else 0
      (format *c-file* "      goto RUN_ALL_DONE;~%    }~%"))

    (if (= indlen 1)
	(lc-num-ref (first indices) :integer)
      (if (= indlen 2)
	  (format nil "(~A * ~A) + ~A" (lc-num-ref (first indices) :integer) (array-dim arr 1) (lc-num-ref (second indices) :integer))
	(if (= indlen 3)
	    (format nil "(~A * ~A * ~A) + (~A * ~A) + ~A"
		    (lc-num-ref (first indices) :integer) (array-dim arr 1) (array-dim arr 2)
		    (lc-num-ref (second indices) :integer) (array-dim arr 1)
		    (lc-num-ref (third indices) :integer))
	  (format nil "(~{~A~^+~})"
		  (loop for index in indices and i from 0
		    collect (format nil "(~A~^~{* ~A~})"
				    (lc-num-ref index :integer)
				    (loop for j from (1+ i) below indlen
				      collect (array-dim arr j))))))))))

(defmacro <gen-aref> (result arr &rest indices)
  (format *c-file* "  ~A = ~A_array[(int)(~A)];~%" (lc2 result) (lc2 arr) (index->cell arr indices))
  nil)

(defmacro <float-aref> (result arr index)
  (format *c-file* "  ~A = ~A[(int)(~A)];~%" (lc2 result) (lc2 arr) (lc-num-ref index :integer))
  nil)

(defmacro <setf-float-aref> (type arr index val)
  (format *c-file* "  ~A[(int)(~A)] ~A ~A;~%"
	  (lc2 arr) (lc-num-ref index :integer)
	  (setf-type->c-op type)
	  (lc-num-ref val :real))
  nil)

(defmacro <double-aref> (result arr &rest indices)
  (format *c-file* "  ~A = ~A[(int)(~A)];~%" (lc2 result) (lc2 arr) (index->cell arr indices))
  nil)

(defmacro <integer-aref> (result arr &rest indices)
  (format *c-file* "  ~A = ~A[(int)(~A)];~%" (lc2 result) (lc2 arr) (index->cell arr indices))
  nil)

(defmacro <setf-double-aref> (type &rest args) ;last arg is new val
  (let* ((arr (first args))
	 (val (first (last args)))
	 (indices (butlast (cdr args))))
    (format *c-file* "  ~A[(int)(~A)] ~A ~A;~%"
	    (lc2 arr) (index->cell arr indices)
	    (setf-type->c-op type)
	    (lc-num-ref val :real))
    nil))

(defmacro <setf-integer-aref> (type &rest args) ;last arg is new val
  (let* ((arr (first args))
	 (val (first (last args)))
	 (indices (butlast (cdr args))))
    (format *c-file* "  ~A[(int)(~A)] ~A ~A;~%"
	    (lc2 arr) (index->cell arr indices)
	    (setf-type->c-op type)
	    (lc-num-ref val :integer))
    nil))

(defmacro <aref> (result arr &rest indices)
  (if (null indices) (warn "(aref ~A~{ ~A~}))?" arr indices))
  (let* ((lresname (second result))
	 (resdepth (* 2 (third result)))
	 (reslst (gethash lresname vars))
	 (resname (lc lresname))
	 (lci (index->cell arr indices))
	 (arrinfo (gethash (second arr) vars)))
    (if (or (varinfo-gen-type reslst)
	    (and arrinfo
		 (eq (varinfo-gen-type arrinfo) :mus-any-array)))
	(format *c-file* "  ~A = ~A_array[(int)(~A)];~%" (lc2 result) (lc2 arr) (index->cell arr indices))
      (progn
	(format *c-file* "  {int lci; lci = ~A;~%" lci)
	(when (> *safety* 0)
	  (format *c-file* "  if (lci < 0) {mus_error(0, \"~A[%d]?\", lci); goto RUN_ALL_DONE;}~%   ~
                          else if (lci >= ~A) {mus_error(0, \"invalid index: ~A[%d > %d]\", lci, ~A); goto RUN_ALL_DONE;}~%"
		  (clean-arg arr)
		  (array-size arr) (clean-arg arr) (array-size arr)))
	(format *c-file* "  switch (~A)~%    {~%" (array-type arr))
	(format *c-file* "    case ~A: ~A break;~%" +real-array+ (lc-set +setf+ result (real-array-ref arr "lci")))
	(format *c-file* "    case ~A: ~A break;~%" +integer-array+ (lc-set +setf+ result (integer-array-ref arr "lci") :integer))
	(format *c-file* "    default: {~%      int iblock, rblock;~%")
	(format *c-file* "      iblock = ~A + (lci * 2); rblock = ~A + lci;~%"
		(array-iblock arr) (array-rblock arr))
	;; can't use lc-set for the general case because there's no *_r form to access the doubles
	(if (member (varinfo-type reslst) '(:clm-integer :clm-boolean :clm-real))
	    (format *c-file* "      if (clm_int[iblock] == ~A) ~A = (clm_double[rblock]); else ~A = (clm_int[iblock + 1]);~%"
		    +real+ resname resname)
	  (if (member (varinfo-type reslst) '(:integer :real :boolean))
	      (if (and (varinfo-shadowed reslst)
		       (> resdepth 0))
		  (format *c-file* "      if (clm_int[iblock] == ~A) ~A_~D = (clm_double[rblock]); else ~A_~D = (clm_int[iblock + 1]);~%"
			  +real+ resname resdepth resname resdepth)
		(format *c-file* "      if (clm_int[iblock] == ~A) ~A = (clm_double[rblock]); else ~A = (clm_int[iblock + 1]);~%"
			+real+ resname resname))
	    (progn
	      (if *clm-debug* (format t "untyped array: ~A~%" (second arr)))
	      ;; general case, copy entire val into var
	      ;; float here? (i.e. first copy int to real in val?
	      (format *c-file* "      clm_int[~A + ~D] = clm_int[iblock];~%" resname resdepth)
	      (format *c-file* "      clm_int[~A + ~D + 1] = clm_int[iblock+1];~%" resname resdepth)
	      (format *c-file* "      clm_double[~A_r + ~D] = clm_double[rblock];~%" resname resdepth))))
	(format *c-file* "  }}}~%~%"))))
  nil)

(defmacro <setf-aref> (type &rest args) ;last arg is new val
  (if (< (length args) 3) (warn "(~A (aref~{ ~A~}))?" (setf-type->lisp-op type) (butlast args)))
  (let* ((arr (first args))
	 (val (first (last args)))
	 (indices (butlast (cdr args)))
	 (op (setf-type->c-op type))
	 (lci (index->cell arr indices)))
    (if (null lci) (warn "no array indices: (~A (aref ~{ ~A~}))?" (setf-type->lisp-op type) (butlast args)))
    (format *c-file* "  {int lci; lci = ~A;~%" lci)
    (when (> *safety* 0)
      (format *c-file* "  if (lci < 0) mus_error(0, \"~A[%d]?\", lci);~%   ~
                          else if (lci >= ~A) mus_error(0, \"invalid index: ~A[%d > %d]\", lci, ~A);~%"
	      (clean-arg arr)
	      (array-size arr) (clean-arg arr) (array-size arr)))
    (format *c-file* "  switch (~A)~%    {~%" (array-type arr))
    (format *c-file* "    case ~A: ~A ~A ~A; break;~%" +real-array+ (real-array-ref arr "lci") op (lc-num-ref val))
    (format *c-file* "    case ~A: ~A ~A ~A; break;~%" +integer-array+ (integer-array-ref arr "lci") op (lc-num-ref val))
    (format *c-file* "    default: {~%      int iblock,rblock;~%")
    (format *c-file* "      iblock = ~A + (lci*2); rblock = ~A + lci;~%"
	    (array-iblock arr) (array-rblock arr))
    ;; can't use lc-set for the general case because there's no *_r form to access the doubles
    (if (constantp val)
	(if (eq val t)
	    (format *c-file* "    clm_int[iblock] = ~D; clm_int[iblock + 1] = 1;~%" +no-type+)
	  (if (eq val nil)
	      (format *c-file* "    clm_int[iblock] = ~D; clm_int[iblock + 1] = 0;~%" +no-type+)
	    (if (integerp val)
		(format *c-file* "    clm_int[iblock] = ~D; clm_int[iblock + 1] ~A ~D;~%" +integer+ op val)
	      (format *c-file* "    clm_int[iblock] = ~D; clm_double[rblock] ~A ~A;~%" +real+ op (lc-num-ref val)))))
      (let* ((lresname (second val))
	     (resdepth (third val))
	     (reslst (gethash lresname vars))
	     (resname (lc lresname)))
	(if (member (varinfo-type reslst) '(:clm-integer :clm-boolean :integer :boolean))
	    (format *c-file* "      clm_int[iblock] = ~D; clm_int[iblock+1] ~A ~A;~%" +integer+ op (lc-num-ref val))
	  (if (member (varinfo-type reslst) '(:clm-real :real))
	      (format *c-file* "      clm_int[iblock] = ~D; clm_double[rblock] ~A ~A;~%" +real+ op (lc-num-ref val))
	    (progn
	      (if *clm-debug* (format t "set untyped array: ~A~%" (second arr)))
	      (format *c-file* "      clm_int[iblock] = clm_int[~A + ~D];~%" resname (* 2 resdepth))
	      (format *c-file* "      clm_int[iblock + 1] ~A clm_int[~A + ~D + 1];~%" op resname (* 2 resdepth))
	      (format *c-file* "      clm_double[rblock] ~A clm_double[~A_r+~D];~%" op resname resdepth))))))
    (format *c-file* "  }}}~%~%"))
  nil)

(defmacro <array-in-bounds-p> (result arr &rest indices)
  (format *c-file* "  ~A = (~{(~A >= 0) && (~A < ~A)~^ && ~});~%"
	  (lc2 result)
	  (loop for index in indices and i from 0
	    collect (lc-num-ref index)
	    collect (lc-num-ref index)
	    collect (array-dim arr i)))
  nil)



(defun lc-arr-ref (n)
  (let ((info (gethash (second n) vars)))
    (if (and info
	     (member (varinfo-type info) '(:float-array :double-array :integer-array)))
	(lc2 n)
      (format nil "(double *)(clm_double + ~A)" (array-rblock n)))))


;;; ---- CLEAR-ARRAY ----

(defmacro <clear-array> (data &optional len)
  (format *c-file* "  memset((void *)(~A), 0, (~A) * sizeof(mus_float_t));~%"
	  (lc-arr-ref data) (if len (lc-num-ref len :integer) (array-size data)))
  nil)

#|
;;; ---- MULTIPLY-ARRAYS ----

(defmacro <multiply-arrays> (data window &optional len)
  (format *c-file* "  mus_multiply_arrays(~A, ~A, ~A);~%"
	  (lc-arr-ref data) (lc-arr-ref window) (if len (lc-num-ref len :integer) (array-size window)))
  nil)
|#

;;; ---- RECTANGULAR->POLAR ----

(defmacro <rectangular2polar> (rdata idata &optional len)
  (format *c-file* "  mus_rectangular_to_polar(~A, ~A, ~A);~%"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (if len (lc-num-ref len :integer) (array-size rdata)))
  nil)

;;; ---- RECTANGULAR->MAGNITUDES ----

(defmacro <rectangular2magnitudes> (rdata idata &optional len)
  (format *c-file* "  mus_rectangular_to_magnitudes(~A, ~A, ~A);~%"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (if len (lc-num-ref len :integer) (array-size rdata)))
  nil)

;;; ---- POLAR->RECTANGULAR ----

(defmacro <polar2rectangular> (rdata idata &optional len)
  (format *c-file* "  mus_polar_to_rectangular(~A, ~A, ~A);~%"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (if len (lc-num-ref len :integer) (array-size rdata)))
  nil)


;;; ---- POLYNOMIAL ----

(defmacro <polynomial> (result tab x &optional len)
  (format *c-file* "  ~A = mus_polynomial(~A, ~A, ~A);~%"
	  (lc2 result) (lc-arr-ref tab) (lc-num-ref x) (if len (lc-num-ref len :integer) (array-size tab)))
  nil)


;;; ---- CHEBYSHEV_U_SUM ----

(defmacro <chebyshev-u-sum> (result x un)
  (format *c-file* "  ~A = mus_chebyshev_u_sum(~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref x) (array-size un) (lc-arr-ref un))
  nil)


;;; ---- CHEBYSHEV_T_SUM ----

(defmacro <chebyshev-t-sum> (result x tn)
  (format *c-file* "  ~A = mus_chebyshev_t_sum(~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref x) (array-size tn) (lc-arr-ref tn))
  nil)


;;; ---- CHEBYSHEV_TU_SUM ----

(defmacro <chebyshev-tu-sum> (result x tn un)
  (format *c-file* "  ~A = mus_chebyshev_tu_sum(~A, ~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref x) (array-size tn) (lc-arr-ref tn) (lc-arr-ref un))
  nil)


;;; ---- DOT-PRODUCT ----

(defmacro <dot-product> (result s1 s2 &optional len)
  (format *c-file* "  ~A = mus_dot_product(~A, ~A, ~A);~%"
	  (lc2 result) (lc-arr-ref s1) (lc-arr-ref s2) (if len (lc-num-ref len :integer) (array-size s1)))
  nil)

;;; ---- FFT ----

(defmacro <fft> (rdata idata n &optional (sign 1))
  (format *c-file* "  mus_fft(~A, ~A, ~A, ~A);"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (lc-num-ref n :integer) (lc-num-ref sign :integer))
  nil)

(defmacro <autocorrelate> (rdata n)
  (format *c-file* "  mus_autocorrelate(~A, ~A);"
	  (lc-arr-ref rdata) (lc-num-ref n :integer))
  nil)

(defmacro <correlate> (rdata idata n)
  (format *c-file* "  mus_correlate(~A, ~A, ~A);"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (lc-num-ref n :integer))
  nil)

(defmacro <convolution> (rdata idata n)
  (format *c-file* "  mus_convolution(~A, ~A, ~A);"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (lc-num-ref n :integer))
  nil)

(defmacro <spectrum> (rdata idata window type &optional len)
  (format *c-file* "  mus_spectrum(~A, ~A, ~A, ~A, ~A);~%"
	  (lc-arr-ref rdata) (lc-arr-ref idata) (lc-arr-ref window)
	  (if len (lc-num-ref len :integer) (array-size window))
	  (lc-num-ref type :integer))
  nil)

(defmacro <comment> (func &rest args)
  (format *c-file* "~80,1T/* (~(~A~)~A */~%" func (format nil "~{ ~(~A~)~}" args))
  nil)

(defmacro <hz2radians> (result val)
  (format *c-file* "  ~A = mus_hz_to_radians(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <radians2hz> (result val)
  (format *c-file* "  ~A = mus_radians_to_hz(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <degrees2radians> (result val)
  (format *c-file* "  ~A = mus_degrees_to_radians(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <radians2degrees> (result val)
  (format *c-file* "  ~A = mus_radians_to_degrees(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <db2linear> (result val)
  (format *c-file* "  ~A = mus_db_to_linear(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <linear2db> (result val)
  (format *c-file* "  ~A = mus_linear_to_db(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <seconds2samples> (result val)
  (format *c-file* "  ~A = mus_seconds_to_samples(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defmacro <samples2seconds> (result val)
  (format *c-file* "  ~A = mus_samples_to_seconds(~A);~%" (lc2 result) (lc-num-ref val))
  nil)

(defun pretty-comment (func gen &optional fm1 fm2)
  (format nil "/* (~A ~(~A~)~A~A) */" func gen (if fm1 (format nil " ~(~A~)" fm1) "") (if fm2 (format nil " ~(~A~)" fm2) "")))

(defun run-type->class (type) ;first (second?) step in gen-unload process -- get type, dispatch to method
  (cond
   ((= type +no-type+) nil)
   ((= type +integer+) 1)
   ((= type +real+) 1.0)
   ((= type +real-array+) (make-double-array 1))
   ((= type +integer-array+) (make-integer-array 1 :initial-element 0))
   ((= type +array+) (make-array 1))
   ((= type +string+) "1")
;   ((= type +frame+) (make-instance 'common-tones/generators::frame))
;   ((= type +mixer+) (make-instance 'common-tones/generators::mixer))
   ((= type +oscil+) (make-instance 'common-tones/generators::oscil))
   ((= type +ncos+) (make-instance 'common-tones/generators::ncos))
   ((= type +nrxycos+) (make-instance 'common-tones/generators::nrxycos))
   ((= type +nsin+) (make-instance 'common-tones/generators::nsin))
   ((= type +nrxysin+) (make-instance 'common-tones/generators::nrxysin))
   ((= type +ssb-am+) (make-instance 'common-tones/generators::ssb-am))
   ((= type +rand+) (make-instance 'common-tones/generators::rand))
   ((= type +rand-interp+) (make-instance 'common-tones/generators::rand-interp))
   ((= type +table-lookup+) (make-instance 'common-tones/generators::table-lookup))
   ((= type +square-wave+) (make-instance 'common-tones/generators::square-wave))
   ((= type +pulse-train+) (make-instance 'common-tones/generators::pulse-train))
   ((= type +sawtooth-wave+) (make-instance 'common-tones/generators::sawtooth-wave))
   ((= type +triangle-wave+) (make-instance 'common-tones/generators::triangle-wave))
   ((= type +asymmetric-fm+) (make-instance 'common-tones/generators::asymmetric-fm))
   ((= type +wave-train+) (make-instance 'common-tones/generators::wave-train))
   ((= type +one-pole+) (make-instance 'common-tones/generators::one-pole))
   ((= type +two-pole+) (make-instance 'common-tones/generators::two-pole))
   ((= type +one-zero+) (make-instance 'common-tones/generators::one-zero))
   ((= type +two-zero+) (make-instance 'common-tones/generators::two-zero))
   ((= type +delay+) (make-instance 'common-tones/generators::delay))
   ((= type +tap+) (make-instance 'common-tones/generators::tap))
   ((= type +comb+) (make-instance 'common-tones/generators::comb))
   ((= type +filtered-comb+) (make-instance 'common-tones/generators::filtered-comb))
   ((= type +notch+) (make-instance 'common-tones/generators::notch))
   ((= type +all-pass+) (make-instance 'common-tones/generators::all-pass))
   ((= type +moving-average+) (make-instance 'common-tones/generators::moving-average))
   ((= type +filter+) (make-instance 'common-tones/generators::filter))
   ((= type +fir-filter+) (make-instance 'common-tones/generators::fir-filter))
   ((= type +iir-filter+) (make-instance 'common-tones/generators::iir-filter))
   ((= type +env+) (make-instance 'common-tones/generators::seg))
   ((= type +locsig+) (make-instance 'common-tones/generators::locsig))
   ((= type +move-sound+) (make-instance 'common-tones/generators::move-sound))
   ((= type +src+) (make-instance 'common-tones/generators::src))
   ((= type +granulate+) (make-instance 'common-tones/generators::granulate))
   ((= type +readin+) (make-instance 'common-tones/generators::readin))
   ((= type +convolve+) (make-instance 'common-tones/generators::convolve))
   ((= type +polyshape+) (make-instance 'common-tones/generators::polyshape))
   ((= type +polywave+) (make-instance 'common-tones/generators::polywave))
   ((= type +firmant+) (make-instance 'common-tones/generators::firmant))
   ((= type +formant+) (make-instance 'common-tones/generators::formant))
   ((= type +sample2file+) (make-instance 'common-tones/generators::sample->file))
   ((= type +frample2file+) (make-instance 'common-tones/generators::frample->file))
   ((= type +file2sample+) (make-instance 'common-tones/generators::file->sample))
   ((= type +file2frample+) (make-instance 'common-tones/generators::file->frample))
   ((= type +phase-vocoder+) (make-instance 'common-tones/generators::phase-vocoder))
   (t (warn "unmatched type in run-class->type: ~A" type))))



;;; ---- STRINGS ----


(defmacro <string?> (res arg)
  (lc-check-type res arg +string+ "string?"
		 #'(lambda (arg) (if (and arg (stringp arg)) "true" "false"))
		 #'(lambda (typ) (declare (ignore typ)) "false"))
  nil)

(defmethod gen-load ((gen string) i r datai datar)
  (declare (ignore r datar))

  #+(not (or big-endian little-endian)) (warn "attempting to load a string into run without knowing current endianess...")

  (let ((len (length gen)))

    (setf (aref datai i) +string+)
    (setf (aref datai (+ i 1)) len)
    (progn
      #-little-endian
      (loop for k from 0 below len by 4 and j from (+ i 2) do
	(setf (aref datai j) (+ (if (> len (+ k 3)) (char-code (char gen (+ k 3))) 0)
				(if (> len (+ k 2)) (ash (char-code (char gen (+ k 2))) 8) 0)
				(if (> len (+ k 1)) (ash (char-code (char gen (+ k 1))) 16) 0)
				(if (> len k) (ash (char-code (char gen k)) 24) 0))))
      #+little-endian
      (loop for k from 0 below len by 4 and j from (+ i 2) do
	(setf (aref datai j) (+ (if (> len (+ k 3)) (ash (char-code (char gen (+ k 3))) 24) 0)
				(if (> len (+ k 2)) (ash (char-code (char gen (+ k 2))) 16) 0)
				(if (> len (+ k 1)) (ash (char-code (char gen (+ k 1))) 8) 0)
				(if (> len k) (char-code (char gen k)) 0))))
    )))

(defmethod gen-size ((gen string)) (list (+ (ceiling (1+ (length gen)) 4) 2) 0))

(defun unload-string (i r datai datar)
  (declare (ignore r datar))
  (let* ((len (aref datai (+ i 1)))
	 (str (make-string len)))
    (progn
      #-little-endian
      (loop for k from 0 below len by 4 and j from (+ i 2) do
	(setf (char str k) (code-char (ldb (byte 8 24) (aref datai j))))
	(if (> len (+ k 1)) (setf (char str (+ k 1)) (code-char (ldb (byte 8 16) (aref datai j)))))
	(if (> len (+ k 2)) (setf (char str (+ k 2)) (code-char (ldb (byte 8 8) (aref datai j)))))
	(if (> len (+ k 3)) (setf (char str (+ k 3)) (code-char (ldb (byte 8 0) (aref datai j))))))
      #+little-endian
      (loop for k from 0 below len by 4 and j from (+ i 2) do
	(setf (char str k) (code-char (ldb (byte 8 0) (aref datai j))))
	(if (> len (+ k 1)) (setf (char str (+ k 1)) (code-char (ldb (byte 8 8) (aref datai j)))))
	(if (> len (+ k 2)) (setf (char str (+ k 2)) (code-char (ldb (byte 8 16) (aref datai j)))))
	(if (> len (+ k 3)) (setf (char str (+ k 3)) (code-char (ldb (byte 8 24) (aref datai j)))))))
    str))

(defmethod gen-unload ((gen string) iadr radr datai datar)
  (declare (ignore radr))
  (unload-string (aref datai (+ iadr 1)) (aref datai (+ (aref datai iadr) 1)) datai datar))

(defmacro <probe-file> (res arg)
  ;; actually should return true file name
  (format *c-file* "  ~A = mus_file_probe((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-duration> (res arg)
  (format *c-file* "  ~A = mus_sound_duration((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-chans> (res arg)
  (format *c-file* "  ~A = mus_sound_chans((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-data-format> (res arg)
  (format *c-file* "  ~A = mus_sound_sample_type((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-data-location> (res arg)
  (format *c-file* "  ~A = mus_sound_data_location((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-datum-size> (res arg)
  (format *c-file* "  ~A = mus_sound_datum_size((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-header-type> (res arg)
  (format *c-file* "  ~A = mus_sound_header_type((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-length> (res arg)
  (format *c-file* "  ~A = mus_sound_length((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-samples> (res arg)
  (format *c-file* "  ~A = mus_sound_samples((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-framples> (res arg)
  (format *c-file* "  ~A = mus_sound_framples((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-srate> (res arg)
  (format *c-file* "  ~A = mus_sound_srate((char *)(~A));~%" (lc2 res) (lc-ref arg :string))
  nil)

(defmacro <sound-comment> (res file)
  (format *c-file* "  if (~A) free(~A);~%" (lc2 res) (lc2 res))
  (format *c-file* "  ~A = mus_sound_comment(~A);~%" (lc2 res) (lc2 file))
  nil)



(defun ciadr (iloc offset)
  (format nil "clm_int[clm_int[~A + 1] + ~A]" iloc offset))

(defun cradr (iloc offset)
  (format nil "clm_double[clm_int[clm_int[~A + 1] + 1] + ~A]" iloc offset))


(defmacro <frample2file> (stream pass inf)
  (format *c-file* "  mus_frample_to_file(~A, ~A, ~A);~%" (lc2 stream) (lc-num-ref pass :integer) (lc2 inf))
  nil)

(defmacro <frample2frample> (mx1 fr2 outf)
  (format *c-file* "  mus_frample_to_frample(~A, CLM_ARR_SIZE(CLM_VAR_ADDR(~A)), ~A, CLM_ARR_SIZE(CLM_VAR_ADDR(~A)), ~A, CLM_ARR_SIZE(CLM_VAR_ADDR(~A)));~%"
	  (lc2 mx1) (varinfo-iloc (gethash (second mx1) vars))
	  (lc2 fr2)  (varinfo-iloc (gethash (second fr2) vars))
	  (lc2 outf) (varinfo-iloc (gethash (second outf) vars)))
  nil)



;;; ---- OSCIL ----


(defmethod gen-load ((gen oscil) i r datai datar)
  (setf (aref datai i) +oscil+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (oscil-freq gen)))
  (setf (aref datar (+ r 1)) (double (oscil-phase gen))))

(defmethod gen-size ((gen oscil)) (list 2 2))

(defmethod gen-unload ((gen oscil) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (oscil-freq gen) (aref datar r))
    (setf (oscil-phase gen) (aref datar (+ r 1)))
    gen))

(defmacro <oscil?> (res arg)
  (format *c-file* "  ~A = mus_is_oscil(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <oscil> (result s &optional fm pm)
  (let* ((pm-in-use (and pm (or (not (constantp pm)) (not (zerop pm)))))
	 (fm-in-use (and fm (or (not (constantp fm)) (not (zerop fm)))))
	 (cname (lc2 s))
	 (resname (lc2 result)))
    (when (> *safety* 0)
      (format *c-file* "  if (!(mus_is_oscil(~A))) {mus_error(0, \"oscil ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	      cname (clean-arg s) cname))
    (if pm-in-use
	(format *c-file* "  ~A = mus_oscil(~A, ~A, ~A);~80,1T/* (oscil ~A ~A ~A) */~%"
		resname cname (lc-num-ref fm) (lc-num-ref pm) (clean-arg s) (clean-arg fm) (clean-arg pm))
      (if fm-in-use
	  (format *c-file* "  ~A = mus_oscil_fm(~A, ~A);~80,1T/* (oscil ~A ~A) */~%"
		  resname cname (lc-num-ref fm) (clean-arg s) (clean-arg fm))
	(format *c-file* "  ~A = mus_oscil_unmodulated(~A);~80,1T/* (oscil ~A) */~%"
		resname cname (clean-arg s))))
    nil))

(defmethod gen-reflect ((gen oscil) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_oscil(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen oscil) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_OSCIL_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_oscil(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc))



;;; ---- SAWTOOTH-WAVE, SQUARE-WAVE, PULSE-TRAIN, TRIANGLE-WAVE ----


(defun load-sw (gen i r datai datar)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (sw-freq gen)))
  (setf (aref datar (+ r 1)) (double (sw-phase gen)))
  (setf (aref datar (+ r 2)) (double (mus-scaler gen))))

(defmethod gen-load ((gen triangle-wave) i r datai datar)
  (setf (aref datai i) +triangle-wave+)
  (load-sw gen i r datai datar))

(defmethod gen-load ((gen sawtooth-wave) i r datai datar)
  (setf (aref datai i) +sawtooth-wave+)
  (load-sw gen i r datai datar))

(defmethod gen-load ((gen square-wave) i r datai datar)
  (setf (aref datai i) +square-wave+)
  (load-sw gen i r datai datar))

(defmethod gen-load ((gen pulse-train) i r datai datar)
  (setf (aref datai i) +pulse-train+)
  (load-sw gen i r datai datar))

(defmethod gen-size ((gen triangle-wave)) (list 2 3)) ; should cover all cases

(defun unload-sw (gen iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (sw-freq gen) (aref datar r))
    (setf (sw-phase gen) (aref datar (+ r 1)))
    (setf (mus-scaler gen) (aref datar (+ r 2)))
    gen))

(defmethod gen-unload ((gen triangle-wave) i r datai datar) (unload-sw gen i r datai datar)) ; covers all?

(defmacro <square-wave?> (res arg)
  (format *c-file* "  ~A = mus_is_square_wave(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <pulse-train?> (res arg)
  (format *c-file* "  ~A = mus_is_pulse_train(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <triangle-wave?> (res arg)
  (format *c-file* "  ~A = mus_is_triangle_wave(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <sawtooth-wave?> (res arg)
  (format *c-file* "  ~A = mus_is_sawtooth_wave(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <square-wave> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_square_wave(~A))) {mus_error(0, \"square-wave ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_square_wave(~A, ~A);~80,1T/* (square-wave ~A~A) */~%"
	  (lc2 result) (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s) (if fm (clean-arg fm) ""))
  nil)

(defmacro <pulse-train> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_pulse_train(~A))) {mus_error(0, \"pulse-train ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_pulse_train(~A, ~A);~80,1T/* (pulse-train ~A~A) */~%"
	  (lc2 result) (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s) (if fm (clean-arg fm) ""))
  nil)

(defmacro <triangle-wave> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_triangle_wave(~A))) {mus_error(0, \"triangle-wave ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_triangle_wave(~A, ~A);~80,1T/* (triangle-wave ~A~A) */~%"
	  (lc2 result) (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s) (if fm (clean-arg fm) ""))
  nil)

(defmacro <sawtooth-wave> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_sawtooth_wave(~A))) {mus_error(0, \"sawtooth-wave ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_sawtooth_wave(~A, ~A);~80,1T/* (sawtooth-wave ~A~A) */~%"
	  (lc2 result) (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s) (if fm (clean-arg fm) ""))
  nil)

(defmethod gen-reflect ((gen triangle-wave) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_triangle_wave(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
    		(cradr iadr 2) name)))

(defmethod gen-reflect ((gen square-wave) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_square_wave(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
    		(cradr iadr 2) name)))

(defmethod gen-reflect ((gen sawtooth-wave) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_sawtooth_wave(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
    		(cradr iadr 2) name)))

(defmethod gen-reflect ((gen pulse-train) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_pulse_train(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
    		(cradr iadr 2) name)))


(defmethod gen-make ((gen triangle-wave) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_TRIANGLE_WAVE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_triangle_wave(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))

(defmethod gen-make ((gen square-wave) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_SQUARE_WAVE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_square_wave(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))

(defmethod gen-make ((gen sawtooth-wave) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_SAWTOOTH_WAVE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_sawtooth_wave(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))

(defmethod gen-make ((gen pulse-train) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_PULSE_TRAIN_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_pulse_train(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))



;;; ---- NCOS ----


(defmethod gen-load ((s ncos) i r datai datar)
  (setf (aref datai i) +ncos+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (ncosp-n s))
  (setf (aref datar r) (double (ncosp-freq s)))
  (setf (aref datar (+ r 1)) (double (ncosp-phase s)))
  (setf (aref datar (+ r 2)) (double (ncosp-scaler s))))

(defmethod gen-unload ((gen ncos) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (ncosp-n gen) (aref datai (+ i 2)))
    (setf (ncosp-freq gen) (aref datar r))
    (setf (ncosp-phase gen) (aref datar (+ r 1)))
    (setf (ncosp-scaler gen) (aref datar (+ r 2)))
    gen))

(defmethod gen-reflect ((gen ncos) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_ncos(~A))~%    {~%     ~
                           ~A = mus_length(~A);~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    }~%"
		name
		(ciadr iadr 2) name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name)))

(defmethod gen-size ((gen ncos)) (list 3 3))

(defmethod gen-make ((gen ncos) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_NCOS_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_ncos(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc))

(defmacro <ncos> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_ncos(~A))) {mus_error(0, \"ncos ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_ncos(~A, ~A);~80,1T/* (ncos ~A~A) */~%"
	  (lc2 result)
	  (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s)
	  (if fm (format nil " ~A" (clean-arg fm)) ""))
  nil)

(defmacro <ncos?> (res arg)
  (format *c-file* "  ~A = mus_is_ncos(~A);~%" (lc2 res) (lc2 arg))
  nil)


;;; ---- NSIN ----


(defmethod gen-load ((s nsin) i r datai datar)
  (setf (aref datai i) +nsin+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (ncosp-n s))
  (setf (aref datar r) (double (ncosp-freq s)))
  (setf (aref datar (+ r 1)) (double (ncosp-phase s)))
  (setf (aref datar (+ r 2)) (double (ncosp-scaler s))))

(defmethod gen-unload ((gen nsin) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (ncosp-n gen) (aref datai (+ i 2)))
    (setf (ncosp-freq gen) (aref datar r))
    (setf (ncosp-phase gen) (aref datar (+ r 1)))
    (setf (ncosp-scaler gen) (aref datar (+ r 2)))
    gen))

(defmethod gen-reflect ((gen nsin) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_nsin(~A))~%    {~%     ~
                           ~A = mus_length(~A);~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%    }~%"
		name
		(ciadr iadr 2) name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name)))

(defmethod gen-size ((gen nsin)) (list 3 3))

(defmethod gen-make ((gen nsin) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_NSIN_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_nsin(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc))

(defmacro <nsin> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_nsin(~A))) {mus_error(0, \"nsin ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_nsin(~A, ~A);~80,1T/* (nsin ~A~A) */~%"
	  (lc2 result)
	  (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s)
	  (if fm (format nil " ~A" (clean-arg fm)) ""))
  nil)

(defmacro <nsin?> (res arg)
  (format *c-file* "  ~A = mus_is_nsin(~A);~%" (lc2 res) (lc2 arg))
  nil)


;;; ---- NRXYCOS ----


(defmethod gen-load ((s nrxycos) i r datai datar)
  (setf (aref datai i) +nrxycos+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (nrxy-n s))
  (setf (aref datar r) (double (nrxy-freq s)))
  (setf (aref datar (+ r 1)) (double (nrxy-phase s)))
  (setf (aref datar (+ r 2)) (double (nrxy-r s)))
  (setf (aref datar (+ r 3)) (double (nrxy-ratio s))))

(defmethod gen-unload ((gen nrxycos) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (nrxy-n gen) (aref datai (+ i 2)))
    (setf (nrxy-freq gen) (aref datar r))
    (setf (nrxy-phase gen) (aref datar (+ r 1)))
    (setf (nrxy-r gen) (aref datar (+ r 2)))
    (setf (nrxy-ratio gen) (aref datar (+ r 3)))
    gen))

(defmethod gen-reflect ((gen nrxycos) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_nrxycos(~A))~%    {~%     ~
                           ~A = mus_length(~A);~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%     ~
                           ~A = mus_offset(~A);~%    }~%"
		name
		(ciadr iadr 2) name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name
		(cradr iadr 3) name)))

(defmethod gen-size ((gen nrxycos)) (list 3 4))

(defmethod gen-make ((gen nrxycos) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_NRXYCOS_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_nrxycos(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 3],~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc indent iloc))

(defmacro <nrxycos> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_nrxycos(~A))) {mus_error(0, \"nrxycos ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_nrxycos(~A, ~A);~80,1T/* (nrxycos ~A~A) */~%"
	  (lc2 result)
	  (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s)
	  (if fm (format nil " ~A" (clean-arg fm)) ""))
  nil)

(defmacro <nrxycos?> (res arg)
  (format *c-file* "  ~A = mus_is_nrxycos(~A);~%" (lc2 res) (lc2 arg))
  nil)


;;; ---- NRXYSIN ----


(defmethod gen-load ((s nrxysin) i r datai datar)
  (setf (aref datai i) +nrxysin+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (nrxy-n s))
  (setf (aref datar r) (double (nrxy-freq s)))
  (setf (aref datar (+ r 1)) (double (nrxy-phase s)))
  (setf (aref datar (+ r 2)) (double (nrxy-r s)))
  (setf (aref datar (+ r 3)) (double (nrxy-ratio s))))

(defmethod gen-unload ((gen nrxysin) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (nrxy-n gen) (aref datai (+ i 2)))
    (setf (nrxy-freq gen) (aref datar r))
    (setf (nrxy-phase gen) (aref datar (+ r 1)))
    (setf (nrxy-r gen) (aref datar (+ r 2)))
    (setf (nrxy-ratio gen) (aref datar (+ r 3)))
    gen))

(defmethod gen-reflect ((gen nrxysin) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_nrxysin(~A))~%    {~%     ~
                           ~A = mus_length(~A);~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A);~%     ~
                           ~A = mus_offset(~A);~%    }~%"
		name
		(ciadr iadr 2) name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name
		(cradr iadr 3) name)))

(defmethod gen-size ((gen nrxysin)) (list 3 4))

(defmethod gen-make ((gen nrxysin) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_NRXYSIN_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_nrxysin(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 3],~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc indent iloc))

(defmacro <nrxysin> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_nrxysin(~A))) {mus_error(0, \"nrxysin ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_nrxysin(~A, ~A);~80,1T/* (nrxysin ~A~A) */~%"
	  (lc2 result)
	  (lc2 s)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s)
	  (if fm (format nil " ~A" (clean-arg fm)) ""))
  nil)

(defmacro <nrxysin?> (res arg)
  (format *c-file* "  ~A = mus_is_nrxysin(~A);~%" (lc2 res) (lc2 arg))
  nil)



;;; ---- SSB-AM ----


(defmethod gen-load ((gen ssb-am) i r datai datar)
  ;; (format t "load gen at int: ~D (~D), double: ~D (~F)~%" i (mus-order gen) r (mus-frequency gen))
  (setf (aref datai i) +ssb-am+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (mus-order gen))
  (setf (aref datar r) (double (mus-frequency gen))))

(defmethod gen-size ((gen ssb-am)) (list 3 1))

(defmethod gen-unload ((gen ssb-am) i r datai datar)
  (declare (ignore i r datai datar))
  nil)

(defmethod gen-reflect ((gen ssb-am) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen ssb-am) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_SSB_AM_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens,~%~A    ~
                         mus_make_ssb_am(clm_double[CLM_RLOC(~A)],~%~A      ~
                          clm_int[CLM_ILOC(~A) + 2]));~%"
	  indent iloc indent result indent iloc indent iloc))

(defmacro <ssb-am?> (res arg)
  (format *c-file* "  ~A = mus_is_ssb_am(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <ssb-am> (result s insig &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_ssb_am(~A))) {mus_error(0, \"ssb-am ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (let* ((fm-in-use (and fm (or (not (constantp fm)) (not (zerop fm)))))
	 (cname (lc2 s))
	 (resname (lc2 result)))
    (if fm-in-use
	(format *c-file* "  ~A = mus_ssb_am(~A, ~A, ~A);~80,1T/* (ssb-am ~A ~A ~A) */~%"
		resname cname (lc-num-ref insig) (lc-num-ref fm)
		(clean-arg s) (clean-arg insig) (clean-arg fm))
      (format *c-file* "  ~A = mus_ssb_am_unmodulated(~A, ~A);~80,1T/* (ssb-am ~A ~A) */~%"
	      resname cname (lc-num-ref insig)
	      (clean-arg s) (clean-arg insig)))
    nil))


;;; ---- RAND, RAND-INTERP ----


(defun noi-load (s i r datai datar)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (if (noi-distribution s) (length (noi-distribution s)) 0))
  (setf (aref datar r) (double (mus-frequency s)))
  (setf (aref datar (+ r 1)) (double (noi-base s)))
  (if (noi-distribution s)
      (load-real-array (noi-distribution s) (+ i 3) (+ r 2) datai datar)))

(defmethod gen-load((s rand) i r datai datar)
  (setf (aref datai i) +rand+)
  (noi-load s i r datai datar))

(defmethod gen-load((s rand-interp) i r datai datar)
  (setf (aref datai i) +rand-interp+)
  (noi-load s i r datai datar))

(defmethod gen-size ((gen rand))
  (if (noi-distribution gen)
      (list (+ 3 6)
	    (+ 2 (length (noi-distribution gen))))
    (list 3 2)))

(defmethod gen-size ((gen rand-interp))
  (if (noi-distribution gen)
      (list (+ 3 6)
	    (+ 2 (length (noi-distribution gen))))
    (list 3 2)))

(defun noi-unload (s iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (noi-freq s) (hz->radians (aref datar r)))
    (setf (noi-base s) (aref datar (+ r 1)))
    (if (> (aref datai (+ i 2)) 0)
	(setf (noi-distribution s) (unload-real-array (+ i 3) datai datar)))
    s))

(defmethod gen-unload((s rand) i r datai datar)
  (noi-unload s i r datai datar))

(defmethod gen-unload((s rand-interp) i r datai datar)
  (noi-unload s i r datai datar))

(defmacro <rand?> (res arg)
  (format *c-file* "  ~A = mus_is_rand(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <rand-interp?> (res arg)
  (format *c-file* "  ~A = mus_is_rand_interp(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <rand> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_rand(~A))) {mus_error(0, \"rand ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_rand(~A, ~A);~%" (lc2 result) (lc2 s) (if fm (lc-num-ref fm) "0.0"))
  nil)

(defmacro <rand-interp> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_rand_interp(~A))) {mus_error(0, \"rand-interp ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_rand_interp(~A, ~A);~%" (lc2 result) (lc2 s) (if fm (lc-num-ref fm) "0.0"))
  nil)

(defmethod gen-reflect ((gen rand) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_rand(~A))~%    ~
                          {~%     ~
                           ~A = mus_frequency(~A);~%     ~
                           ~A = mus_scaler(~A); /* amp */~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-reflect ((gen rand-interp) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_rand_interp(~A))~%    ~
                          {~%     ~
                           ~A = mus_frequency(~A);~%     ~
                           ~A = mus_scaler(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen rand) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_RAND_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_rand_with_distribution(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           (clm_int[CLM_ILOC(~A) + 2] > 0) ? ((double *)(clm_double + CLM_RLOC(~A) + 2)) : NULL,~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2]));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc iloc indent
	  iloc))

(defmethod gen-make ((gen rand-interp) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_RAND_INTERP_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_rand_interp_with_distribution(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           (clm_int[CLM_ILOC(~A) + 2] > 0) ? ((double *)(clm_double + CLM_RLOC(~A) + 2)) : NULL,~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2]));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc iloc indent
	  iloc))


;;; ---- TABLE-LOOKUP ----


(defmethod gen-load ((tbl table-lookup) i r datai datar)
  (setf (aref datai i) +table-lookup+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (length (tbl-wave tbl)))
  (setf (aref datai (+ i 3)) (tbl-type tbl))
  (setf (aref datar r) (double (mus-frequency tbl)))
  (setf (aref datar (+ r 1)) (double (mus-phase tbl)))
  (load-real-array (tbl-wave tbl) (+ i 4) (+ r 2) datai datar))

(defmethod gen-unload ((gen table-lookup) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (tbl-wave gen) (unload-real-array (+ i 4) datai datar))
    (setf (mus-frequency gen) (aref datar r))
    (setf (mus-phase gen) (aref datar (+ r 1)))
    gen))

(defmethod gen-size ((gen table-lookup))
  (list (+ 4 6) (+ 2 (length (tbl-wave gen)))))

(defmacro <table-lookup> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_table_lookup(~A))) {mus_error(0, \"table-lookup ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if fm
      (format *c-file* "  ~A = mus_table_lookup(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref fm))
    (format *c-file* "  ~A = mus_table_lookup_unmodulated(~A);~%" (lc2 result) (lc2 s)))
  nil)

(defmacro <table-lookup?> (res arg)
  (format *c-file* "  ~A = mus_is_table_lookup(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <array-interp> (result s index &optional size)
  (if (integerp index)
      (format *c-file* "  ~A = ~A;~%"
	      (lc2 result) (real-array-ref s index))
    (format *c-file* "  ~A = mus_array_interp(~A, ~A, ~A);~%"
	    (lc2 result) (lc-arr-ref s) (lc-num-ref index) (if size (lc-num-ref size :integer) (array-size s))))
  nil)

(defmacro <mus-interpolate> (result type s index &optional size yn1)
  (format *c-file* "  ~A = mus_interpolate(~A, ~A, ~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref type :integer)
	  (lc-arr-ref s) (lc-num-ref index)
	  (if size (lc-num-ref size :integer) (array-size s))
	  (if yn1 (lc-num-ref yn1) 0.0))
  nil)

(defmethod gen-reflect ((gen table-lookup) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_table_lookup(~A))~%    ~
                          {~%     ~
                           ~A = mus_frequency(~A);~%     ~
                           ~A = mus_phase(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen table-lookup) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_TABLE_LOOKUP_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_table_lookup(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3])); /* type */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))


(defmacro <file2array> (filename chan start framples arr)
  (format *c-file* "  mus_file_to_float_array((char *)(~A), ~A, ~A, ~A, ~A);~%"
	  (lc-ref filename :string)
	  (lc-num-ref chan :integer)
	  (lc-num-ref start :integer)
	  (lc-num-ref framples :integer)
	  (lc-arr-ref arr))
  nil)

(defmacro <array2file> (filename arr samps srate chans) ;assumes mus-bfloat and next header
  (format *c-file* "  mus_float_array_to_file((char *)(~A), ~A, ~A, (int)(~A), ~A);~%"
	  (lc-ref filename :string)
	  (lc-arr-ref arr)
	  (lc-num-ref samps :integer)
	  (lc-num-ref srate)
	  (lc-num-ref chans :integer))
  nil)


;;; ---- ONE-POLE ----


(defmethod gen-load ((s one-pole) i r datai datar)
  (setf (aref datai i) +one-pole+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (flt-a0 s)))
  (setf (aref datar (+ r 1)) (double (flt-b1 s))))

(defmethod gen-size ((s one-pole)) (list 2 2))

(defmethod gen-unload ((s one-pole) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (flt-a0 s) (aref datar r))
    (setf (flt-b1 s) (aref datar (+ r 1)))
    s))

(defmacro <one-pole> (result s arg)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_one_pole(~A))) {mus_error(0, \"one-pole ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_one_pole(~A, ~A);~80,1T/* (one-pole ~A ~A) */~%"
	  (lc2 result) (lc2 s) (lc-num-ref arg)
	  (clean-arg s) (clean-arg arg))
  nil)

(defmacro <one-pole?> (res arg)
  (format *c-file* "  ~A = mus_is_one_pole(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen one-pole) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_one_pole(~A))~%    ~
                          {~%     ~
                           ~A = mus_xcoeff(~A, 0);~%     ~
                           ~A = mus_ycoeff(~A, 1);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen one-pole) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_ONE_POLE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_one_pole(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc))


;;; ---- ONE-ZERO ----


(defmethod gen-load ((s one-zero) i r datai datar)
  (setf (aref datai i) +one-zero+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (flt-a0 s)))
  (setf (aref datar (+ r 1)) (double (flt-a1 s))))

(defmethod gen-size ((s one-zero)) (list 2 2))

(defmethod gen-unload ((s one-zero) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (flt-a0 s) (aref datar r))
    (setf (flt-a1 s) (aref datar (+ r 1)))
    s))

(defmacro <one-zero> (result s arg)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_one_zero(~A))) {mus_error(0, \"one-zero ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_one_zero(~A, ~A);~80,1T/* (one-zero ~A ~A) */~%"
	  (lc2 result) (lc2 s) (lc-num-ref arg)
	  (clean-arg s) (clean-arg arg))
  nil)

(defmacro <one-zero?> (res arg)
  (format *c-file* "  ~A = mus_is_one_zero(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen one-zero) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_one_zero(~A))~%    ~
                          {~%     ~
                           ~A = mus_xcoeff(~A, 0);~%     ~
                           ~A = mus_xcoeff(~A, 1);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen one-zero) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_ONE_ZERO_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_one_zero(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1]));~%"
	  indent iloc indent result indent indent iloc indent iloc))



;;; ---- TWO-POLE ----


(defmethod gen-load ((s two-pole) i r datai datar)
  (setf (aref datai i) +two-pole+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (flt-a0 s)))
  (setf (aref datar (+ r 1)) (double (flt-b1 s)))
  (setf (aref datar (+ r 2)) (double (flt-b2 s))))

(defmethod gen-size ((s two-pole)) (list 2 3))

(defmethod gen-unload ((s two-pole) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (flt-a0 s) (aref datar r))
    (setf (flt-b1 s) (aref datar (+ r 1)))
    (setf (flt-b2 s) (aref datar (+ r 2)))
    s))

(defmacro <two-pole> (result s arg)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_two_pole(~A))) {mus_error(0, \"two-pole ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_two_pole(~A, ~A);~80,1T/* (two-pole ~A ~A) */~%"
	  (lc2 result) (lc2 s) (lc-num-ref arg)
	  (clean-arg s) (clean-arg arg))
  nil)

(defmacro <two-pole?> (res arg)
  (format *c-file* "  ~A = mus_is_two_pole(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen two-pole) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_two_pole(~A))~%    ~
                          {~%     ~
                           ~A = mus_xcoeff(~A, 0);~%     ~
                           ~A = mus_ycoeff(~A, 1);~%     ~
                           ~A = mus_ycoeff(~A, 2);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name)))

(defmethod gen-make ((gen two-pole) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_TWO_POLE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_two_pole(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))


;;; ---- TWO-ZERO ----


(defmethod gen-load ((s two-zero) i r datai datar)
  (setf (aref datai i) +two-zero+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (flt-a0 s)))
  (setf (aref datar (+ r 1)) (double (flt-a1 s)))
  (setf (aref datar (+ r 2)) (double (flt-a2 s))))

(defmethod gen-size ((s two-zero)) (list 2 3))

(defmethod gen-unload ((s two-zero) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (flt-a0 s) (aref datar r))
    (setf (flt-a1 s) (aref datar (+ r 1)))
    (setf (flt-a2 s) (aref datar (+ r 2)))
    s))

(defmacro <two-zero> (result s arg)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_two_zero(~A))) {mus_error(0, \"two-zero ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_two_zero(~A, ~A);~80,1T/* (two-zero ~A ~A) */~%"
	  (lc2 result) (lc2 s) (lc-num-ref arg)
	  (clean-arg s) (clean-arg arg))
  nil)

(defmacro <two-zero?> (res arg)
  (format *c-file* "  ~A = mus_is_two_zero(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen two-zero) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_two_zero(~A))~%    ~
                          {~%     ~
                           ~A = mus_xcoeff(~A, 0);~%     ~
                           ~A = mus_xcoeff(~A, 1);~%     ~
                           ~A = mus_xcoeff(~A, 2);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 2) name)))

(defmethod gen-make ((gen two-zero) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_TWO_ZERO_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_two_zero(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc))


;;; ---- FORMANT ----


(defmethod gen-load ((s formant) i r datai datar)
  (setf (aref datai i) +formant+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (radius s)))
  (setf (aref datar (+ r 1)) (double (frequency s))))

(defmethod gen-size ((s formant)) (list 2 3))

(defmethod gen-unload ((s formant) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (mus-scaler s) (aref datar r))
    (setf (mus-frequency s) (aref datar (+ r 1)))
    s))

(defmacro <formant> (result s arg &optional freq)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_formant(~A))) {mus_error(0, \"formant ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if freq
      (format *c-file* "  ~A = mus_formant_with_frequency(~A, ~A, ~A);~80,1T/* (formant ~A ~A ~A) */~%"
	      (lc2 result) (lc2 s) (lc-num-ref arg) (lc-num-ref freq)
	      (clean-arg s) (clean-arg arg) (clean-arg freq))
    (format *c-file* "  ~A = mus_formant(~A, ~A);~80,1T/* (formant ~A ~A) */~%"
	    (lc2 result) (lc2 s) (lc-num-ref arg)
	    (clean-arg s) (clean-arg arg)))
  nil)

(defmacro <formant?> (res arg)
  (format *c-file* "  ~A = mus_is_formant(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen formant) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_formant(~A))~%    ~
                          {~%     ~
                           ~A = mus_scaler(~A);~%     ~
                           ~A = mus_frequency(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen formant) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_FORMANT_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_formant(~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A)]));~%"
	  indent iloc indent result indent indent iloc indent iloc))


;;; ---- FIRMANT ----


(defmethod gen-load ((s firmant) i r datai datar)
  (setf (aref datai i) +firmant+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (frm-radius s)))
  (setf (aref datar (+ r 1)) (double (frm-frequency s))))

(defmethod gen-size ((s firmant)) (list 2 3))

(defmethod gen-unload ((s firmant) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (mus-scaler s) (aref datar r))
    (setf (mus-frequency s) (aref datar (+ r 1)))
    s))

(defmacro <firmant> (result s arg &optional freq)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_firmant(~A))) {mus_error(0, \"firmant ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if freq
      (format *c-file* "  ~A = mus_firmant_with_frequency(~A, ~A, ~A);~80,1T/* (firmant ~A ~A ~A) */~%"
	      (lc2 result) (lc2 s) (lc-num-ref arg) (lc-num-ref freq)
	      (clean-arg s) (clean-arg arg) (clean-arg freq))
    (format *c-file* "  ~A = mus_firmant(~A, ~A);~80,1T/* (firmant ~A ~A) */~%"
	    (lc2 result) (lc2 s) (lc-num-ref arg)
	    (clean-arg s) (clean-arg arg)))
  nil)

(defmacro <firmant?> (res arg)
  (format *c-file* "  ~A = mus_is_firmant(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen firmant) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_firmant(~A))~%    ~
                          {~%     ~
                           ~A = mus_scaler(~A);~%     ~
                           ~A = mus_frequency(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen firmant) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_FIRMANT_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_firmant(~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A)]));~%"
	  indent iloc indent result indent indent iloc indent iloc))



;;; ---- POLYSHAPE ----


(defmethod gen-load ((w polyshape) i r datai datar)
  (setf (aref datai i) +polyshape+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (length (ws-wave w)))
  (setf (aref datar r) (double (ws-freq w)))
  (setf (aref datar (+ r 1)) (double (ws-phase w)))
  (load-real-array (ws-wave w) (+ i 3) (+ r 2) datai datar))

(defmethod gen-size ((s polyshape))
  (list (+ 3 6)
	(+ 2 (length (ws-wave s)))))

(defmethod gen-unload ((s polyshape) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (ws-freq s) (aref datar r))
    (setf (ws-phase s) (aref datar (+ r 1)))
    (setf (ws-wave s) (unload-real-array (+ i 3) datai datar))
    s))

(defmacro <polyshape?> (res arg)
  (format *c-file* "  ~A = mus_is_polyshape(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <polyshape> (result s &optional (index 1.0) fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_polyshape(~A))) {mus_error(0, \"polyshape ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if fm
      (format *c-file* "  ~A = mus_polyshape(~A, ~A, ~A);~80,1T/* (polyshape ~A ~A ~A) */~%"
	      (lc2 result) (lc2 s) (lc-num-ref index) (lc-num-ref fm)
	      (clean-arg s) (clean-arg index) (clean-arg fm))
    (if index
	(format *c-file* "  ~A = mus_polyshape_unmodulated(~A, ~A);~80,1T/* (polyshape ~A ~A) */~%"
		(lc2 result) (lc2 s) (lc-num-ref index)
		(clean-arg s) (clean-arg index))
      (format *c-file* "  ~A = mus_polyshape_no_input(~A);~80,1T/* (polyshape ~A) */~%"
		(lc2 result) (lc2 s)
		(clean-arg s))))
  nil)

(defmethod gen-reflect ((gen polyshape) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_polyshape(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen polyshape) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_POLYSHAPE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_polyshape(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2], MUS_CHEBYSHEV_FIRST_KIND));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))



;;; ---- POLYWAVE ----


(defmethod gen-load ((w polywave) i r datai datar)
  (setf (aref datai i) +polywave+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (length (ws-wave w)))
  (setf (aref datar r) (double (ws-freq w)))
  (setf (aref datar (+ r 1)) (double (ws-phase w)))
  (load-real-array (ws-wave w) (+ i 3) (+ r 2) datai datar))

(defmethod gen-size ((s polywave))
  (list (+ 3 6)
	(+ 2 (length (ws-wave s)))))

(defmethod gen-unload ((s polywave) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (ws-freq s) (aref datar r))
    (setf (ws-phase s) (aref datar (+ r 1)))
    (setf (ws-wave s) (unload-real-array (+ i 3) datai datar))
    s))

(defmacro <polywave?> (res arg)
  (format *c-file* "  ~A = mus_is_polywave(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <polywave> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_polywave(~A))) {mus_error(0, \"polywave ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if fm
      (format *c-file* "  ~A = mus_polywave(~A, ~A);~80,1T/* (polywave ~A ~A) */~%"
	      (lc2 result) (lc2 s) (lc-num-ref fm)
	      (clean-arg s) (clean-arg fm))
    (format *c-file* "  ~A = mus_polywave_unmodulated(~A);~80,1T/* (polywave ~A) */~%"
	    (lc2 result) (lc2 s)
	    (clean-arg s)))
  nil)

(defmethod gen-reflect ((gen polywave) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_polywave(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen polywave) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_POLYWAVE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_polywave(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2], MUS_CHEBYSHEV_FIRST_KIND));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc))




;;; ---- ASYMMETRIC-FM ----

;;;

;;; 0:type 1:datar

;;; 0:freq 1:phase 2:ratio 3:r


(defmethod gen-load ((w asymmetric-fm) i r datai datar)
  (setf (aref datai i) +asymmetric-fm+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (asymfm-freq w)))
  (setf (aref datar (+ r 1)) (double (asymfm-phase w)))
  (setf (aref datar (+ r 2)) (double (asymfm-ratio w)))
  (setf (aref datar (+ r 3)) (double (asymfm-r w))))

(defmethod gen-size ((gen asymmetric-fm)) (list 2 4))

(defmethod gen-unload ((gen asymmetric-fm) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (asymfm-freq gen) (aref datar r))
    (setf (asymfm-phase gen) (aref datar (+ r 1)))
    (setf (asymfm-ratio gen) (aref datar (+ r 2)))
    (setf (asymfm-r gen) (aref datar (+ r 3)))
    gen))

(defmacro <asymmetric-fm?> (res arg)
  (format *c-file* "  ~A = mus_is_asymmetric_fm(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <asymmetric-fm> (result s index &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_asymmetric_fm(~A))) {mus_error(0, \"asymmetric-fm ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_asymmetric_fm(~A, ~A, ~A);~80,1T/* (asymmetric-fm ~A ~A~A) */~%"
	  (lc2 result) (lc2 s) (lc-num-ref index)
	  (if fm (lc-num-ref fm) "0.0")
	  (clean-arg s) (clean-arg index)
	  (if fm (format nil " ~A" (clean-arg fm)) ""))
  nil)

(defmethod gen-reflect ((gen asymmetric-fm) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_asymmetric_fm(~A))~%    ~
                          {~%     ~
                           ~A = mus_hz_to_radians(mus_frequency(~A));~%     ~
                           ~A = mus_phase(~A);~%     ~
                           ~A = mus_scaler(~A); /* a */~%     ~
                           ~A = mus_increment(~A); /* ratio */~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name
		(cradr iadr 3) name
		(cradr iadr 2) name)))

(defmethod gen-make ((gen asymmetric-fm) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_ASYMMETRIC_FM_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_asymmetric_fm(~%~A      ~
                           mus_radians_to_hz(clm_double[CLM_RLOC(~A)]),~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 3],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2]));~%"
	  indent iloc indent result indent indent iloc indent iloc indent iloc indent iloc))



;;; ---- OUT-ANY


(defmacro <out-any> (result pass val &optional (chan 0) stream)
  (if (not stream)
      (setf stream (current-var '*output*)))
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_output(~A))) {mus_error(0, \"output stream ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 stream) (clean-arg stream) (lc2 stream)))
  (format *c-file* "  ~A = mus_out_any(~A, ~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref pass :integer) (lc-num-ref val) (lc-num-ref chan :integer) (lc2 stream))
  nil)

(defmacro <mus-output?> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_is_output(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmethod gen-load ((rd sample->file) i r datai datar)
  (setf (aref datai i) +sample2file+)
  (gen-load (mus-file-name rd) (+ i 1) r datai datar))

(defmethod gen-size ((rd sample->file))
  (list (+ 1 2 (ceiling (1+ (length (mus-file-name rd))) 4)) 0))

(defmethod gen-unload ((rd sample->file) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmethod gen-load ((rd frample->file) i r datai datar)
  (setf (aref datai i) +frample2file+)
  (gen-load (mus-file-name rd) (+ i 1) r datai datar))

(defmethod gen-size ((rd frample->file))
  (list (+ 1 2 (ceiling (1+ (length (mus-file-name rd))) 4)) 0))

(defmethod gen-unload ((rd frample->file) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmethod gen-reflect ((gen sample->file) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-reflect ((gen frample->file) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen sample->file) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif ((CLM_SAMPLE2FILE_P(~A)) || (CLM_FRAMPLE2FILE_P(~A)))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_sample_to_file_with_comment((char *)(clm_int + CLM_ILOC(~A) + 1 + 2), 1, MUS_BSHORT, MUS_NEXT, NULL));~%"
	  indent iloc iloc indent
	  result indent
	  iloc))

(defmethod gen-make ((gen frample->file) stream result iloc indent ctr)
  (declare (ignore ctr))
  (if (equal result "_OUTPUT_")
      (format stream "~A_OUTPUT_ = clm_output();~%" indent)
    (if (equal result "_REVERB_")
	(format stream "~A_REVERB_ = clm_reverb();~%" indent)
      (format stream "~Aif ((CLM_FRAMPLE2FILE_P(~A)) || (CLM_SAMPLE2FILE_P(~A)))~%~A  ~
                        ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                           mus_make_frample_to_file_with_comment((char *)(clm_int + CLM_ILOC(~A) + 1 + 2), 1, MUS_BSHORT, MUS_NEXT, NULL));~%"
	  indent iloc iloc indent
	  result indent
	  iloc))))


;;; -------- IN-ANY


(defmethod gen-load ((rd file->sample) i r datai datar)
  (setf (aref datai i) +file2sample+)
  (setf (aref datai (+ i 1)) (f2s-size rd))
  (gen-load (mus-file-name rd) (+ i 2) r datai datar))

(defmethod gen-size ((rd file->sample))
  (list (+ 2 2 (ceiling (1+ (length (mus-file-name rd))) 4)) 0))

(defmethod gen-unload ((rd file->sample) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmethod gen-load ((rd file->frample) i r datai datar)
  (setf (aref datai i) +file2frample+)
  (setf (aref datai (+ i 1)) (f2f-size rd))
  (gen-load (mus-file-name rd) (+ i 2) r datai datar))

(defmethod gen-size ((rd file->frample))
  (list (+ 2 2 (ceiling (1+ (length (mus-file-name rd))) 4)) 0))

(defmethod gen-unload ((rd file->frample) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmacro <in-any> (result samp chan i-stream)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_input(~A))) {mus_error(0, \"input stream ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 i-stream) (clean-arg i-stream) (lc2 i-stream)))
  (format *c-file* "  ~A = mus_in_any(~A, ~A, ~A);~%"
	  (lc2 result) (lc-num-ref samp :integer) (lc-num-ref chan :integer) (lc2 i-stream))
  nil)

(defmacro <file2sample?> (res arg)
  (format *c-file* "  ~A = mus_is_file_to_sample(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <file2sample> (result i-stream samp chan)
  (format *c-file* "  ~A = mus_file_to_sample(~A, ~A, ~A);~%"
	  (lc2 result) (lc2 i-stream) (lc-num-ref samp :integer) (lc-num-ref chan :integer))
  nil)

(defmacro <mus-input?> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_is_input(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <file2frample> (i-stream samp outf)
  (format *c-file* "  mus_file_to_frample(~A, ~A, ~A);~%"
	  (lc2 i-stream) (lc-num-ref samp :integer) (lc2 outf))
  nil)

(defmethod gen-reflect ((gen file->sample) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-reflect ((gen file->frample) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen file->sample) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif ((CLM_FILE2SAMPLE_P(~A)) || (CLM_FILE2FRAMPLE_P(~A)))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_file_to_sample_with_buffer_size((char *)(clm_int + CLM_ILOC(~A) + 2 + 2), clm_int[CLM_ILOC(~A) + 1]));~%"
	  indent iloc iloc indent
	  result indent
	  iloc iloc))

(defmethod gen-make ((gen file->frample) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif ((CLM_FILE2FRAMPLE_P(~A)) || (CLM_FILE2SAMPLE_P(~A)))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_file_to_frample_with_buffer_size((char *)(clm_int + CLM_ILOC(~A) + 2 + 2), clm_int[CLM_ILOC(~A) + 1]));~%"
	  indent iloc iloc indent
	  result indent
	  iloc iloc))




;;; ---- READIN ----


(defmethod gen-load ((rd readin) i r datai datar)
  (setf (aref datai i) +readin+)
  (to-bignum (rdin-loc rd) datai (+ i 1))
  (setf (aref datai (+ i 3)) (rdin-dir rd))
  (setf (aref datai (+ i 4)) (rdin-chn rd))
  (setf (aref datai (+ i 5)) (rdin-size rd))
  (gen-load (mus-file-name rd) (+ i 6) r datai datar))

(defmethod gen-size ((rd readin))
  (list (+ 7 2 (ceiling (1+ (length (mus-file-name rd))) 4)) 0))

(defmethod gen-unload ((rd readin) iadr radr datai datar)
  (declare (ignore radr datar))
  (let* ((i (aref datai (+ iadr 1))))
    (setf (rdin-dir rd) (aref datai (+ i 3)))
    (setf (rdin-chn rd) (aref datai (+ i 2)))
    rd))

(defmacro <readin> (result rd)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_readin(~A))) {mus_error(0, \"readin ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 rd) (clean-arg rd) (lc2 rd)))
  (format *c-file* "  ~A = mus_readin(~A);~%" (lc2 result) (lc2 rd))
  nil)

(defmacro <readin?> (res arg)
  (format *c-file* "  ~A = mus_is_readin(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen readin) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_readin(~A))~%    ~
                          {~%     ~
                           ~A = mus_increment(~A);~%     ~
                           ~A = mus_channel(~A);~%    ~
                          }~%"
		name
		(ciadr iadr 3) name
		(ciadr iadr 2) name)))

(defmethod gen-make ((gen readin) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_READIN_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_readin_with_buffer_size(~%~A      ~
                           (char *)(clm_int + CLM_ILOC(~A) + 6 + 2), /* filename */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4], /* chan */~%~A      ~
                           clm_to_mus_long_t(clm_int, CLM_ILOC(~A) + 1), /* start */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3], /* direction */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 5]));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))



;;; ---- LOCSIG ----


(defmacro <locsig-ref> (result loc chan)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (format *c-file* "  ~A = mus_locsig_ref(~A,~A);~%" (lc2 result) (lc2 loc) (lc-num-ref chan :integer))
  nil)

(defmacro <locsig-reverb-ref> (result loc chan)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (format *c-file* "  ~A = mus_locsig_reverb_ref(~A, ~A);~%" (lc2 result) (lc2 loc) (lc-num-ref chan :integer))
  nil)

(defmacro <locsig-set!> (type loc chan val)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (if (= type +setf+)
      (format *c-file* "  mus_locsig_set(~A, ~A, ~A);~%"
	      (lc2 loc) (lc-num-ref chan :integer) (lc-num-ref val))
    (format *c-file* "  mus_locsig_set(~A, ~A, mus_locsig_ref(~A, ~A) ~A ~A);~%"
	      (lc2 loc) (lc-num-ref chan :integer)
	      (lc2 loc) (lc-num-ref chan :integer)
	      (if (= type +incf+) "+" "-")
	      (lc-num-ref val)))
  nil)

(defmacro <locsig-reverb-set!> (type loc chan val)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (if (= type +setf+)
      (format *c-file* "  mus_locsig_reverb_set(~A, ~A, ~A);~%"
	      (lc2 loc) (lc-num-ref chan :integer) (lc-num-ref val))
    (format *c-file* "  mus_locsig_reverb_set(~A, ~A, mus_locsig_reverb_ref(~A, ~A) ~A ~A);~%"
	      (lc2 loc) (lc-num-ref chan :integer)
	      (lc2 loc) (lc-num-ref chan :integer)
	      (if (= type +incf+) "+" "-")
	      (lc-num-ref val)))
  nil)

(defmacro <locsig?> (res arg)
  (format *c-file* "  ~A = mus_is_locsig(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <locsig> (result loc samp val)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (format *c-file* "  ~A = clm_locsig(~A, ~A, ~A);~%" (lc2 result) (lc2 loc) (lc-num-ref samp :integer) (lc-num-ref val))
  nil)

(defmacro <move-locsig> (loc degree distance)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_locsig(~A))) {mus_error(0, \"locsig ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (format *c-file* "  mus_move_locsig(~A, ~A, ~A);~%" (lc2 loc) (lc-num-ref degree) (lc-num-ref distance))
  nil)

(defmethod gen-load ((gen locsig) i r datai datar)
  (setf (aref datai i) +locsig+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (locs-chans gen))
  (setf (aref datai (+ i 3)) (locs-type gen))
  (setf (aref datar r) (double (locs-degree gen)))
  (setf (aref datar (+ r 1)) (double (locs-distance gen)))
  (setf (aref datar (+ r 2)) (double (locs-reverb gen))))

(defmethod gen-size ((gen locsig)) (list 4 3))

(defmethod gen-unload ((gen locsig) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmethod gen-reflect ((gen locsig) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen locsig) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_LOCSIG_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_locsig(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2],~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           clm_output(), (clm_reverb()) ? mus_channels(clm_reverb()) : 0, clm_reverb(),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3]));~%"
	  indent iloc indent
	  result indent
	  indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  indent
	  iloc
	  ))

;; reverb chans used to be set to 1 (22-Jun-10)



;;; ---- MOVE-SOUND ----


(defmacro <move-sound?> (res arg)
  (format *c-file* "  ~A = mus_is_move_sound(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <move-sound> (result loc samp val)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_move_sound(~A))) {mus_error(0, \"move-sound ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 loc) (clean-arg loc) (lc2 loc)))
  (format *c-file* "  ~A = mus_move_sound(~A, ~A, ~A);~%" (lc2 result) (lc2 loc) (lc-num-ref samp :integer) (lc-num-ref val))
  nil)

(defmethod gen-load ((gen move-sound) i r datai datar)
  (setf (aref datai i) +move-sound+)
  (setf (aref datai (+ i 1)) r)

  (let* ((dloc-list (dlocs-data gen))
	 (start (nth 0 dloc-list))
	 (end (nth 1 dloc-list))
	 (out-channels (nth 2 dloc-list))
	 (rev-channels (nth 3 dloc-list))
	 (doppler-delay (nth 4 dloc-list))
	 (doppler-env (nth 5 dloc-list))
	 (rev-env (nth 6 dloc-list))
	 (out-delays (nth 7 dloc-list))
	 (out-envs (nth 8 dloc-list))
	 (rev-envs (nth 9 dloc-list)) ; can be nil
	 (out-map (nth 10 dloc-list))
	 (ploc 0)
	 (iloc 0)
	 (rloc 0))
    ;; presumably *output* and *reverb* can be accessed in C as in locsig

    (setf (aref datai (+ i 2)) start)
    (setf (aref datai (+ i 3)) end)
    (setf (aref datai (+ i 4)) out-channels)
    (setf (aref datai (+ i 5)) rev-channels)

    (setf iloc (+ i 6))
    (setf rloc r)

    ;; here are 7 empty var (2wd) locations (pointers to locations set below, dependent on various sizes)
    (setf ploc iloc) ; base of pointer table
    (incf iloc 14)

    ;; doppler delay
    (gen-load doppler-delay iloc rloc datai datar)
    (setf (aref datai (+ ploc 0)) +delay+)
    (setf (aref datai (+ ploc 1)) iloc)
    (incf iloc (car (gen-size doppler-delay)))
    (incf rloc (cadr (gen-size doppler-delay)))

    ;; doppler env
    (gen-load doppler-env iloc rloc datai datar)
    (setf (aref datai (+ ploc 2)) +env+)
    (setf (aref datai (+ ploc 3)) iloc)
    (incf iloc (car (gen-size doppler-env)))
    (incf rloc (cadr (gen-size doppler-env)))

    ;; rev global env
    (gen-load rev-env iloc rloc datai datar)
    (setf (aref datai (+ ploc 4)) +env+)
    (setf (aref datai (+ ploc 5)) iloc)
    (incf iloc (car (gen-size rev-env)))
    (incf rloc (cadr (gen-size rev-env)))

    ;; now 3 clm arrays and 1 int array
    ;; out-delays (individual delays can be nil)
    (gen-load out-delays iloc rloc datai datar)
    (setf (aref datai (+ ploc 6)) +array+)
    (setf (aref datai (+ ploc 7)) iloc)
    (incf iloc (car (gen-size out-delays)))
    (incf rloc (cadr (gen-size out-delays)))

    ;; out-envs
    (gen-load out-envs iloc rloc datai datar)
    (setf (aref datai (+ ploc 8)) +array+)
    (setf (aref datai (+ ploc 9)) iloc)
    (incf iloc (car (gen-size out-envs)))
    (incf rloc (cadr (gen-size out-envs)))

    ;; rev-envs
    (if rev-envs
	(progn
	  (gen-load rev-envs iloc rloc datai datar)
	  (setf (aref datai (+ ploc 10)) +array+)
	  (setf (aref datai (+ ploc 11)) iloc)
	  (incf iloc (car (gen-size rev-envs)))
	  (incf rloc (cadr (gen-size rev-envs))))
      (progn
	(setf (aref datai (+ ploc 10)) +no-type+)
	(setf (aref datai (+ ploc 11)) 0)))

    ;; out-map
    (load-integer-array out-map iloc rloc datai datar (length out-map))
    (setf (aref datai (+ ploc 12)) +integer-array+)
    (setf (aref datai (+ ploc 13)) iloc)
    (incf iloc (+ 6 (length out-map)))
    ))

(defmethod gen-size ((gen move-sound))
  (let* ((data (dlocs-data gen))
	 (isize 20)
	 (rsize 0))
    (loop for i from 4 to 10 do
      (if (nth i data) ; revenvs can be nil
	  (let ((gsize (gen-size (nth i data))))
	    (incf isize (car gsize))
	    (incf rsize (cadr gsize)))))
    (list isize rsize)))

(defmethod gen-unload ((gen move-sound) iadr radr datai datar)
  (declare (ignore iadr radr datai datar))
  nil)

(defmethod gen-reflect ((gen move-sound) key val)
  (declare (ignore key val))
  nil)

(defun move-sound-array-make (gen stream name indent)
  (format stream "~Aif (CLM_ARRAY_P(iloc))~%    {~%" indent)
  (format stream "~A    ~A = (mus_any **)calloc(CLM_ARR_SIZE(CLM_VAR_ADDR(iloc)), sizeof(mus_any *));~%" indent name)
  (format stream "~A    { /* load ~A array */~%       ~
                            int i;~%~A       ~
                            for (i = 0; i < CLM_ARR_SIZE(CLM_VAR_ADDR(iloc)); i++)~%~A         ~
                              if (clm_int[CLM_ARR_IBLOCK(CLM_VAR_ADDR(iloc)) + (i * 2) + 1] != 0)~%~A            ~
                                {~%"
	 indent name indent indent indent)
  (gen-make gen
	    stream
	    (format nil "~A[i]" name)
	    (format nil "CLM_ARR_IBLOCK(CLM_VAR_ADDR(iloc)) + (i * 2)")
	    "              "
	    nil)
  (format stream "    }}}~%"))


(defmethod gen-make ((gen move-sound) stream result iloc indent ctr)
  (setf indent (concatenate 'string indent "  "))
  (format stream "  if (CLM_MOVE_SOUND_P(~A))~%    ~
                   {~%~A~
                    int iloc;~%~A~
                    mus_any *doppler_delay = NULL, *doppler_env = NULL, *rev_env = NULL;~%~A~
                    mus_any **out_delays = NULL, **out_envs = NULL, **rev_envs = NULL;~%~A~
                    int *out_map = NULL;~%"
	  iloc
	  indent indent indent indent)

  (format stream "~Ailoc = CLM_ILOC(~A) + 6;~%" indent iloc)
  (gen-make (make-instance 'common-tones/generators::delay) stream "doppler_delay" "iloc" indent ctr)

  (format stream "~Ailoc = CLM_ILOC(~A) + 8;~%" indent iloc)
  (gen-make (make-instance 'common-tones/generators::seg) stream "doppler_env" "iloc" indent ctr)

  (format stream "~Ailoc = CLM_ILOC(~A) + 10;~%" indent iloc)
  (gen-make (make-instance 'common-tones/generators::seg) stream "rev_env" "iloc" indent ctr)

  (format stream "~Ailoc = CLM_ILOC(~A) + 12;~%" indent iloc)
  (move-sound-array-make (make-instance 'common-tones/generators::delay) stream "out_delays" indent)

  (format stream "~Ailoc = CLM_ILOC(~A) + 14;~%" indent iloc)
  (move-sound-array-make (make-instance 'common-tones/generators::seg) stream "out_envs" indent)

  (format stream "~Ailoc = CLM_ILOC(~A) + 16;~%" indent iloc)
  (move-sound-array-make (make-instance 'common-tones/generators::seg) stream "rev_envs" indent)

  (format stream "~Ailoc = CLM_ILOC(~A) + 18;~%~A" indent iloc indent)
  (format stream "out_map = (int *)(clm_int + CLM_ARR_IBLOCK(CLM_ILOC(iloc)));~%~A" indent)

  (format stream "~%~A~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_move_sound(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],   /* start */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],   /* end */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4],   /* out-channels */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 5],   /* rev-channels */~%~A      ~
                           doppler_delay, doppler_env, rev_env, out_delays, out_envs, rev_envs, out_map,~%~A      ~
                           clm_output(), clm_reverb(), false, false));~%    ~
                         }~%"
	  indent
	  result indent
	  indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  indent
	  ))



;;; ---- WAVE-TRAIN ----


(defmethod gen-load ((w wave-train) i r datai datar)
  (setf (aref datai i) +wave-train+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (length (wt-wave w)))
  (setf (aref datai (+ i 3)) (wt-type w))
  (setf (aref datar r) (double (mus-frequency w)))
  (setf (aref datar (+ r 1)) (double (mus-phase w)))
  (load-real-array (wt-wave w) (+ i 4) (+ r 2) datai datar))

(defmethod gen-unload ((w wave-train) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (wt-freq w) (aref datar r))
    (setf (wt-phase w) (aref datar (+ r 1)))
    (setf (wt-wave w) (unload-real-array (+ i 4) datai datar))
    w))

(defmethod gen-size ((gen wave-train))
  (list (+ 4 6) (+ 2 (length (wt-wave gen)))))

(defmacro <wave-train> (result s &optional fm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_wave_train(~A))) {mus_error(0, \"wave-train ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if fm
      (format *c-file* "  ~A = mus_wave_train(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref fm))
    (format *c-file* "  ~A = mus_wave_train_unmodulated(~A);~%" (lc2 result) (lc2 s)))
  nil)

(defmacro <wave-train?> (res arg)
  (format *c-file* "  ~A = mus_is_wave_train(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen wave-train) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_wave_train(~A))~%    ~
                          {~%     ~
                           ~A = mus_frequency(~A);~%    ~
                           ~A = mus_phase(~A);~%    ~
                          }~%"
		name
		(cradr iadr 0) name
		(cradr iadr 1) name)))

(defmethod gen-make ((gen wave-train) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_WAVE_TRAIN_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_wave_train(~%~A      ~
                           clm_double[CLM_RLOC(~A)],~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2),~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* length */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3]));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))



;;; ---- FILTER ----


(defmethod gen-load ((s filter) i r datai datar)
  (let ((leny (length (flt-y s)))
	(lenx (length (flt-x s))))
    (setf (aref datai i) +filter+)
    (setf (aref datai (+ i 1)) r)
    (setf (aref datai (+ i 2)) (flt-order s))
    (load-real-array (flt-y s) (+ i 3) r datai datar)
    (load-real-array (flt-x s) (+ i 3 6) (+ r leny) datai datar) ;6=array-header-size of 1-dim array
    (load-real-array (flt-state s) (+ i 3 12) (+ r leny lenx) datai datar)))

(defmethod gen-size ((s filter))
  (list (+ 3 6 6 6) (+ (length (flt-y s)) (length (flt-x s)) (length (flt-state s)))))

(defmethod gen-unload ((s filter) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1))))
    (setf (flt-order s) (aref datai (+ i 2)))
    (setf (flt-y s) (unload-real-array (+ i 3) datai datar))
    (setf (flt-x s) (unload-real-array (+ i 3 6) datai datar))
    (setf (flt-state s) (unload-real-array (+ i 3 12) datai datar))
    s))

(defmacro <filter?> (res arg)
  (format *c-file* "  ~A = mus_is_filter(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <filter> (result s inval)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_filter(~A))) {mus_error(0, \"filter ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_filter(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval))
  nil)

(defmethod gen-reflect ((gen filter) key val)
  (declare (ignore key val))
  ;; nothing (user-visible) can change here except the array contents, and that's handled by gen-unload
  nil)

(defmethod gen-make ((gen filter) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_FILTER_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_filter(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + clm_int[CLM_ILOC(~A) + 2]),/* xcoeffs */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A)),/* ycoeffs */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + (2 * clm_int[CLM_ILOC(~A) + 2]))));/* state */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc iloc indent
	  iloc indent
	  iloc iloc))



(defmethod gen-load ((s fir-filter) i r datai datar)
  (let ((lenx (length (flt-x s))))
    (setf (aref datai i) +fir-filter+)
    (setf (aref datai (+ i 1)) r)
    (setf (aref datai (+ i 2)) (flt-order s))
    (load-real-array (flt-x s) (+ i 3) r datai datar)
    (load-real-array (flt-state s) (+ i 3 6) (+ r lenx) datai datar)))

(defmethod gen-size ((s fir-filter))
  (list (+ 3 6 6) (+ (length (flt-x s)) (length (flt-state s)))))

(defmethod gen-unload ((s fir-filter) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1))))
    (setf (flt-order s) (aref datai (+ i 2)))
    (setf (flt-x s) (unload-real-array (+ i 3) datai datar))
    (setf (flt-state s) (unload-real-array (+ i 3 6) datai datar))
    s))

(defmacro <fir-filter> (result s inval)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_fir_filter(~A))) {mus_error(0, \"fir-filter ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_fir_filter(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval))
  nil)

(defmacro <fir-filter?> (res arg)
  (format *c-file* "  ~A = mus_is_fir_filter(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen fir-filter) key val)
  (declare (ignore key val))
  ;; nothing (user-visible) can change here except the array contents, and that's handled by gen-unload
  nil)

(defmethod gen-make ((gen fir-filter) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_FIR_FILTER_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_fir_filter(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A)),/* xcoeffs */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + clm_int[CLM_ILOC(~A) + 2])));/* state */~%"

	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc iloc))



(defmethod gen-load ((s iir-filter) i r datai datar)
  (let ((leny (length (flt-y s))))
    (setf (aref datai i) +iir-filter+)
    (setf (aref datai (+ i 1)) r)
    (setf (aref datai (+ i 2)) (flt-order s))
    (load-real-array (flt-y s) (+ i 3) r datai datar)
    (load-real-array (flt-state s) (+ i 3 6) (+ r leny) datai datar)))

(defmethod gen-size ((s iir-filter))
  (list (+ 3 6 6) (+ (length (flt-y s)) (length (flt-state s)))))

(defmethod gen-unload ((s iir-filter) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1))))
    (setf (flt-order s) (aref datai (+ i 2)))
    (setf (flt-y s) (unload-real-array (+ i 3) datai datar))
    (setf (flt-state s) (unload-real-array (+ i 3 6) datai datar))
    s))

(defmacro <iir-filter> (result s inval)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_iir_filter(~A))) {mus_error(0, \"iir-filter ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_iir_filter(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval))
  nil)

(defmacro <iir-filter?> (res arg)
  (format *c-file* "  ~A = mus_is_iir_filter(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen iir-filter) key val)
  (declare (ignore key val))
  ;; nothing (user-visible) can change here except the array contents, and that's handled by gen-unload
  nil)

(defmethod gen-make ((gen iir-filter) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_IIR_FILTER_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_iir_filter(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A)),/* ycoeffs */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + clm_int[CLM_ILOC(~A) + 2])));/* state */~%"

	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc iloc))



;;; ---- DELAY ----


(defmethod gen-load ((d delay) i r datai datar)
  (setf (aref datai i) +delay+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (setf (aref datai (+ i 3)) (floor (dly-zsize d)))
  (setf (aref datai (+ i 4)) (dly-type d))
  (load-real-array (dly-line d) (+ i 5) r datai datar (dly-zsize d)))

(defmethod gen-size ((d delay)) (list (+ 5 6) (dly-zsize d)))

(defmethod gen-unload ((d delay) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1))))
    (setf (dly-line d) (unload-real-array (+ i 5) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <delay> (result s inval &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_delay(~A))) {mus_error(0, \"delay ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_delay(~A, ~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_delay_unmodulated(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval)))
  nil)

(defmacro <delay-tick> (result s inval)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_delay(~A))) {mus_error(0, \"delay ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_delay_tick(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval))
  nil)

(defmacro <tap> (result s &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_delay(~A))) {mus_error(0, \"delay ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_tap(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_tap_unmodulated(~A);~%" (lc2 result) (lc2 s)))
  nil)

(defmacro <delay?> (res arg)
  (format *c-file* "  ~A = mus_is_delay(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen delay) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen delay) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_DELAY_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_delay(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A)),/* delay line */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],/* max size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4]));/* interp type */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))


;;; ---- COMB ----


(defmethod gen-load ((d comb) i r datai datar)
  (setf (aref datai i) +comb+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (dly-xscl d)))
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (setf (aref datai (+ i 3)) (floor (dly-zsize d)))
  (setf (aref datai (+ i 4)) (dly-type d))
  (load-real-array (dly-line d) (+ i 5) (+ r 1) datai datar (dly-zsize d)))

(defmethod gen-size ((d comb)) (list (+ 5 6) (+ 1 (dly-zsize d))))

(defmethod gen-unload ((d comb) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (dly-xscl d) (aref datar r))
    (setf (dly-line d) (unload-real-array (+ i 5) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <comb> (result s inval &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_comb(~A))) {mus_error(0, \"comb ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_comb(~A, ~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_comb_unmodulated(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval)))
  nil)

(defmacro <comb?> (res arg)
  (format *c-file* "  ~A = mus_is_comb(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen comb) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_comb(~A))~%    ~
                          {~%     ~
                            ~A = mus_feedback(~A);~%    ~
                          }~%"
	    name
	    (cradr iadr 0) name)))

(defmethod gen-make ((gen comb) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_COMB_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_comb(~%~A      ~
                           clm_double[CLM_RLOC(~A)],/* feedback */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 1),/* delay line */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],/* max size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4]));/* interp type */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))


;;; ---- FILTERED-COMB ----


(defmethod gen-load ((d filtered-comb) i r datai datar)
  (setf (aref datai i) +filtered-comb+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (dly-xscl d)))
  (setf (aref datar (+ r 1)) (double (flt-a0 (dly-filter d))))
  (setf (aref datar (+ r 2)) (double (flt-a1 (dly-filter d))))
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (setf (aref datai (+ i 3)) (floor (dly-zsize d)))
  (setf (aref datai (+ i 4)) (dly-type d))
  (load-real-array (dly-line d) (+ i 5) (+ r 3) datai datar (dly-zsize d)))

(defmethod gen-size ((d filtered-comb)) (list (+ 5 6) (+ 3 (dly-zsize d))))

(defmethod gen-unload ((d filtered-comb) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (dly-xscl d) (aref datar r))
    (setf (dly-line d) (unload-real-array (+ i 5) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <filtered-comb> (result s inval &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_filtered_comb(~A))) {mus_error(0, \"filtered-comb ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_filtered_comb(~A, ~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_filtered_comb_unmodulated(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval)))
  nil)

(defmacro <filtered-comb?> (res arg)
  (format *c-file* "  ~A = mus_is_filtered_comb(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen filtered-comb) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_filtered_comb(~A))~%    ~
                          {~%     ~
                            ~A = mus_feedback(~A);~%    ~
                          }~%"
	    name
	    (cradr iadr 0) name)))

(defmethod gen-make ((gen filtered-comb) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_FILTERED_COMB_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_filtered_comb(~%~A      ~
                           clm_double[CLM_RLOC(~A)],/* feedback */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 3),/* delay line */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],/* max size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4],/* interp type */~A      ~
	                   mus_make_one_zero(clm_double[CLM_RLOC(~A) + 1], clm_double[CLM_RLOC(~A) + 2])));~%" ;; memory leak...
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc iloc
	  ))


;;; ---- NOTCH ----


(defmethod gen-load ((d notch) i r datai datar)
  (setf (aref datai i) +notch+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (dly-xscl d)))
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (setf (aref datai (+ i 3)) (floor (dly-zsize d)))
  (setf (aref datai (+ i 4)) (dly-type d))
  (load-real-array (dly-line d) (+ i 5) (+ r 1) datai datar (dly-zsize d)))

(defmethod gen-size ((d notch)) (list (+ 5 6) (+ 1 (dly-zsize d))))

(defmethod gen-unload ((d notch) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (dly-xscl d) (aref datar r))
    (setf (dly-line d) (unload-real-array (+ i 5) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <notch> (result s inval &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_notch(~A))) {mus_error(0, \"notch ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_notch(~A, ~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_notch_unmodulated(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval)))
  nil)

(defmacro <notch?> (res arg)
  (format *c-file* "  ~A = mus_is_notch(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen notch) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_notch(~A))~%    ~
                          {~%     ~
                            ~A = mus_feedforward(~A);~%    ~
                          }~%"
	    name
	    (cradr iadr 0) name)))

(defmethod gen-make ((gen notch) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_NOTCH_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_notch(~%~A      ~
                           clm_double[CLM_RLOC(~A)],/* feedforward */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 1),/* delay line */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],/* max size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4]));/* interp type */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))



;;; ---- ALL-PASS ----


(defmethod gen-load ((d all-pass) i r datai datar)
  (setf (aref datai i) +all-pass+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (dly-xscl d)))
  (setf (aref datar (+ r 1)) (double (dly-yscl d)))
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (setf (aref datai (+ i 3)) (floor (dly-zsize d)))
  (setf (aref datai (+ i 4)) (dly-type d))
  (load-real-array (dly-line d) (+ i 5) (+ r 2) datai datar (dly-zsize d)))

(defmethod gen-size ((d all-pass)) (list (+ 5 6) (+ 2 (dly-zsize d))))

(defmethod gen-unload ((d all-pass) iadr radr datai datar)
  (declare (ignore radr))
  (let* ((i (aref datai (+ iadr 1)))
	 (r (aref datai (+ i 1))))
    (setf (dly-xscl d) (aref datar r))
    (setf (dly-yscl d) (aref datar (+ r 1)))
    (setf (dly-line d) (unload-real-array (+ i 5) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <all-pass> (result s inval  &optional pm)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_all_pass(~A))) {mus_error(0, \"all-pass ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (if pm
      (format *c-file* "  ~A = mus_all_pass(~A, ~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval) (lc-num-ref pm))
    (format *c-file* "  ~A = mus_all_pass_unmodulated(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval)))
  nil)

(defmacro <all-pass?> (res arg)
  (format *c-file* "  ~A = mus_is_all_pass(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen all-pass) key val)
  (let ((name (lc key))
	(iadr (varinfo-iloc val)))
    (format *c-file* "  if (mus_is_all_pass(~A))~%    ~
                          {~%     ~
                            ~A = mus_feedforward(~A);~%     ~
                            ~A = mus_feedback(~A);~%    ~
                          }~%"
	    name
	    (cradr iadr 0) name
	    (cradr iadr 1) name)))

(defmethod gen-make ((gen all-pass) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_ALL_PASS_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_all_pass(~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1],/* feedback */~%~A      ~
                           clm_double[CLM_RLOC(~A)],/* feedforward */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2),/* delay line */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3],/* max size */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 4]));/* interp type */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc))


;;; ---- MOVING-AVERAGE ----


(defmethod gen-load ((d moving-average) i r datai datar)
  (setf (aref datai i) +moving-average+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (floor (dly-size d)))
  (load-real-array (dly-line d) (+ i 3) r datai datar (dly-zsize d)))

(defmethod gen-size ((d moving-average)) (list (+ 3 6) (dly-size d)))

(defmethod gen-unload ((d moving-average) iadr radr datai datar)
  (declare (ignore radr))
  (let ((i (aref datai (+ iadr 1))))
    (setf (dly-line d) (unload-real-array (+ i 3) datai datar))
    (setf (dly-size d) (length (dly-line d)))
    d))

(defmacro <moving-average> (result s inval)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_moving_average(~A))) {mus_error(0, \"moving-average ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_moving_average(~A, ~A);~%" (lc2 result) (lc2 s) (lc-num-ref inval))
  nil)

(defmacro <moving-average?> (res arg)
  (format *c-file* "  ~A = mus_is_moving_average(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmethod gen-reflect ((gen moving-average) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen moving-average) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_MOVING_AVERAGE_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_moving_average(~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2],/* size */~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 2)));~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc))



;;; ---- ENV ----


(defun load-envelope (arr i r datai datar)
  (let ((len (length arr)))
    (setf (aref datai (+ i +aref-type+)) +real-array+)
    (setf (aref datai (+ i +aref-rblock+)) r)
    (setf (aref datai (+ i +aref-size+)) len)
    (setf (aref datai (+ i +aref-dims+)) 1)
    (setf (aref datai (+ i +aref-dims+ 1)) len)
    (loop for k from 0 below len and x in arr do
      (setf (aref datar (+ r k)) (double x)))))

(defun unload-envelope (i r datai datar str)
  (let ((len (aref datai (+ i +aref-size+)))
	(rb (aref datai (+ i +aref-rblock+))))
    (if (/= r rb) (error "envelope ~A location is confused? ~D /= ~D" str r rb))
    (let ((new-arr nil))
      (loop for i from 0 below len do
	(push (aref datar (+ rb i)) new-arr))
      (nreverse new-arr))))

(defmethod gen-load ((e seg) i r datai datar)
  (setf (aref datai i) +env+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (/ (length (seg-original-data e)) 2)) ; npts
  (setf (aref datai (+ i 3)) (seg-end e)) ;dur-1
  (setf (aref datar r) (double (seg-base e)))
  (setf (aref datar (+ r 1)) (double (seg-original-scaler e)))
  (setf (aref datar (+ r 2)) (double (seg-original-offset e)))
  (load-envelope (seg-original-data e) (+ i 4) (+ r 3) datai datar))

(defmethod gen-size ((e seg)) (list (+ 4 6) (+ 3 (length (seg-original-data e)))))

(defmethod gen-unload ((e seg) iadr radr datai datar)
  (declare (ignore radr iadr datai datar))
  nil) ; nil here turns off the setf, so we should get the env unchanged

(defmacro <env> (result e)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_env(~A))) {mus_error(0, \"env ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 e) (clean-arg e) (lc2 e)))
  (format *c-file* "  ~A = mus_env(~A);~80,1T/* (env ~A) */~%"
	  (lc2 result) (lc2 e)
	  (clean-arg e))
  nil)

(defmacro <restart-env> (e)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_env(~A))) {mus_error(0, \"(restart-)env ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 e) (clean-arg e) (lc2 e)))
  (format *c-file* "  mus_reset(~A);~%" (lc2 e))
  nil)

(defmacro <env?> (res arg)
  (format *c-file* "  ~A = mus_is_env(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <env-interp> (result x e &optional (base 1.0))
  (declare (ignore base))
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_env(~A))) {mus_error(0, \"env(-interp) ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 e) (clean-arg e) (lc2 e)))
  (format *c-file* "  ~A = mus_env_interp(~A, ~A);~80,1T/* (env-interp ~A ~A) */~%"
	  (lc2 result) (lc-num-ref x) (lc2 e)
	  (clean-arg x)
	  (clean-arg e))
  nil)

(defmethod gen-reflect ((gen seg) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen seg) stream result iloc indent ctr)
  (declare (ignore ctr))
  (format stream "~Aif (CLM_ENV_P(~A))~%~A  ~
                      ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
                         mus_make_env(~%~A      ~
                           (double *)(clm_double + CLM_RLOC(~A) + 3), /* breakpoints */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 2], /* num points */~%~A      ~
                           clm_double[CLM_RLOC(~A) + 1], /* scaler */~%~A      ~
                           clm_double[CLM_RLOC(~A) + 2], /* offset */~%~A      ~
                           clm_double[CLM_RLOC(~A)], /* base */~%~A      ~
                           0.0, /* dur useless here */~%~A      ~
                           clm_int[CLM_ILOC(~A) + 3], /* end */~%~A      ~
                           NULL)); /* original data -- handled internally */~%"
	  indent iloc indent
	  result indent indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  iloc indent
	  indent
	  iloc indent))



;;; ---- AS-NEEDED INPUT FUNCTION ----


(defmacro <start-function> (ctr type)
  (if (= type +as-needed-input+)
      (format *c-file* "static mus_float_t as_needed_~D(void *gen, int direction)~%" ctr)
    (if (= type +as-needed-edit+)
	(format *c-file* "static int as_needed_~D(void *closure)~%" ctr)
      (if (= type +as-needed-analyze+)
	  (format *c-file* "static bool as_needed_~D(void *closure, mus_float_t (*input)(void *arg1, int direction))~%" ctr)
	(format *c-file* "static mus_float_t as_needed_~D(void *closure)~%" ctr))))
  (format *c-file* "{~%")
  nil)

(defmacro <end-function> (var ctr)
  (declare (ignore ctr))
  (format *c-file* "  return(~A);~%" (if (numberp var) var (lc var)))
  (format *c-file* "}~%")
  nil)

(defmacro <lambda-arg> (var ctr type)
  (declare (ignore ctr))
  (if (= type +as-needed-input+)
      (format *c-file* "  ~A = direction;~%" (lc2 var))
    (format *c-file* "  ~A = closure;~%" (lc2 var))) ; this is probably not usable in this context
  nil)



;;; ---- CONVOLVE ----


(defmethod gen-load ((c convolve) i r datai datar)
  (setf (aref datai i) +convolve+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (conv-hop c))
  (setf (aref datai (+ i 3)) (conv-fltlen c))
  (setf (aref datai (+ i 4)) (if (mus-input? (conv-rd c)) 1 0))
  (setf (aref datai (+ i 5)) (if (mus-input? (conv-rd c)) (mus-channel (conv-rd c)) 0))
  (to-bignum (if (mus-input? (conv-rd c)) (mus-location (conv-rd c)) 0) datai (+ i 6))
  (load-real-array (conv-filtr c) (+ i 8) r datai datar)
  (if (mus-input? (conv-rd c))
      (gen-load (mus-file-name (conv-rd c)) (+ i 8 6) r datai datar)))

(defmethod gen-size ((c convolve))
  (list (+ 8 6 (if (mus-input? (conv-rd c)) (+ 2 (ceiling (1+ (length (mus-file-name (conv-rd c)))) 4)) 0))
	(length (conv-filtr c))))

(defmethod gen-unload ((c convolve) iadr radr datai datar)
  (declare (ignore radr iadr datai datar))
  nil)

(defmacro <convolve?> (res arg)
  (format *c-file* "  ~A = mus_is_convolve(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <convolve> (result s &optional func)
  (declare (ignore func))
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_convolve(~A))) {mus_error(0, \"convolve ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_convolve(~A, NULL);~%" (lc2 result) (lc2 s))
  nil)

(defmethod gen-reflect ((gen convolve) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen convolve) stream result iloc indent ctr)
  (if (not ctr)
      (progn
	(format stream "~Aif (CLM_CONVOLVE_P(~A))~%~A  ~
                             {~%~A    ~
                              if (clm_int[CLM_ILOC(~A) + 4])~%~A      ~
                                {~%~A        ~
                                 mus_any *rd = NULL;~%~A"
		indent iloc indent
		indent
		iloc indent
		indent
		indent)
	(format stream "        rd = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                  mus_make_readin(~%~A            ~
                                    (char *)(clm_int + CLM_ILOC(~A) + 8 + 6 + 2), /* filename */~%~A              ~
                                    clm_int[CLM_ILOC(~A) + 5], /* chan */~%~A              ~
                                    clm_to_mus_long_t(clm_int, CLM_ILOC(~A) + 6), /* start */~%~A              ~
                                    1)); /* direction */~%~A"
		indent
		indent
		iloc indent
		iloc indent
		iloc indent
		indent)
	(format stream "        ~A = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                    mus_make_convolve(clm_as_needed_input,~%~A            ~
                                      (double *)(clm_double + CLM_RLOC(~A)),~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 2],~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 3],~%~A            ~
                                      (void *)rd));~%~A       ~
                               }~%~A  }~%"
		result indent
		indent
		iloc indent
		iloc indent
		iloc indent
		indent
		indent))
    (progn
      (format stream "~Aif (CLM_CONVOLVE_P(~A))~%~A  ~
                         ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
			        mus_make_convolve(as_needed_~D,~%~A      ~
                                  (double *)(clm_double + CLM_RLOC(~A)),~%~A      ~
                                  clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                                  clm_int[CLM_ILOC(~A) + 3],~%~A      ~
                                  NULL));~%"
	      indent iloc indent
	      result indent
	      (cadr ctr) indent
	      iloc indent
	      iloc indent
	      iloc indent))))


;;; ---- SRC ----


(defmethod gen-load ((s src) i r datai datar)
  (setf (aref datai i) +src+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datai (+ i 2)) (sr-width s)) ; can be *clm-src-width*
  (setf (aref datar r) (double (sr-incr s))) ; srate
  (setf (aref datai (+ i 3)) (if (mus-input? (sr-rd s)) 1 0))
  (setf (aref datai (+ i 4)) (if (mus-input? (sr-rd s)) (mus-channel (sr-rd s)) 0))
  (to-bignum (if (mus-input? (sr-rd s)) (mus-location (sr-rd s)) 0) datai (+ i 5))
  (if (mus-input? (sr-rd s))
      (gen-load (mus-file-name (sr-rd s)) (+ i 7) r datai datar)))

(defmethod gen-size ((s src))
  (list (+ 7 (if (mus-input? (sr-rd s)) (+ 2 (ceiling (1+ (length (mus-file-name (sr-rd s)))) 4)) 0))
	1))

(defmethod gen-unload ((s src) iadr radr datai datar)
  (declare (ignore radr iadr datai datar))
  nil)

(defmacro <src?> (res arg)
  (format *c-file* "  ~A = mus_is_src(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <src> (result s &optional fm func)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_src(~A))) {mus_error(0, \"src ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (let* ((info (gethash (second s) vars))
	 (ctr (and info (varinfo-ctr info) (cadr (varinfo-ctr info)))))
    (if fm
	(format *c-file* "  ~A = mus_src(~A, ~A, ~A);~%"
		(lc2 result) (lc2 s) (lc-num-ref fm)
		(if func (format nil "as_needed_~A" ctr) "NULL"))
      (format *c-file* "  ~A = mus_src(~A, 0.0, ~A);~%"
	      (lc2 result) (lc2 s)
	      (if func (format nil "as_needed_~A" ctr) "NULL"))))
  nil)

(defmethod gen-reflect ((gen src) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen src) stream result iloc indent ctr)
  (if (not ctr)
      (progn
	(if *clm-debug* (format t "output to ~A, no ctr~%" stream))
	(format stream "~Aif (CLM_SRC_P(~A))~%~A  ~
                             {~%~A    ~
                              if (clm_int[CLM_ILOC(~A) + 3])~%~A      ~
                                {~%~A        ~
                                 mus_any *rd = NULL;~%~A"
		indent iloc indent
		indent
		iloc indent
		indent
		indent)
	(format stream "        rd = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                  mus_make_readin(~%~A            ~
                                    (char *)(clm_int + CLM_ILOC(~A) + 7 + 2), /* filename */~%~A              ~
                                    clm_int[CLM_ILOC(~A) + 4], /* chan */~%~A              ~
                                    clm_to_mus_long_t(clm_int, CLM_ILOC(~A) + 5), /* start */~%~A              ~
                                    1)); /* direction */~%~A"
		indent
		indent
		iloc indent
		iloc indent
		iloc indent
		indent)
	(format stream "        ~A = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                    clm_make_src(clm_as_needed_input,~%~A            ~
                                      clm_double[CLM_RLOC(~A)], /* srate */~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 2], /* width */~%~A            ~
                                      (void *)rd));~%~A       ~
                               }~%~A  }~%"
		;; rd as last arg -> closure passed as 1st arg to input func
		result indent
		indent
		iloc indent
		iloc indent
		indent
		indent))
    (progn
      (format stream "~Aif (CLM_SRC_P(~A))~%~A  ~
                         ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
			        clm_make_src(NULL,~%~A      ~
                                  clm_double[CLM_RLOC(~A)],~%~A      ~
                                  clm_int[CLM_ILOC(~A) + 2],~%~A      ~
                                  NULL));~%"
	      indent iloc indent
	      result indent
	      indent
	      iloc indent
	      iloc indent))))



;;; ---- GRANULATE ----


;;; I think it would work to pass the edit and input funcs at run-time via mus_granulate_with_editor


(defmethod gen-load ((s granulate) i r datai datar)
  (setf (aref datai i) +granulate+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (spd-expansion s)))
  (setf (aref datar (+ r 1)) (double (spd-length s)))
  (setf (aref datar (+ r 2)) (double (spd-amp s)))
  (setf (aref datar (+ r 3)) (double (spd-hop s)))
  (setf (aref datar (+ r 4)) (double (spd-ramp s)))
  (setf (aref datar (+ r 5)) (double (spd-jitter s)))
  (setf (aref datai (+ i 2)) (or (spd-max-size s) 0))
  (setf (aref datai (+ i 3)) (if (mus-input? (spd-rd s)) 1 0))
  (setf (aref datai (+ i 4)) (if (mus-input? (spd-rd s)) (mus-channel (spd-rd s)) 0))
  (to-bignum (if (mus-input? (spd-rd s)) (mus-location (spd-rd s)) 0) datai (+ i 5))
  (if (mus-input? (spd-rd s))
      (gen-load (mus-file-name (spd-rd s)) (+ i 7) r datai datar)))

(defmethod gen-size ((s granulate))
  (list (+ 7 (if (mus-input? (spd-rd s)) (+ 2 (ceiling (1+ (length (mus-file-name (spd-rd s)))) 4)) 0))
	6))

(defmethod gen-unload ((s granulate) iadr radr datai datar)
  (declare (ignore radr iadr datai datar))
  nil)

(defmacro <granulate?> (res arg)
  (format *c-file* "  ~A = mus_is_granulate(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <granulate> (result s &optional input-func edit-func)
  (declare (ignore input-func edit-func))
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_granulate(~A))) {mus_error(0, \"granulate ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_granulate(~A, NULL);~%" (lc2 result) (lc2 s))
  nil)

(defmethod gen-reflect ((gen granulate) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen granulate) stream result iloc indent ctr)
    (let ((edit-ctr (if (eq (car (car ctr)) :edit)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :edit))
			  (cadr (cadr ctr)))))
	  (input-ctr (if (eq (car (car ctr)) :input)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :input))
			  (cadr (cadr ctr))))))
  (if (not input-ctr)
      (progn
	(format stream "~Aif (CLM_GRANULATE_P(~A))~%~A  ~
                             {~%~A    ~
                              if (clm_int[CLM_ILOC(~A) + 3])~%~A      ~
                                {~%~A        ~
                                 mus_any *rd = NULL;~%~A"
		indent iloc indent
		indent
		iloc indent
		indent
		indent)
	(format stream "        rd = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                  mus_make_readin(~%~A            ~
                                    (char *)(clm_int + CLM_ILOC(~A) + 7 + 2), /* filename */~%~A              ~
                                    clm_int[CLM_ILOC(~A) + 4], /* chan */~%~A              ~
                                    clm_to_mus_long_t(clm_int, CLM_ILOC(~A) + 5), /* start */~%~A              ~
                                    1)); /* direction */~%~A"
		indent
		indent
		iloc indent
		iloc indent
		iloc indent
		indent)
	(format stream "        ~A = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                    mus_make_granulate(clm_as_needed_input,~%~A            ~
                                      clm_double[CLM_RLOC(~A)], /* expansion */~%~A            ~
                                      clm_double[CLM_RLOC(~A) + 1], /* length */~%~A            ~
                                      clm_double[CLM_RLOC(~A) + 2], /* scaler */~%~A            ~
                                      clm_double[CLM_RLOC(~A) + 3], /* hop */~%~A            ~
                                      clm_double[CLM_RLOC(~A) + 4], /* ramp */~%~A            ~
                                      clm_double[CLM_RLOC(~A) + 5], /* jitter */~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 2], /* max-size */~%~A            ~
                                      ~A, /* edit func */~%~A            ~
                                      (void *)rd));~%~A       ~
                               }~%~A  }~%"
		result indent
		indent
		iloc indent
		iloc indent
		iloc indent
		iloc indent
		iloc indent
		iloc indent
		iloc indent
		(if edit-ctr (format nil "as_needed_~D" edit-ctr) "NULL") indent
		indent
		indent))
    (progn
      (format stream "~Aif (CLM_GRANULATE_P(~A))~%~A  ~
                         ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
			        mus_make_granulate(as_needed_~D,~%~A      ~
                                clm_double[CLM_RLOC(~A)], /* expansion */~%~A            ~
                                clm_double[CLM_RLOC(~A) + 1], /* length */~%~A            ~
                                clm_double[CLM_RLOC(~A) + 2], /* scaler */~%~A            ~
                                clm_double[CLM_RLOC(~A) + 3], /* hop */~%~A            ~
                                clm_double[CLM_RLOC(~A) + 4], /* ramp */~%~A            ~
                                clm_double[CLM_RLOC(~A) + 5], /* jitter */~%~A            ~
                                clm_int[CLM_ILOC(~A) + 2], /* max-size */~%~A            ~
                                ~A, /* edit func */~%~A            ~
                                NULL));~%"
	      indent iloc indent
	      result indent
	      input-ctr indent
	      iloc indent
	      iloc indent
	      iloc indent
	      iloc indent
	      iloc indent
	      iloc indent
	      iloc indent
	      (if edit-ctr (format nil "as_needed_~D" edit-ctr) "NULL") indent
	      )))))



;;; -------- PHASE-VOCODER


(defmethod gen-load ((s phase-vocoder) i r datai datar)
  (setf (aref datai i) +phase-vocoder+)
  (setf (aref datai (+ i 1)) r)
  (setf (aref datar r) (double (pv-pitch s)))
  (setf (aref datai (+ i 2)) (or (pv-N s) 0)) ;fftsize
  (setf (aref datai (+ i 3)) (pv-interp s))
  (setf (aref datai (+ i 4)) (or (pv-overlap s) 0))
  (setf (aref datai (+ i 5)) (if (mus-input? (pv-input s)) 1 0))
  (setf (aref datai (+ i 6)) (if (mus-input? (pv-input s)) (mus-channel (pv-input s)) 0))
  (to-bignum (if (mus-input? (pv-input s)) (mus-location (pv-input s)) 0) datai (+ i 7))
  (if (mus-input? (pv-input s))
      (gen-load (mus-file-name (pv-input s)) (+ i 9) r datai datar)))

(defmethod gen-size ((s phase-vocoder))
  (list (+ 9 (if (mus-input? (pv-input s)) (+ 2 (ceiling (1+ (length (mus-file-name (pv-input s)))) 4)) 0))
	1))

(defmethod gen-unload ((s phase-vocoder) iadr radr datai datar)
  (declare (ignore radr iadr datai datar))
  nil)

(defmacro <phase-vocoder?> (res arg)
  (format *c-file* "  ~A = mus_is_phase_vocoder(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <phase-vocoder> (result s &optional func1 func2 func3 func4)
  (declare (ignore func1 func2 func3 func4))
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 s) (clean-arg s) (lc2 s)))
  (format *c-file* "  ~A = mus_phase_vocoder(~A, NULL);~%" (lc2 result) (lc2 s))
  nil)

(defmethod gen-reflect ((gen phase-vocoder) key val)
  (declare (ignore key val))
  nil)

(defmethod gen-make ((gen phase-vocoder) stream result iloc indent ctr)
    (let ((edit-ctr (if (eq (car (car ctr)) :edit)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :edit))
			  (cadr (cadr ctr)))))
	  (input-ctr (if (eq (car (car ctr)) :input)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :input))
			  (cadr (cadr ctr)))))
	  (analyze-ctr (if (eq (car (car ctr)) :analyze)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :analyze))
			  (cadr (cadr ctr)))))
	  (synthesize-ctr (if (eq (car (car ctr)) :synthesize)
			(cadr (car ctr))
		      (if (and (cdr ctr)
			       (eq (car (cadr ctr)) :synthesize))
			  (cadr (cadr ctr))))))

  (if (not input-ctr)
      (progn
	(format stream "~Aif (CLM_PHASE_VOCODER_P(~A))~%~A  ~
                             {~%~A    ~
                              if (clm_int[CLM_ILOC(~A) + 5])~%~A      ~
                                {~%~A        ~
                                 mus_any *rd = NULL;~%~A"
		indent iloc indent
		indent
		iloc indent
		indent
		indent)
	(format stream "        rd = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                  mus_make_readin(~%~A            ~
                                    (char *)(clm_int + CLM_ILOC(~A) + 9 + 2), /* filename */~%~A              ~
                                    clm_int[CLM_ILOC(~A) + 6], /* chan */~%~A              ~
                                    clm_to_mus_long_t(clm_int, CLM_ILOC(~A) + 7), /* start */~%~A              ~
                                    1)); /* direction */~%~A"
		indent
		indent
		iloc indent
		iloc indent
		iloc indent
		indent)
	(format stream "        ~A = clm_add_gen_to_genbag(all_gens, ~%~A          ~
                                    mus_make_phase_vocoder(clm_as_needed_input,~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 2], /* fftsize */~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 4], /* overlap */~%~A            ~
                                      clm_int[CLM_ILOC(~A) + 3], /* interp */~%~A            ~
                                      clm_double[CLM_RLOC(~A)], /* pitch */~%~A            ~
                                      ~A, /* analyze func */~%~A            ~
                                      ~A, /* edit func */~%~A            ~
                                      ~A, /* synthesize func */~%~A            ~
                                      (void *)rd));~%~A       ~
                               }~%~A  }~%"
		result indent
		indent
		iloc indent
		iloc indent
		iloc indent
		iloc indent
		(if analyze-ctr (format nil "as_needed_~D" analyze-ctr) "NULL") indent
		(if edit-ctr (format nil "as_needed_~D" edit-ctr) "NULL") indent
		(if synthesize-ctr (format nil "as_needed_~D" synthesize-ctr) "NULL") indent
		indent
		indent))
    (progn
      (format stream "~Aif (CLM_PHASE_VOCODER_P(~A))~%~A  ~
                         ~A = clm_add_gen_to_genbag(all_gens, ~%~A    ~
			        mus_make_phase_vocoder(as_needed_~D,~%~A      ~
                                clm_int[CLM_ILOC(~A) + 2], /* fftsize */~%~A            ~
                                clm_int[CLM_ILOC(~A) + 4], /* overlap */~%~A            ~
                                clm_int[CLM_ILOC(~A) + 3], /* interp */~%~A            ~
                                clm_double[CLM_RLOC(~A)], /* pitch */~%~A            ~
                                ~A, /* analyze func */~%~A            ~
                                ~A, /* edit func */~%~A            ~
                                ~A, /* synthesize func */~%~A            ~
                                NULL));~%"
	      indent iloc indent
	      result indent
	      input-ctr indent
	      iloc indent
	      iloc indent
	      iloc indent
	      iloc indent
	      (if analyze-ctr (format nil "as_needed_~D" analyze-ctr) "NULL") indent
	      (if edit-ctr (format nil "as_needed_~D" edit-ctr) "NULL") indent
	      (if synthesize-ctr (format nil "as_needed_~D" synthesize-ctr) "NULL") indent
	      )))))


(defmacro <phase-vocoder-amps> (res gen)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 gen) (clean-arg gen) (lc2 gen)))
  (format *c-file* "  ~A = mus_phase_vocoder_amps(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <phase-vocoder-amp-increments> (res gen)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 gen) (clean-arg gen) (lc2 gen)))
  (format *c-file* "  ~A = mus_phase_vocoder_amp_increments(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <phase-vocoder-freqs> (res gen)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 gen) (clean-arg gen) (lc2 gen)))
  (format *c-file* "  ~A = mus_phase_vocoder_freqs(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <phase-vocoder-phases> (res gen)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 gen) (clean-arg gen) (lc2 gen)))
  (format *c-file* "  ~A = mus_is_phase_vocoderhases(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <phase-vocoder-phase-increments> (res gen)
  (when (> *safety* 0)
    (format *c-file* "  if (!(mus_is_phase_vocoder(~A))) {mus_error(0, \"phase-vocoder ~(~A~) is %s\", mus_describe(~A)); goto RUN_ALL_DONE;}~%"
	    (lc2 gen) (clean-arg gen) (lc2 gen)))
  (format *c-file* "  ~A = mus_is_phase_vocoderhase_increments(~A);~%" (lc2 res) (lc2 gen))
  nil)



;;; -------- DEF-CLM-STRUCT


(defmacro def-clm-struct (name &rest fields)
  `(eval-when #-(or clozure excl) (:compile-toplevel :load-toplevel)
	      #+(or clozure excl) (compile load eval)
     (progn
       (defstruct (,name (:type vector) :named)
	 ,@(loop for fld in fields collect
	     (if (listp fld)
		 (if (not (symbolp (second fld)))
		     fld
		   (if (not (member (second fld) (list 'integer 'float 'double-float 'real 'short-float 'single-float 'rational 'number 'bignum 'fixnum)))
		       (error "~A is not a type def-clm-struct can handle" (second fld))
		     (first fld)))
	       fld)))
       ,@(loop for field in fields and i from 1 collect
	   (let ((fieldname (concatenate 'string
					 (symbol-name name)
					 "-"
					 (symbol-name (if (listp field) (first field) field)))))
	   `(progn (common-tones::def-clm-fun (intern ,fieldname)
		     #'(lambda (var x) (package-op 'common-tones::<aref> var (list 'aref (cadr x) ,i))))
		   (push (list (intern ,fieldname) 'common-tones::<setf-aref> (list ,i)) common-tones::setf-functions)))))))


(defmacro def-clm-float-struct (name &rest fields)
  `(eval-when #-(or clozure excl) (:compile-toplevel :load-toplevel)
	      #+(or clozure excl) (compile load eval)
     (progn
       (defstruct (,name (:type (vector double-float)))
	 ,@(loop for fld in fields collect
	     (if (listp fld)
		 (if (not (symbolp (second fld)))
		     fld
		   (if (not (member (second fld) (list 'float 'double-float 'real 'short-float 'single-float 'number)))
		       (error "~A is not a type def-clm-float-struct can handle" (second fld))
		     (first fld)))
	       fld)))
       ,@(loop for field in fields and i from 0 collect
	   (let ((fieldname (concatenate 'string
					 (symbol-name name)
					 "-"
					 (symbol-name (if (listp field) (first field) field)))))
	   `(progn (common-tones::def-clm-fun (intern ,fieldname)
		     #'(lambda (var x) (package-op 'common-tones::<double-aref> var (list 'aref (cadr x) ,i) :clm-real)))
		   (push (list (intern ,fieldname) 'common-tones::<setf-double-aref> (list ,i) :clm-real) common-tones::setf-functions)))))))


;;; ---------------- RUN*

;;; new form courtesy Larry Troxler


(defvar *with-reflection* nil)

(defmacro run* (vars body)
  (setf *with-reflection* t)
  `(progn
     (run ,body)
     (let ((insvars (get *clm-ins* :ins-vars)))
       ,@(loop for v in vars collect
	   `(let ((var (find (symbol-name ',v) insvars :test #'string-equal :key #'first)))
	      (when var
		(if (and (fourth var)
			 (member (fourth var) (list :integer :real :boolean)))
		    (if (eq (fourth var) :integer)
			(setf ,v (aref (clm-datai) (second var)))
		      (if (eq (fourth var) :real)
			  (setf ,v (aref (clm-datar) (third var)))
			(setf ,v (not (= (aref (clm-datai) (second var)) 0)))))
		  (let ((val (gen-unload (run-type->class (aref (clm-datai) (second var)))
					 (second var) (third var)
					 (clm-datai) (clm-datar))))
		    (when val (setf ,v val))))))))))


;;; This is the story, a sad tale but true

;;; Of a programmer who had far too little to do.

;;; One day as he sat in his hut swilling stew,

;;; He cried "CLM takes forever, it's stuck in a slough!,

;;; Its C code is slow, too slow by a few.

;;; Why, with just a small effort, say one line or two,

;;; It could outpace a no-op, you could scarcely say 'boo'"!

;;; So he sat in his kitchen and worked like a dog.

;;; He typed and he typed 'til his mind was a fog.

;;; Now 6000 lines later, what wonders we see!

;;; CLM is much faster, and faster still it will be!

;;; In fact, for most cases, C beats the DSP!

;;; But bummed is our coder; he grumbles at night.

;;; That DSP code took him a year to write.

;;; He was paid many dollars, and spent them with glee,

;;; But his employer might mutter, this result were he to see.




;;; Generic Functions


(defun generator? (gen)
  (let ((var (gethash (second gen) vars)))
    (and var
	 (varinfo-gen-type var))))

(defmacro <mus-frequency> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_frequency(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-frequency> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_frequency(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_frequency(~A, mus_frequency(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-phase> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_phase(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-phase> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_phase(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_phase(~A, mus_phase(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-scaler> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_scaler(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-scaler> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_scaler(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_scaler(~A, mus_scaler(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-offset> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_offset(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-reset> (res gen)
  (declare (ignore res))
  (if (generator? gen)
      (format *c-file* "  mus_reset(~A);~%" (lc2 gen)))
  nil)

(defmacro <mus-increment> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_increment(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-increment> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_increment(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_increment(~A, mus_increment(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-length> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_length(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-length> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_length(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_length(~A, mus_length(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-ramp> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_ramp(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-ramp> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_ramp(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_ramp(~A, mus_ramp(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-interp-type> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_interp_type(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-a0> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_xcoeff(~A, 0);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-a0> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_xcoeff(~A, 0, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_xcoeff(~A, 0, mus_xcoeff(~A, 0) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-a1> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_xcoeff(~A, 1);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-a1> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_xcoeff(~A, 1, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_xcoeff(~A, 1, mus_xcoeff(~A, 1) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-a2> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_xcoeff(~A, 2);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-a2> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_xcoeff(~A, 2, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_xcoeff(~A, 2, mus_xcoeff(~A, 2) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-b1> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_ycoeff(~A, 1);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-b1> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_ycoeff(~A, 1, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_ycoeff(~A, 1, mus_ycoeff(~A, 1) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-b2> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_ycoeff(~A, 2);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-b2> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_ycoeff(~A, 2, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_ycoeff(~A, 2, mus_xcoeff(~A, 2) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-set-xcoeff> (type gen index val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_xcoeff(~A, ~A, ~A);~%"
		  (lc2 gen) (lc-num-ref index :integer) (lc-num-ref val :real))
	(format *c-file* "  mus_set_xcoeff(~A, ~A, mus_xcoeff(~A, ~A) ~A ~A);~%"
		(lc2 gen) (lc-num-ref index :integer)
		(lc2 gen) (lc-num-ref index :integer)
		(if (= type +incf+) "+" "-")
		(lc-num-ref val :real))))
  nil)

(defmacro <mus-set-ycoeff> (type gen index val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_ycoeff(~A, ~A, ~A);~%"
		  (lc2 gen) (lc-num-ref index :integer) (lc-num-ref val :real))
	(format *c-file* "  mus_set_ycoeff(~A, ~A, mus_ycoeff(~A, ~A) ~A ~A);~%"
		(lc2 gen) (lc-num-ref index :integer)
		(lc2 gen) (lc-num-ref index :integer)
		(if (= type +incf+) "+" "-")
		(lc-num-ref val :real))))
  nil)

(defmacro <mus-location> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_location(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-location> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_location(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_location(~A, mus_location(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-channel> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_channel(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-channels> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_channels(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 1;~%" (lc2 res)))
  nil)

(defmacro <mus-order> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_order(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-width> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_width(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-width> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_width(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_width(~A, mus_width(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-hop> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_hop(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-hop> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_hop(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_hop(~A, mus_hop(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-equal> (res gen1 gen2)
  (format *c-file* "  ~A = mus_equal(~A, ~A);~%" (lc2 res) (lc2 gen1) (lc2 gen2))
  nil)


(defmacro <mus-describe> (res gen)
  (format *c-file* "  if (~A) free(~A);~%" (lc2 res) (lc2 res))
  (format *c-file* "  ~A = strdup(mus_describe(~A));~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <mus-name> (res gen)
  (format *c-file* "  ~A = (char *)mus_name(~A);~%" (lc2 res) (lc2 gen))
  nil)

#|
;; removed 24-Feb-15
(defmacro <mus-set-name> (type gen val)
  (declare (ignore type))
  (if (listp val)
      (format *c-file* "  mus_set_name(~A, ~A);~%" (lc2 gen) (lc2 val))
      (format *c-file* "  mus_set_name(~A, ~S);~%" (lc2 gen) val))
  nil)
|#

(defmacro <mus-file-name> (res gen)
  (format *c-file* "  ~A = mus_file_name(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <mus-header-type-name> (res arg)
  (format *c-file* "  ~A = (char *)mus_header_type_name(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <mus-data-format-name> (res arg)
  (format *c-file* "  ~A = (char *)mus_sample_type_name(~A);~%" (lc2 res) (lc2 arg))
  nil)

(defmacro <string=> (res arg1 arg2)
  (format *c-file* " ~A = (strcmp(~A, ~A) == 0);~%" (lc2 res) (lc-str-ref arg1) (lc-str-ref arg2))
  nil)

(defmacro <string-equal> (res arg1 arg2)
  (format *c-file* " ~A = (strcasecmp(~A, ~A) == 0);~%" (lc2 res) (lc-str-ref arg1) (lc-str-ref arg2))
  nil)


(defmacro <mus-data> (res gen)
  (format *c-file* "  ~A = mus_data(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <mus-xcoeffs> (res gen)
  (format *c-file* "  ~A = mus_xcoeffs(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <mus-ycoeffs> (res gen)
  (format *c-file* "  ~A = mus_ycoeffs(~A);~%" (lc2 res) (lc2 gen))
  nil)

(defmacro <mus-xcoeff> (res gen index)
  (format *c-file* "  ~A = mus_xcoeff(~A, ~A);~%" (lc2 res) (lc2 gen) (lc-num-ref index :integer))
  nil)

(defmacro <mus-ycoeff> (res gen index)
  (format *c-file* "  ~A = mus_ycoeff(~A, ~A);~%" (lc2 res) (lc2 gen) (lc-num-ref index :integer))
  nil)

(defmacro <mus-feedforward> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_feedforward(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-feedforward> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_feedforward(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_feedforward(~A, mus_feedforward(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)

(defmacro <mus-feedback> (res gen)
  (if (generator? gen)
      (format *c-file* "  ~A = mus_feedback(~A);~%" (lc2 res) (lc2 gen))
    (format *c-file* "  ~A = 0.0;~%" (lc2 res)))
  nil)

(defmacro <mus-set-feedback> (type gen val)
  (if (generator? gen)
      (if (= type +setf+)
	  (format *c-file* "  mus_set_feedback(~A, ~A);~%" (lc2 gen) (lc-num-ref val))
	(format *c-file* "  mus_set_feedback(~A, mus_feedback(~A) ~A ~A);~%"
		(lc2 gen) (lc2 gen) (if (= type +incf+) "+" "-") (lc-num-ref val))))
  nil)


(defvar *c-proc* nil)			;these are passed in from definstrument
(defvar *lisp-proc* nil)

(defvar dat-r 0)
(defvar dat-i 0)

(defun method-type (method)
  (if (member method '(mus-length mus-channel mus-location mus-channels mus-order))
      'int
    (if (member method '(mus-data mus-xcoeffs mus-ycoeffs
		         phase-vocoder-amps phase-vocoder-amp-increments phase-vocoder-phases phase-vocoder-phase-increments phase-vocoder-freqs))
	'float-array
      'double)))

(defun unchain-variable (lst)
  (when (and (not (varinfo-refd lst))
	     (varinfo-ref-chain lst))
    (loop for var in (varinfo-ref-chain lst) do
      (if (varinfo-ref-chain var)
	  (unchain-variable var))
      (if (varinfo-refd var)
	  (setf (varinfo-refd lst) t)))))

(defvar variable-load-list nil)

(defmacro <start> (end)
  (setf dat-r +float-block-size+)
  (setf dat-i +int-block-size+)
  (clrhash c-names)        ;used to avoid collisions in the lisp->c name translation
  (clrhash variable-name)         ;save translated names
  (setf variable-load-list nil)   ;hash->list for saving in fasl file

  (maphash #'(lambda (key lst)
	       (unchain-variable lst)
	       (when (and (null (varinfo-type lst))
			  (varinfo-refd lst))
		 (setf (varinfo-iloc lst) dat-i)
		 (setf (varinfo-rloc lst) dat-r)
		 (if (not (varinfo-temp lst)) (push (list key dat-i dat-r nil) variable-load-list))
		 (incf dat-i (* 2 (varinfo-max-depth lst)))
		 (incf dat-r (varinfo-max-depth lst))))
	   vars)

  (let* ((ints nil)
	 (bools nil)
	 (floats nil)
	 (float-arrays nil)
	 (double-arrays nil)
	 (integer-arrays nil)
	 (strs nil)
	 (gens nil)
	 (gen-arrays nil)
	 (got-globals nil)
	 (g-ints nil)
	 (g-bools nil)
	 (g-floats nil)
	 (g-float-arrays nil)
	 (g-integer-arrays nil)
	 (g-double-arrays nil)
	 (g-strs nil)
	 (g-gens nil)
	 (g-gen-arrays nil)
	 (lcoop (and loop-var (lc loop-var)))
	 (loopv (and loop-var (gethash loop-var vars))))
    (when loop-var
      (setf (varinfo-refd loopv) t)
      (setf (varinfo-temp loopv) t)
      (setf (varinfo-type loopv) :integer)) ;:clm-integer means it can't be shadowed by local loop counters

    (if *clm-report-untyped-vars*
	(format t "~%untyped variables: "))

    (maphash #'(lambda (key lst)
		 (when (varinfo-refd lst)
		   (let ((depth (varinfo-max-depth lst)))

		     (if *clm-report-untyped-vars*
			 (if (and (not (varinfo-type lst))
				  (not (varinfo-gen-type lst)))
			     (format t "~A " key)))

		     (cond ((eq (varinfo-type lst) :real)
			    (setf (varinfo-rloc lst) dat-r)
			    (push (list key 0 dat-r :real) variable-load-list)
			    (if (varinfo-global lst)
				(push (lc key) g-floats)
			      (push (lc key) floats))
			    (incf dat-r depth))

			   ((member (varinfo-type lst) '(:integer :bignum))
			    (setf (varinfo-iloc lst) dat-i)
			    (if (not (varinfo-temp lst)) (push (list key dat-i 0 :integer) variable-load-list))
			    (if (varinfo-global lst)
				(push (lc key) g-ints)
			      (push (lc key) ints))
			    (incf dat-i (* 2 depth)))

			   ((eq (varinfo-type lst) :boolean)
			    (setf (varinfo-iloc lst) dat-i)
			    (if (not (varinfo-temp lst)) (push (list key dat-i 0 :boolean) variable-load-list))
			    (if (varinfo-global lst)
				(push (lc key) g-bools)
			      (push (lc key) bools))
			    (incf dat-i (* 2 depth)))

			   ((eq (varinfo-type lst) :clm-real)
			    (if (varinfo-global lst)
				(push (lc key) g-floats)
			      (push (lc key) floats)))

			   ((eq (varinfo-type lst) :clm-integer)
			    (if (varinfo-global lst)
				(push (lc key) g-ints)
			      (push (lc key) ints)))

			   ((eq (varinfo-type lst) :clm-boolean)
			    (if (varinfo-global lst)
				(push (lc key) g-bools)
			      (push (lc key) bools)))

			   ((eq (varinfo-type lst) :string)
			    (if (not (varinfo-temp lst))
				(progn
				  (push (list key dat-i 0 :string) variable-load-list)
				  (setf (varinfo-iloc lst) dat-i)))
			    (if (varinfo-global lst)
				(push (lc key) g-strs)
			      (push (lc key) strs))
			    (incf dat-i depth))

			   ((eq (varinfo-type lst) :float-array)
			    (if (varinfo-global lst)
				(push (lc key) g-float-arrays)
			      (push (lc key) float-arrays)))

			   ((eq (varinfo-type lst) :double-array)
			    (if (not (varinfo-temp lst))
				(progn
				  (push (list key dat-i 0 :double-array) variable-load-list)
				  (setf (varinfo-iloc lst) dat-i)
				  (incf dat-i (* 2 depth))))
			    (if (varinfo-global lst)
				(push (lc key) g-double-arrays)
			      (push (lc key) double-arrays)))

			   ((eq (varinfo-type lst) :integer-array)
			    (if (not (varinfo-temp lst))
				(progn
				  (push (list key dat-i 0 :integer-array) variable-load-list)
				  (setf (varinfo-iloc lst) dat-i)
				  (incf dat-i (* 2 depth))))
			    (if (varinfo-global lst)
				(push (lc key) g-integer-arrays)
			      (push (lc key) integer-arrays)))

			   ((eq (varinfo-type lst) :mus-any)
			    (if (varinfo-global lst)
				(push (lc key) g-gens)
			      (push (lc key) gens)))

			   (t
			    (if (varinfo-gen-type lst)
				(if (eq (varinfo-gen-type lst) :mus-any-array)
				    (if (varinfo-global lst)
					(push (lc key) g-gen-arrays)
				      (push (lc key) gen-arrays))
				  (progn
				    (if (eq (varinfo-gen-type lst) :mus-any)
					(if (or (eq key '*output*)
						(eq key '*reverb*))
					    (setf (varinfo-gen-type lst) 'frample->file)
					  (if (not (varinfo-temp lst))
					      (warn "unknown global gen: ~A" key))))
				    (if (varinfo-global lst)
					(push (lc key) g-gens)
				      (push (lc key) gens))))))))))
	     vars)
    (if *clm-report-untyped-vars*
	(format t "~%~%"))

    (when (or g-ints g-floats g-float-arrays g-strs g-gens g-gen-arrays g-bools g-double-arrays g-integer-arrays)
      (setf got-globals t)
      (format *c-file* "/* variables used by anonymous \"as-needed\" functions */~%")
      (format *c-file* "static int *clm_int = NULL;~%")
      (format *c-file* "static double *clm_double = NULL;~%"))
    (if g-ints (format *c-file* "static mus_long_t~{ ~A~^,~};~%" g-ints))
    (if g-bools (format *c-file* "static bool~{ ~A~^,~};~%" g-bools))
    (if g-floats (format *c-file* "static double~{ ~A~^,~};~%" g-floats))
    (if g-float-arrays (format *c-file* "static double~{ *~A = NULL~^,~};~%" g-float-arrays))
    (if g-double-arrays (format *c-file* "static double~{ *~A = NULL~^,~};~%" g-double-arrays))
    (if g-integer-arrays (format *c-file* "static int~{ *~A = NULL~^,~};~%" g-integer-arrays))
    (if g-strs (format *c-file* "static char~{ *~A = NULL~^,~};~%" g-strs))
    (if g-gens (format *c-file* "static mus_any~{ *~A = NULL~^,~};~%" g-gens))
    (if g-gen-arrays (format *c-file* "static mus_any~{ **~A_array = NULL~^,~};~%" g-gen-arrays))
    (if (> function-ctr 0)
	(loop for func in *as-needed-function-types* do
	  (let ((type (car func))
		(ctr (cadr func)))
	    (if *clm-debug* (format t "func ~A ~A~%" type ctr))
	    (if (eq type :input)
		(format *c-file* "static mus_float_t as_needed_~D(void *gen, int direction);~%" ctr)
	      (if (eq type :edit)
		  (format *c-file* "static int as_needed_~D(void *closure);~%" ctr)
		(if (eq type :analyze)
		    (format *c-file* "static bool as_needed_~D(void *closure, mus_float_t (*input)(void *arg1, int direction));~%" ctr)
		  (format *c-file* "static mus_float_t as_needed_~D(void *closure);~%" ctr)))))))
    (if (or g-ints g-floats g-gens g-gen-arrays g-strs g-float-arrays g-double-arrays g-integer-arrays (> function-ctr 0))
	(format *c-file* "~%"))

    (format *c-file* "~Aint ~A (double *~Aclm_double, int datar_len, int *~Aclm_int, int datai_len)~%{~%"
	    #+(and excl windoze) "_declspec(dllexport) " #-(and excl windoze) ""
	    *c-proc*
	    (if got-globals "incoming_" "")
	    (if got-globals "incoming_" "")
	    )

    (format *c-file* "  /* The _clm_* variables are temporary values generated by the run macro */~%")
    (format *c-file* "  void (*old_SIGINT)(int);~%")
    (if ints (format *c-file* "  mus_long_t~{ ~A~^,~};~%" ints))
    (if bools (format *c-file* "  bool~{ ~A~^,~};~%" bools))
    (if strs (format *c-file* "  char~{ *~A = NULL~^,~};~%" strs))
    (if floats (format *c-file* "  double~{ ~A~^,~};~%" floats))
    (if float-arrays (format *c-file* "  double~{ *~A = NULL~^,~};~%" float-arrays))
    (if double-arrays (format *c-file* "  double~{ *~A = NULL~^,~};~%" double-arrays))
    (if integer-arrays (format *c-file* "  int~{ *~A = NULL~^,~};~%" integer-arrays)) ; it's a pointer into clm_int which is declared int
    (if gens (format *c-file* "  mus_any~{ *~A = NULL~^,~};~%" gens))
    (if gen-arrays (format *c-file* "  mus_any~{ **~A_array = NULL~^,~};~%" gen-arrays))
    (format *c-file* "  void *all_gens = NULL;~%")
    (format *c-file* "  mus_long_t clm_beg, clm_end;~%")

    (format *c-file* "  ")
    (maphash #'(lambda (key val)
		 (let* ((c-name (lc key)))
		   ;; :clm-* temps are already allocated and need no initialization
		   (if (eq (varinfo-type val) :integer)
		       (if (varinfo-iloc val)
			   (progn
			     ;(format *c-file* "mus_long_t ~A; " c-name)
			     (if (varinfo-shadowed val)
				 (loop for i from 1 below (varinfo-max-depth val) do
				   (format *c-file* "mus_long_t ~A_~A; " c-name i)))))
		     (if (eq (varinfo-type val) :real)
			 (if (varinfo-rloc val)
			     (progn
			       ;(format *c-file* "double ~A; " c-name)
			       (if (varinfo-shadowed val)
				   (loop for i from 1 below (varinfo-max-depth val) do
				     (format *c-file* "double ~A_~A; " c-name i)))))))))
	     vars)
    (format *c-file* "~%")

    #+sgi (progn
	    (format *c-file* "  union fpc_csr fpunder_f;~%")
	    (format *c-file* "  fpunder_f.fc_word = get_fpc_csr();  fpunder_f.fc_struct.flush = 1;  set_fpc_csr(fpunder_f.fc_word);~%"))

    (if got-globals
	(format *c-file* "  clm_int = incoming_clm_int;~%  clm_double = incoming_clm_double;~%"))
    (format *c-file* "  all_gens = clm_make_genbag();~%")
    (format *c-file* "  clm_beg = clm_to_mus_long_t(clm_int, ~D);~%" +clm-beg+)
    (format *c-file* "  clm_end = clm_to_mus_long_t(clm_int, ~D);~%" +clm-end+)

    ;; need two passes here -- as-needed internals need to be created
    ;;   first since src, for example, may call them when it is created
    (maphash #'(lambda (key val)
		 (let* ((c-name (lc key)))
		   (if (and (null (varinfo-type val))
			    (varinfo-refd val))
		       (if (and (varinfo-gen-type val)
				(not (eq (varinfo-gen-type val) :mus-any))
				(not (eq (varinfo-gen-type val) :mus-any-array))
				(varinfo-global val)
				(not (varinfo-ctr val)))
			   (progn
			     (gen-make-1 (varinfo-gen-type val)
					 *c-file*
					 c-name
					 (format nil "~D" (varinfo-iloc val))
					 "  "
					 (varinfo-ctr val))
			     (setf (varinfo-loaded val) t))))))
	     vars)

    (maphash #'(lambda (key val)
		 (let* ((c-name (lc key)))
		   ;; :clm-* temps are already allocated and need no initialization
		   ;; here we are defining offsets into clm_int and clm_double

		   (cond ((eq (varinfo-type val) :integer)
			  (if (varinfo-iloc val)
			      (progn
				(format *c-file* "  ~A = clm_int[~D];~%" c-name (varinfo-iloc val))
				(if (varinfo-shadowed val)
				    (loop for i from 1 below (varinfo-max-depth val) do
				      (format *c-file* "  ~A_~A = clm_int[~D];~%" c-name i (+ i (varinfo-iloc val))))))))

			 ((eq (varinfo-type val) :boolean)
			  (if (varinfo-iloc val)
			      (progn
				(format *c-file* "  ~A = (bool)clm_int[~D];~%" c-name (varinfo-iloc val))
				(if (varinfo-shadowed val)
				    (loop for i from 1 below (varinfo-max-depth val) do
				      (format *c-file* "  ~A_~A = (bool)clm_int[~D];~%" c-name i (+ i (varinfo-iloc val))))))))

			 ((eq (varinfo-type val) :real)
			  (if (varinfo-rloc val)
			      (progn
				(format *c-file* "  ~A = clm_double[~D];~%" c-name (varinfo-rloc val))
				(if (varinfo-shadowed val)
				    (loop for i from 1 below (varinfo-max-depth val) do
				      (format *c-file* "  ~A_~A = clm_double[~D];~%" c-name i (+ i (varinfo-rloc val))))))))

			 ((eq (varinfo-type val) :bignum)
			  (if (varinfo-iloc val)
			      (format *c-file* "  ~A = clm_to_mus_long_t(clm_int, ~D);~%" c-name (varinfo-iloc val))))

			 ((eq (varinfo-type val) :string)
			  (if (varinfo-iloc val)
			      (format *c-file* "  if (CLM_VAR_TYPE(~A) != CLM_NO_TYPE) ~A = (char *)(clm_int + clm_int[~A + 1] + 2);~%"
				      (varinfo-iloc val) c-name (varinfo-iloc val))))

			 ((eq (varinfo-type val) :double-array)
			  (if (varinfo-iloc val)
			      (format *c-file* "  if (CLM_VAR_TYPE(~A) != CLM_NO_TYPE) ~A = (double *)(clm_double + CLM_ARR_RBLOCK(CLM_VAR_ADDR(~A)));~%"
				      (varinfo-iloc val) c-name (varinfo-iloc val))))

			 ((eq (varinfo-type val) :integer-array)
			  (if (varinfo-iloc val)
			      (format *c-file* "  if (CLM_VAR_TYPE(~A) != CLM_NO_TYPE) ~A = (int *)(clm_int + CLM_ARR_IBLOCK(CLM_VAR_ADDR(~A)));~%"
				      (varinfo-iloc val) c-name (varinfo-iloc val))))

			 (t
			  (if (and (null (varinfo-type val))
				   (varinfo-refd val))
			      (if (varinfo-gen-type val)
				  (progn
				    (if *clm-debug* (format t "make ~A?~%" val))
				    (if (not (eq (varinfo-gen-type val) :mus-any))
					(if (eq (varinfo-gen-type val) :mus-any-array)
					    (progn
					      (if *clm-debug* (format t "making gen array~%"))
					      (format *c-file* "  #define ~A ~D~%  #define ~A_r ~D~%"
						      c-name (varinfo-iloc val)
						      c-name (varinfo-rloc val))
					      (gen-array-make key val))
					  (if (not (varinfo-loaded val)) ; not one handled in the first pass above
					      (gen-make-1 (varinfo-gen-type val)
							  *c-file*
							  c-name
							  (format nil "~D" (varinfo-iloc val))
							  "  "
							  (varinfo-ctr val))))))
				(format *c-file* "  #define ~A ~D~%  #define ~A_r ~D~%"
					c-name (varinfo-iloc val)
					c-name (varinfo-rloc val))))))))
	     vars)

    (if end
	(format *c-file* "  if (clm_beg > clm_end) return(1);~%"))
    (format *c-file* "  got_sigint = 0; old_SIGINT = clm_signal(SIGINT, sig_err); ~80,1T/* trap SIGINT */~%")
    (if loop-var (format *c-file* "  ~A = clm_beg; ~80,1T/* pass counter */~%" lcoop))

    (format *c-file* "~%SAMPLE_LOOP_BEGIN:~%")
    (format *c-file* "  if (got_sigint != 0) {clm_int[~D] = (int)got_sigint; goto RUN_ALL_DONE;}~%" +clm-interrupted+)
    (if (and end loop-var) (format *c-file* "  if (~A > clm_end) {~A = clm_end; goto RUN_ALL_DONE;}~%" lcoop lcoop))
    )
  nil)

(defun get-var-sizes (i r vallist)
  (let ((ni i)
	(nr r))
    (loop for cvar in vallist do
      (if (and (not (eq cvar :local)) cvar (not (eq cvar t)) (not (numberp cvar)))
	  (let* ((sizes (gen-size cvar)))
	    (when sizes
	      (incf ni (first sizes))
	      (incf nr (second sizes))))))
    (values ni nr)))

(defun pv ()
  (maphash #'(lambda (k v) (format t "~A ~A~%" k v)) common-tones::vars))

(defun load-vars (i r varlist vallist datai datar)
  ;; set variable pointers while loading data
  ;; i and r are already accomodating var space (not struct)

  ;;(format t "load-vars: ~D ~D ~A ~A~%" i r varlist vallist)
  ;;  (loop for var in varlist and val in vallist do (format t "~A: ~A~%" var val))

  (let ((ni i)
	(nr r))
    ;; varlist is a list of triples of variables that need data allocation and initialization
    (loop for cvar in vallist and var in varlist do
      (if (not (eq cvar :local))
	  (let ((iloc (second var))
		(rloc (third var)))
	    (if (eq (fourth var) :integer)
		#-cmu (setf (aref datai iloc) (floor cvar))
		#+cmu (setf (aref datai iloc) (ldb (byte 32 0) (floor cvar)))
              (if (eq (fourth var) :real)
		  (setf (aref datar rloc) (double cvar))
		(if (eq (fourth var) :bignum)
		    (to-bignum cvar datai iloc)
		  (if (eq (fourth var) :boolean)
		      (setf (aref datai iloc) (if cvar 1 0))
		  (let ((sizes (gen-size cvar)))
		    (if sizes
			(progn
			  (gen-load cvar ni nr datai datar)
			  (setf (aref datai iloc) (aref datai ni))
			  (setf (aref datai (1+ iloc)) ni)
			  (if (> (second sizes) 0) (setf (aref datar rloc) (aref datar nr)))
			  (incf ni (first sizes))
			  (incf nr (second sizes)))
		      (gen-load cvar iloc rloc datai datar))))))))))))

(defmacro <end-1> ()
  (if loop-var
      (format *c-file* "  ~A++;~80,1T/* increment pass counter and loop */~%goto SAMPLE_LOOP_BEGIN;~%~%" (lc loop-var))
    (format *c-file* "goto RUN_ALL_DONE;~%~%"))

  (format *c-file* "RUN_ALL_DONE:~%")
  (format *c-file* "  clm_signal(SIGINT,old_SIGINT);~%")
  (if *with-reflection*
      (maphash #'(lambda (key val)
		   (let* ((c-name (lc key)))
		     (if (not (varinfo-shadowed val))
			 (if (and (or (eq (varinfo-type val) :integer)
				      (eq (varinfo-type val) :boolean))
				  (varinfo-iloc val))
			     (format *c-file* "  clm_int[~D] = ~A;~%" (varinfo-iloc val) c-name)
			   (if (and (eq (varinfo-type val) :real)
				    (varinfo-rloc val))
			       (format *c-file* "  clm_double[~D] = ~A;~%" (varinfo-rloc val) c-name)
			     (if (and (varinfo-gen-type val)
				      (not (eq (varinfo-gen-type val) :mus-any))
				      (not (eq (varinfo-gen-type val) :mus-any-array))
				      (not (eq (varinfo-type val) :bignum))
				      (>= (varinfo-depth val) 0)) ; otherwise an internal let binding etc
				 (gen-reflect-1 (varinfo-gen-type val) key val)))))))
	       vars))
  (format *c-file* "  if (all_gens) clm_free_genbag(all_gens);~%")
  (maphash #'(lambda (key lst)
	       (when (varinfo-refd lst)
		 (if (eq (varinfo-gen-type lst) :mus-any-array)
		     (format *c-file* "  if (~A_array) free(~A_array);~%" (lc key) (lc key)))))
	   vars)
  (format *c-file* "~%  return(1);~%}~%~%")
  (setf *safety* 0) ; don't try to goto RUN_ALL_DONE in the as-needed-input functions
  nil)

(defmacro <end-2> (beg end end-specified)
   `(let ((vardata (list ,@(map 'list #'(lambda (var)
					    (let ((lst (gethash (first var) vars)))
					      (if (minusp (varinfo-depth lst))
						  :local
						(first var))))
				  variable-load-list))))
	;; find how big the clm_int and clm_double arrays need to be (scan variable-load-list)
	(multiple-value-bind
	    (*clm-datai-len* *clm-datar-len*)
	    (get-var-sizes ,dat-i ,dat-r vardata)
	  (let ((*clm-beg* (floor (+ ,beg *offset*)))
		(*clm-end* (and (numberp ,end) (floor (+ ,end *offset*))))
		(*clm-datai* (make-integer-array *clm-datai-len* :initial-element 0))
		;; type must not be 'integer here because ACL shifts every element over 3 from C's point of view.
		(*clm-datar* (make-double-array *clm-datar-len*)))

;	    (format t "var sizes: ~A ~A~%" *clm-datai-len* *clm-datar-len*)

	    (load-vars ,dat-i ,dat-r ',variable-load-list vardata *clm-datai* *clm-datar*)
	    (to-bignum *clm-beg* *clm-datai* +clm-beg+)
	    (to-bignum (or *clm-end* 0) *clm-datai* +clm-end+)

	    (if (and ,end-specified
		     (< *clm-end* *clm-beg*))
		(warn "end time < begin time: ~A from ~A to ~A?" ',*lisp-proc* *clm-beg* *clm-end*))

	    (if *clm-debug*
		(print (format nil "(~A ~A~%  ~A~%  ~A ~A ~A)" ',*c-proc* *clm-beg* *clm-end* *clm-datar* *clm-datai* *clm-datar-len*)))

	    (setf (get *clm-ins* :datai) *clm-datai*)
	    (setf (get *clm-ins* :datar) *clm-datar*)
	    (setf clm-last-begin-time (max clm-last-begin-time *clm-beg*))
	    (progn
	      #-openmcl (tagbody
		     (restart-case
			(,*c-proc* *clm-datar* *clm-datar-len* *clm-datai* *clm-datai-len*)
			(nil ()
			     :report "try to exit current note cleanly and go on."
			     (go NOTE-ALL-DONE)))
		       NOTE-ALL-DONE
		       )
	      #+openmcl
	      (tagbody
		       (restart-case
			(ccl::with-foreign-double-float-array (p *clm-datar*)
			  (,*c-proc* p *clm-datar-len* (heap-int* *clm-datai*) *clm-datai-len*))
			(nil ()
			     :report "try to exit current note cleanly and go on."
			     (go NOTE-ALL-DONE)))
		       NOTE-ALL-DONE
		       )
		)

	    (if (not (zerop (aref *clm-datai* +clm-interrupted+)))
		(if (not (= (aref *clm-datai* +clm-interrupted+) -1))
		    (format t "interrupted: ~A" (aref *clm-datai* +clm-interrupted+))
		  (error "error called within run")))

	    ))))

;;; % in clm-print should be translated in C to %%
