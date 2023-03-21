#+(and allegro-version>= (version>= 5 0)) 
(eval-when (compile load eval) 
  (pushnew :acl-50 *features*))

#+(and allegro-version>= (version>= 6 0)) 
(eval-when (compile load eval) 
  (pushnew :acl-60 *features*))

#+(and allegro-version>= (version>= 6 1)) 
(eval-when (compile load eval) 
  (pushnew :acl-61 *features*))

#+(and allegro-version>= (version>= 6 2)) 
(eval-when (compile load eval) 
  (pushnew :acl-62 *features*))

#+(and allegro-version>= (version>= 7 0)) 
(eval-when (compile load eval) 
  (pushnew :acl-70 *features*))

#+(and allegro-version>= (version>= 8 0)) 
(eval-when (compile load eval) 
  (pushnew :acl-80 *features*))

;;; forced to put this in a separate file because other lisps die
;;; upon encountering any part of it despite Franz's assertion
;;; that it is "truly portable code"

(setf tpl:*zoom-print-level* nil)
(setf tpl:*zoom-print-length* nil)
(setf tpl:*print-level* nil)


