(in-package :common-tones/plugins)

/*!< ------------------------------------------------------------------------

/*!< with-instruments -- make sure needed instruments are loaded before going on.

/*!<

/*!< (with-instruments (FM-VIOLIN "v") (JC-REVERB "jcrev")) for example


(defmacro with-instruments (&rest ins)
  `(progn
     ,@(loop for i in ins collect
       `(when (not (member ',(first i) *clm-instruments*)) (compile-and-load ,(second i))))))