(in-package "SB-KERNEL") 
(defun round-numeric-bound (x class format up-p) 
 (if x 
     (let ((cx (if (consp x) (car x) x))) 
       (ecase class 
         ((nil rational) x) 
         (integer 
          (if (and (consp x) (integerp cx)) 
              (if up-p (1+ cx) (1- cx)) 
              (if up-p (ceiling cx) (floor cx)))) 
         (float 
          (let ((res 
                 (cond 
                   ((and format (subtypep format 'double-float)) 
                    (if (<= most-negative-double-float cx most-positive-double-float) 
                        (coerce cx format) 
                        nil)) 
                   (t 
                    (if (<= most-negative-single-float cx most-positive-single-float) 
                        ;; FIXME 
                        (coerce cx (or format 'single-float)) 
                        nil))))) 
            (if (consp x) (list res) res))))) 
     nil)) 
