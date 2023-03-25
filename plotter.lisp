;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             plotter.lsp
;;;
;;; Version:          1.0
;;;
;;; Purpose:          
;;;
;;;                   Definition of a plotter class implementing a very simple
;;;                   interface from lisp to gnuplot.  This facilitates the
;;;                   creation of simple 2-D (x,y) data plots.  Gnuplot can be
;;;                   found at http://www.gnuplot.org/
;;;
;;;                   The output of gnuplot is set to the portable bit map
;;;                   (pbm) format.  This is automatically displayed using
;;;                   Electric Eyes (ee), the simple image viewer/editor that
;;;                   comes with Red Hat Linux.  This has the advantage over
;;;                   using the default X11 output of gnuplot in that you can
;;;                   use ee to save the plot in various image formats should
;;;                   you wish to.  This is a must if you want to save the plot
;;;                   at all as the image file you will be looking at will have
;;;                   been deleted pretty soon after being created.  The idea
;;;                   behind this strange behaviour was that the way I use the
;;;                   program is to have many plots springing up all over the
;;;                   place to aid in various experiments I make, without being
;;;                   troubled to delete the files afterwards-- I just close
;;;                   the window and carry on.
;;;
;;; Usage:            
;;;
;;;                   Basically a three-step program: 
;;;                       init, add points, display 
;;;                   e.g.: 
;;;
;;;                   (let ((plot (init-plot)))
;;;                     (loop for i below 100 do ;; or whatever...
;;;                           (add-point i (random 10.0) plot))
;;;                     (display-plot plot))
;;;
;;;                   Naturally, you can have as many plot objects as you want
;;;                   running around and display them in any order you wish.
;;;
;;; Configuration: 
;;;                   
;;;                   Instead of using global variables I use static class
;;;                   members to store the paths to the gnuplot program, ee
;;;                   etc.  These and other things can be edited in the
;;;                   defclass form (see below for comments).  Change the
;;;                   :initform key argument to each slot if you need to.
;;;
;;; Bugs:             
;;;
;;;                   Several temporary files are created in the process of
;;;                   creating the plot which should then be deleted by the
;;;                   program.  When you close the plot window, there should be
;;;                   no files hanging around that you have to delete.
;;;                   However, if the viewer program takes too long to open the
;;;                   file, it may have been deleted too soon so you won't see
;;;                   anything.  Sorry about that, maybe I'll think of a better
;;;                   solution next time (UNIX pipes?...)  In the meantime, if
;;;                   this happens, try increasing the value of the
;;;                   sleep-hack-duration slot in the plotter defclass.
;;;
;;;                   If you initialize a plotter object but then never call
;;;                   display, then there'll definitely be a few plotter-*
;;;                   files hanging around in /tmp that you can safely delete.
;;;                   If only there was the CLOS equivalent of a C++
;;;                   destructor...
;;;
;;;                   This program is really more of a script than a program
;;;                   and as such is rather tailor-made for CLisp running on
;;;                   Red-Hat Linux, which is rather a restricted view of the
;;;                   world.  It could be easily ported to other OS's though
;;;                   just by changing the directories, possibly the name of
;;;                   the shell function (simply "shell" in CLisp) and the
;;;                   image viewer.  The use of gnuplot should remain the same
;;;                   as far as I can tell.
;;;
;;; Author:           Michael Edwards: michael@ccrma.stanford.edu
;;;
;;; Creation date:    10.12.00
;;;
;;; $$ Last modified: 14:15:42 Mon Dec 11 2000 CET
;;;
;;; Changed on Thu Jan 11 11:09:48 AST 2001
;;; by Marco Trevisani
;;; 1) (defmethod display...  becomes (defmethod gdisplay...
;;; "display" in cmucl conflicts with an already existing function call
;;; 2) replace "shell" with "run-in-shell"
;;; "shell" is a specific function of clisp, run-in-shell is a more general
;;; clm call.
;;; 3) replace /zap with /tmp -- a standard temporary directory in Unix/Linux
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass plotter ()

  ;; feel free to change any of the following slots.
  
  ;; the path to the gnuplot program
  ((gnuplot :initform "/usr/bin/gnuplot" :accessor gnuplot :allocation :class)
   ;; the path to the image-viewer program--any would do as long as it can be
   ;; called with the image name as the first command line argument and it can
   ;; read pbm format.
   (image-viewer :initform "/usr/bin/eeyes" :accessor image-viewer
                 :allocation :class)
   ;; the temporary directory where data and image files are stored before
   ;; being deleted.
   (tmp-dir :initform "/tmp" :accessor tmp-dir :allocation :class)
   ;; how long the display function sleeps in seconds between firing up the
   ;; image viewer and deleting the temp files (see bugs above)
   (sleep-hack-duration :initform 1 :accessor sleep-hack-duration
                        :allocation :class)

   ;; don't edit anything below
   (num-active-plotters :type integer :initform 0 :allocation :class 
                        :accessor num-active-plotters)
   (active :type boolean :accessor active)
   (data-file :type string :accessor data-file)
   (data-stream :type file-stream :accessor data-stream)
   (command-file :type string :accessor command-file)
   (image-file :type string :accessor image-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun init-plot ()
  (make-instance 'plotter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod initialize-instance :after ((i plotter) &rest initargs)
  (let ((tmp-dir (tmp-dir i)))
    (incf (num-active-plotters i))
    (setf (data-file i) (format nil "~a/plotter-data-file~a.txt"
                                tmp-dir (num-active-plotters i))
          ;; no chance of using the with-open-file safe version of Lisp IO here
          (data-stream i) (open (data-file i)
                                :direction :output :if-exists :overwrite
                                :if-does-not-exist :create)
          (command-file i) (format nil "~a/plotter-command-file~a.txt"
                                   tmp-dir (num-active-plotters i))
          (image-file i) (format nil "~a/plotter-image-file~a.pbm"
                                 tmp-dir (num-active-plotters i))
          (active i) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-object ((i plotter) stream)
  (format stream 
          "~&    active: ~a~
           ~&    data-file: ~a~
           ~&    data-stream: ~a~
           ~&    command-file: ~a~
           ~&    image-file: ~a~
           ~&    Class variables: ~
           ~&        gnu-plot: ~a~
           ~&        image-viewer: ~a~
           ~&        tmp-dir: ~a~
           ~&        sleep-hack-duration: ~a~
           ~&        num-active-plotters: ~a~%"
          (active i)
          (data-file i) 
          (data-stream i)
          (command-file i) 
          (image-file i)
          (gnuplot i)
          (image-viewer i)
          (tmp-dir i)
          (sleep-hack-duration i)
          (num-active-plotters i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod add-point (x y (i plotter))
  (unless (active i)
    (plotter-not-active i))
  (format (data-stream i) 
          "~&~,6f ~,6f"
          x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod gdisplay ((i plotter))
  (unless (active i)
    (plotter-not-active i))
  (close (data-stream i))
  (create-command-file i)
  ;; mettere +cmu e +clisp per i differenti run-in shell
  (run-in-shell (gnuplot i) (command-file i))
  (run-in-shell (image-viewer i) (image-file i))
  ;; wait for one (or more) second for the image viewer to open the file then
  ;; delete the temp files.  This is not ideal, in fact it's a terrible hack,
  ;; but...
  (sleep (sleep-hack-duration i))
  (delete-file (data-file i))
  (delete-file (command-file i))
  (delete-file (image-file i))
  (decf (num-active-plotters i))
  (setf (active i) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod plotter-not-active ((i plotter))
  (error "Once display-plot has been called, the plotter is no longer active~
        ~&Please allocate another plotter with init-plot and continue: ~&~a"
         i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod create-command-file ((i plotter))
  (with-open-file 
   (command-stream (command-file i)
                   :direction :output :if-exists :overwrite 
                   :if-does-not-exist :create) 
   (format command-stream
           "~&set terminal pbm ~
            ~&set output \"~a\" ~
            ~&plot \"~a\" notitle~%"
           (image-file i)
           (data-file i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|

;;; Test code:

(progn
  (setf plot (init-plot))
  (loop for i below 100 do (add-point i (random 10.0) plot))
  (display plot))


|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF plotter.lsp


