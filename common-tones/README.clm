CLM


This is CLM, a sound synthesis package documented in clm.html.
The mailing list for clm news is cmdist@ccrma.stanford.edu.
To subscribe, visit

http://ccrma-mail.stanford.edu/mailman/listinfo/cmdist

This software is available to anyone who is interested, free gratis
for nothing, without warranties of any kind.  Send bug reports or
suggestions to bil@ccrma.stanford.edu.


I think CLM works in acl, openmcl, cmucl, sbcl, lispworks, and clisp
on Linux, *BSD, and OSX machines.  For current status of a given port, 
see the end of this file.


Users of CLM may be interested in snd-15.tar.gz at ccrma-ftp.
It is a sound editor that includes two-way communication
with CLM (using Scheme, Forth, or Ruby).  


Contents: How to build CLM
             Linux notes
             Windows notes
             Mac OSX notes
          URLS
          Foreign Function Interface Troubles
          Clisp troubles
          Current status


------------------------------------------------------------------------
How To Build CLM

In general start lisp and (load "all.lisp").

If your system isn't supported yet, load "all.lisp" anyway, and send
me the errors you hit.  In LispWorks, (load "lw-all.lisp").


----------------
Linux notes:

CLM's RIFF ('wave') output includes the non-standard chunk name "clm "
(it holds the CLM comment string).  If some dumb play program refuses
to deal with this, set the output header comment to nil, and try
again.

ALSA is now the default sound system in Linux.  If you don't have ALSA
(freeBSD for example), 

   (pushnew :oss *features*)

before you load all.lisp.



And here's a note from Fernando:

    > When running the command (compile-file "xyz.ins").  I get a warning that
    > tells me that make can't find the command cc.
    
    I assume you do have gcc installed (just to check do a "rpm -q -a | grep
    gcc" to make sure it is there - if it is not then you need to install the
    gcc rpms). In my system the gcc package includes a link in /usr/bin that
    points from cc to gcc. I don't know how to change the default in clm but
    you could try to set up a link to point cc to gcc, for example:
      cd /usr/bin; ln -s gcc cc
    -- Fernando


If "sndplay" (via the "dac" function) can't be found, you need
to either add the clm directory to your PATH environment
variable, or put the full pathname for sndplay in *clm-player*.
If sndplay is unhappy, try the system-provided playback program.
In Linux, that's aplay:

(setf *clm-player* (lambda (name) 
                     (clm::run-in-shell "aplay" 
                       (format nil "-t au -f float_le -r ~d ~a" clm::*clm-srate* name))))

(Thanks to Anton Gerasimov for this hint).


In Ubuntu, you need /bin/csh; here's Geoff Lee's note about this:

    ... the problem in this case was that Ubuntu 9.10 default install doesn't include 
    /bin/csh by default! This causes all the calls to gcc to fail in all.lisp as they 
    all invoke csh. 

    From a vanilla 9.10 Ubuntu install, the following were sufficient to get clm-4 up 
    and running with the ALSA audio stack: 

     sudo apt-get install emacs23 
     sudo apt-get install slime 
     sudo apt-get install libasound2-dev 
     sudo apt-get install libpthread-stubs0-dev 
     sudo apt-get install csh 



----------------
Windows notes:

First, copy config.windoze to mus-config.h and sndlib.windoze to sndlib.h in the clm directory.
On other systems, these files are created by the configure script, but that doesn't seem
to work in Windows.

If you use the Windows delimiter "\", remember that it is
the Lisp quote-the-next-character symbol, so you need to
repeat it:

(load "c:\\bil\\clm\\all.lisp")

Or in ACL use :cd:

> :cd /bil/clm
> :ld all.lisp

If you're running ACL 5.0 (not ACL Lite) with Microsoft C,
first copy the ACL file acl503.lib to the current (CLM) directory.
If it is named something else, fixup the name in all.lisp.
(In ACL 6, it's apparently named acl601.lib).
CLM does work with ACL Lite in Windoze -- load all.lisp as
usual (it will build libclm.dll), then to use an instrument,
load its code (i.e. skip the compile-file step).

I've assumed in all.lisp that you've used tar to unpack
clm-4.tar.gz, and that full-length file names are available.

If ACL 5.0 (with the FFI) has trouble finding the C compiler:

USER(1): :ld /clm/all.lisp
; Loading C:\clm\all.lisp
;   Loading C:\clm\acl.cl
; Compiling "C:\\clm\\io.c"
Error: "starting shell command" resulted in error
       "No such file or directory"
  [condition type: SIMPLE-ERROR]

Michael Edwards figured out how to fix this:

    "All that needed to be changed was the autoexec.bat file.  A few environment
    variables needed to be added or added to, that's all.  Basically, if you can get
    your autoexec.bat file set up so that you can run the silly bloody MS compiler,
    then you're laughing.  So, my path was changed so:
    
    set PATH=%PATH%;c:\progra~1\devstu~1\vc\bin;c:\progra~1\devstu~1\shared~1\bin;
    
    Note that I had to use the DOS file names rather than c:\program files\...
    otherwise the system complains.
    
    The first dir is where cl.exe lives, the second is where MSPDB50.DLL lives (I
    guess this could be MSPDB60.DLL if you have a newer version of Visual C++).  
    
    Then, according to the compiler documentation, you have to set two other
    environment variables, LIB and INCLUDE, to point to the correct places:
    
    set LIB=c:\program files\devstudio\vc\lib
    set INCLUDE=c:\program files\devstudio\vc\include
    
    Note that there are no double quotes around these paths, and here, the full
    32-bit files names are OK....sigh...
    
    Anyway, once you've changed the autoexec.bat file, I would suggest running it in
    a command prompt window so that you can see if there are any errors (they don't
    show up so easily when you start up your system).  If your path is too long, or
    if you have too many environment variables, then you might get the error "out of
    environment space" as you try and set your new variables.  If this doesn't
    happen, you should be set to restart your system (full restart, not just windows
    restart), fire up lisp and :ld /clm/all.lisp".


Bill sez: when MSVC installs itself, it prints out the name of a file that
    has the necessary commands to fixup the variables mentioned above.  On my
    system, it was named C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT.
    I copied all that gobbledy-gook into c:AUTOEXEC.BAT, rebooted, and ACL was happy.
    Cygwin has a similar setup file named something like C:cygnus\cygwin-b20\cygnus.bat.
    If you want to use gcc et al, this apparently needs to be executed.  Currently
    all.lisp assumes cl is the C compiler name in Windoze.


Kenny Tilton supplies this ACL-related info:

    I am playing with one of the drum.ins instruments I found on the CCRMA site,
    so I had a problem with ACL under windows: once a DLL was loaded it was apparently
    held open or something, so a subsequent compile of the (definstrument...)
    (or compile-file of the same .ins) would fail on "unable to write file" or
    something. After that, using the instrument complained about the function
    no longer being loaded. Anyway, here's the fix I use:

      (defun unload-instrument (ins-name)
        (dolist (dll (ff:list-all-foreign-libraries))
          (when (search (symbol-name ins-name) (pathname-name dll))
            (print `(Unloading foreign library ,dll))
            (ff:unload-foreign-library dll))))

    Then:

      (unload-instrument 'djembe)

    where the finstrument name was actually fm-djembe (SEARCH looks for subsequences)
    Big warning: since the unloading goes by matching any substring, if I had three or
    four (the way developers often do) I'd zap them all with the above. So if I do start
    doing djembesoft and djembezzz and djembe123, I need to "unload" with enough of the
    name to nail the one I am after.


In ACL 7.0, The MS compiler has changed again; here's some info from Michael Edwards:

    It seems the MS compiler has separated its includes and libraries out into two
    separate directories now: in the compiler's directory: Vc7/include (or lib) and
    Vc7/PlatformSDK/inlude (or lib) and whereas one directory each in the environment
    variables for lib and include would suffice before, now you have to have two.

    So on WinXP with the compiler installed into the default directory (c:/program files)
    my environment variables look like this:

        INCLUDE: C:\Program Files\Microsoft Visual Studio .NET\Vc7\include;C:\Program Files\Microsoft Visual Studio .NET\Vc7\PlatformSDK\Include
        LIB C:\Program Files\Microsoft Visual Studio .NET\Vc7\lib;C:\Program Files\Microsoft Visual Studio .NET\Vc7\PlatformSDK\lib

    You set these by going to the Control Panel->System Properties, selecting the Advanced
    tab then clicking Environment Variables at the bottom of the window (instead of editing
    the autoexec.bat file on older windows systems).


----------------
Mac OSX notes:

openmcl version 0.13 or later is needed (CLM needs open-shared-library).
The dac function seems to be getting interruptions from lisp in openmcl --
you may need to use sndplay (from a terminal) or some other program like
QuickTime to play CLM output.  sl-dac doesn't work at all.  The Motu card
probably doesn't work (with dac/sndplay), and I don't know how to fix it.


Bret Battey says:
"To use CLM, you will need OSX Xcode Tools in order to provide the C compiler
and other resources. New Macintoshes are generally not delivered with Xcode
Tools already installed, but in this case you may find the Xcode Tools installer
in an "Installers" folder in the computer's "Applications" folder. An Xcode Tools
CD may be included if you purchased OSX as an upgrade in a box. You can also
download the installer package directly from the Apple Developer Connection
Member Site. IMPORTANT: the default Xcode Tools install does not install all
components necessary to run CLM. When running the Xcode Tools installer,
customize the install and make sure ALL components of Xcode Tools are
selected for installation."


Geoff Lee points out that in sbcl you need to "use the 64-bit sbcl 
in OSX > 10.6 if you are using a machine with a 64-bit capable processor 
(ie core 2 and above, but not core duo or core solo)".


------------------------------------------------------------------------
walk.lisp:

The version of walk.lisp that comes with CLM is not exactly the same as
the ones floating around the net.  In particular, if you're using CCL in
OSX, add #-clozure directly above "(defun macroexpand-all" at around 
line 1339. It should look like this: 

#-clozure 
(defun macroexpand-all (form &optional (environment *toplevel-environment*)) 
(let ((walk-form-expand-macros-p t)) 
  (walk-form form environment))) 



------------------------------------------------------------------------
URLS:

  http://ccrma.stanford.edu/ (ccrma home page)
  http://ccrma.stanford.edu/planetccrma/software/ (CLM RPM images)
  http://ccrma-mail.stanford.edu/mailman/listinfo/planetccrma
  http://ccrma.stanford.edu/~jos/ (Julius Smith's home page)
  http://ccrma.stanford.edu/~juan/ATS.html
  http://sourceforge.net/projects/commonmusic

------------------------------------------------------------------------
Foreign Function Interface Troubles

CMU-CL:
After compiling an instrument, load the file <ins>.cmucl, not the x86f
compiler output.  For example:

(compile-file "v.ins")
(load "v.cmucl")

Also, cmucl only supports one definstrument per file -- this is due
to a long-standing bug in cmucl's load function.


If you get the error: ld: internal error ldlang.c 3088
from all.lisp when it is trying to build libclm.so, 
comment out line 490 of all.lisp:

#-(or windoze (and linux (or cmu acl-new-ffi))) " -lm -lc"

If that doesn't work, try without -lc or -lm (i.e some combination
of libraries should work).


ACL 8.2:
  on 32-bit machines, before loading all.lisp, in mus-config.h.in, 
  change the line

    #define mus_long_t int64_t
  to
    #define mus_long_t int

  Then, in Linux, if you get the error 
    "libclm.so: cannot restore segment prot after reloc: Permission denied."
  while loading all.lisp, change

    (defvar use-chcon nil)
  in all.lisp to
    (defvar use-chcon t)



------------------------------------------------------------------------
Clisp troubles

Several functions don't work in Clisp: basic-convolve, clm-fft, clm-swap-doubles,
run* -- anything that requires that C return an array of values to Lisp
and that isn't handled specially.  Also, "loop" within the run loop
doesn't work because Clisp's expansion of the loop macro requires handling
local "loop-finish" macro definitions -- use "do" instead.  On the Sun,
you need to (pushnew :sun *features*) before loading all.lisp.


Also, sbcl is not compatible with Jack.

------------------------------------------------------------------------
current status of both CLM and CMN (so I can keep track of these things):

SBCL:
  using sbcl-fix.lisp from R Mattes

  1.0.24: cmn netBSD: [cmntest 13-Jan-09]
          clm netBSD: [clmtest 13-Jan-09]

  1.0.50: cmn mac: [cmntest 13-July-11]
          clm mac: [clm-test 13-July-11]

  1.0.53: cmn linux 32: [cmntest 7-Nov-11]
          clm linux 32: [clm-test 7-Nov-11]

  1.0.55: cmn linux: [cmntest 9-Jan-12]
          clm linux: [clm-test 9-Jan-12]


CMUCL 19b:  cmn netBSD: [cmntest 25-Oct-05]
        
      19c:  cmn freeBSD: [cmntest 25-Oct-05]

      19e:  cmn solaris sparc: [cmntest 6-Mar-08]
            clm solaris sparc: [(no test suite) 6-Mar-08]

      20b:  cmn linux 32: [cmntest 9-Aug-10]
            clm linux 32: [(no test suite) 9-Aug-10]

      20c:  cmn mac: [cmntest 7-Nov-11]
            clm mac: [(no test suite) 7-Nov-11]


OPENMCL 0.14.3: cmn mac: [cmntest 24-Oct-05]
                clm mac: [clm-test 24-Oct-05]

        1.0.0:  cmn mac: [cmntest 24-Oct-05]
                clm mac: 13-Nov-05: loads ok, clm-test except "\." in filenames causes trouble,
                  clm-seek-floats|ints are no-ops, and run* appears to be broken.
                
                cmn linux: [cmntest 20-May-07]
                clm linux: [clm-test 20-Feb-08]

CLOZURE CL (OpenMCL) 
          1.1:  cmn linux: [cmntest 23-Apr-08]
                clm linux: [clm-test 23-Apr-08]

          1.2:  cmn linux: [cmntest 25-Feb-09]
                clm linux: [clm-test 25-Feb-09]


CLISP 2.35: cmn freeBSD: [cmntest 29-Oct-05]

      2.41: cmn solaris 64: [cmntest 15-Oct-06] (can't build later versions of clisp?)
            clm solaris 64: [clm-test 15-Oct-06] 

            clm netBSD: [clm-test 15-Oct-06]

      2.42: cmn netBSD: [cmntest 18-Oct-07]

      2.49: cmn linux 32: [cmntest 8-Jul-10]
            clm linux 32: [clm-test 8-Jul-10]

            cmn mac: [cmntest 8-Jul-10]
            clm mac: [clm-test 8-Jul-10]

            cmn linux: [cmntest 8-Jul-10]
            clm linux: [clm-test 8-Jul-10]
              hangs while compiling piano.ins


ACL 8.1: cmn mac: [cmntest 9-May-07]
         clm mac: hangs in ug2 test, just as in linux -- otherwise ok

         cmn Linux 32: [cmntest 9-May-07]
         clm Linux 32: segfault in clm-test
                    Is there a limit on the number of .so files one fasl files can load?
                    ug2 works fine cut in pieces, but dies if done all at once; everything else is happy

         cmn linux: [cmntest 10-May-07]
         clm linux: [clm-test 10-May-07]

         cmn solaris 64: [cmntest 10-May-07]
         clm solaris 64: loader troubles

ACL 8.2: cmn linux 32: [cmntest 5-Nov-09]
         clm linux 32: [clm-test 5-Nov-09]
           "libclm.so: cannot restore segment prot after reloc: Permission denied."
               libclm.so needs the same chcon setting as the acl libs
               chcon -t textrel_shlib_t libclm.so
               and every instrument as well!
               set use-chcon to t in all.lisp in this case
           convolution causes segfault in 32-bit case, but it's ok in the 64-bit case.
               The problem here is mus_long_t which should be int, not int64_t (mus-config.h.in)

         cmn linux: [cmntest 5-Nov-09]
         clm linux: [clm-test 5-Nov-09]

         cmn mac: [cmntest 5-Nov-09]
         clm mac: [clm-test 5-Nov-09]



---------------- CMN ONLY --------------------------------

ABCL: cmn linux 31-Jan-09 ported by Kjetil Matheussen

ECL 9.7.1:  cmn linux: [cmntest 5-Aug-09]

GCL 2.6.7: cmn linux: Cannot build with randomized sbrk
           cmn netBSD: confused about machine type, then dies with
             make: exec(../unixport/saved_pre_gcl) failed (No such file or directory)

XCL (Mar-08): cmn linux: compiles and loads but
            CMN(4): (cmn staff treble c4 q)
            Debugger invoked on condition of type TYPE-ERROR:
              The value #<EXTENSIONS:SPECIAL-OPERATOR QUOTE {8672B50}> is not of type FUNCTION.
            [1] CMN(6): :bt
              '(#<WRITE-PROTECTED-CLEF>)
              4: (#<SYSTEM:COMPILED-CLOSURE {9E5DF00}> #<WRITE-PROTECTED-CLEF> #<CLEF>)
              5: (#<SYSTEM:COMPILED-CLOSURE {9E5DE88}> #<WRITE-PROTECTED-CLEF>)
              6: (FUNCALL #<SYSTEM:COMPILED-CLOSURE {9E5DE88}> #<WRITE-PROTECTED-CLEF>)
              7: (APPLY #<SYSTEM:COMPILED-CLOSURE {9E5DE88}> (#<WRITE-PROTECTED-CLEF>))

