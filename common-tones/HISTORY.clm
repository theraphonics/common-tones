           C L M   C H A N G E S

-------------------------------- 2018 --------------------------------------
8-May-18:  added (clm-initialize-links) at end of all.lisp

-------------------------------- 2015 --------------------------------------

7-Nov:     ccl bugfixes thanks to Matti Koskinen.
9-Sep:     bandedwg.ins thanks to Juan Reyes.
21-Apr:    frames/mixers removed, frames renamed framples.
15-Feb:    LispWorks port thanks to Anders Vinjar.
24-Jan-14: lw-all.lisp thanks to Frank Zalkow (for LispWorks).

-------------------------------- 2014 --------------------------------------

7-Jul:  removed sndlib.h.in, many other configuration-related changes.
27-Mar: removed mus-file-prescaler and mus-prescaler  -- these no longer serve any purpose.

-------------------------------- 2013 --------------------------------------

7-Nov:  defpackage change for cmucl 20d.
21-Mar: clozure 1.7 64-bit OAX changes thanks to Sean Reed and Michael Edwards.

-------------------------------- 2012 --------------------------------------

23-Nov: Michael Edwards added an optional argument to moog-filter in moog.lisp
          to control saturation.
29-Jun: Clozure OSX changes thanks to Michael Klingbeil.
22-Jan: changes to mus-file-probe et al to get bools working across the FFI,
          and sound-frames and friends now follow the *clm-seach-list* list.
        Jack bugfixes in configure.

-------------------------------- 2010 --------------------------------------

11-Dec: removed midi.c, SGI support.
8-Dec:  removed sc.c and clm-snd.lisp (CLM to Snd interjob communication).
4-Dec:  removed mus-audio-mixer-read, mus-audio-mixer-write, and the associated fields and device names.
5-Nov:  some changes for ACL 8.2.
14-Oct: removed sine-summation, sum-of-sines, and sum-of-cosines.
2-Jul:  default audio in Linux is now ALSA.
17-Mar: many libclm.def fixups thanks to Michael Edwards.

-------------------------------- 2009 --------------------------------------

11-Dec: sine-summation, sum-of-sines, and sum-of-cosines are now just 
          synonyms for nrxysin, nsin, and ncos respectively.
31-Oct: removed sine-bank.
7-July: with-threaded-sound
9-June: mutex locks added for multithreading.
27-May: mus_waveshape retired -- it's now just a wrapper for polyshape.
12-May: expandn thanks to Michael Klingbeil.
30-Apr: changes for clozure cl 1.2.
29-Apr: autoc.ins updated by Bret Battey.
15-Apr: rmsp.ins replaced by track-rms.ins thanks to Michael Edwards.
8-Apr:  polywave uses sine-bank if highest harmonic out of Chebyshev range.
25-Mar: eval-when handling changed to be consistent with CLtL 2.
21-Mar: freeverb bugfix thanks to Oded Ben-Tal.
6-Mar:  "error" called in run loop tries to call lisp "error" (rather than exit)
1-Mar:  mus-name can be set.
26-Feb: removed mus-cosines (use mus-length).
24-Feb: make-env envelopes can be in the form '((0 0) (100 1))
20-Feb: x86-64 and ppc openmcl cases now work.
18-Feb: clm4
        changed the gain calculation in formant
        removed mus-formant-radius (use mus-scaler)
        firmant generator.
17-Feb: polywave generator.
15-Feb: nrxysin and nrxycos generators.
12-Feb: nsin and ncos generators.
7-Feb:  oldmacdonald.lisp.
4-Feb:  mus-sound-maxamp (from clm_sound_maxamp -> sound-maxamp).
        *clm-default-frequency* (0.0, not 440.0)
24-Jan: x86-64 and osx ppc sbcl cases now work.

-------------------------------- 2008 --------------------------------------

15-Oct: moved mus-phase-vocoder-outctr to mus-location.
11-Oct: changed default srate to 44100.
8-Aug:  correlate and autocorrelate.
3-Aug:  blackman5..10 and rv2..4 fft windows.
14-Jul: shepard.ins thanks to Juan Reyes.
        removed useless :start arg from make-env.
19-Jun: mus-increment wherever there's a notion of frequency.
9-May:  various changes for ACL 8.1
14-Feb: Windows/ACL 8.0 changes from Michael Edwards.
31-Jan: added filter-scaler arg to make-convolve.

-------------------------------- 2007 --------------------------------------

26-Sep: moved config.h to mus-config.h.
16-Sep: windows bugfixes thanks to Michael Edwards.
28-Aug: sbcl on MacIntel loader bug fixed by Daniel Stahl.
28-Jul: removed make-ppolar|zpolar (use make-two-pole|zero).
3-July: move-sound generator.
        locsig now returns a float (its input).
        dlocsig dir contents moved to main dir and dir deleted.
        dlocsig.lisp and move-sound.ins now use the new move-sound generator.
        freeverb dir contents moved to main dir and dir deleted (freeverb.ins uses filtered-comb now).
29-Jun: filtered-comb generator.
26-Jun: def-clm-float-struct.
8-Apr:  clisp support.
11-Mar: bandedwg.ins thanks to Juan Reyes.
1-Mar:  mus-location for granulate accesses local random number seed.
27-Feb: numerous sbcl-related bugfixes from Fernando.
17-Feb: mus-file-data-clipped-default -> mus-clipping
        mus-file-prescaler-default -> mus-prescaler
2-Feb:  changed mus-audio-sun-outputs to mus-sun-set-outputs, and
        mus-audio-set-oss-buffers to mus-oss-set-buffers
        added mus-netbsd-set-outputs
31-Jan: loop type decl bugfix for sbcl, thanks to Fernando.

-------------------------------- 2006 --------------------------------------

19-Dec: new fft windows: samaraki and ultraspherical (related to the dolph-chebyshev window).
21-Nov: mus_sample_t is now double if use-float-samples and with-doubles (the defaults).
        *clm-tempfile-data-format* and *clm-tempfile-header-type*.
18-Nov: jcvoi bugfix thanks to Michael Klingbeil.
14-Nov: openmcl fixups thanks to Rick.
8-Nov:  new dlocsig.lisp and move-sound.ins from Fernando.
25-Oct: various changes for openmcl 1.0, sbcl 0.9.5, cmucl 19c, acl 8.0.
        clm.asd thanks to Rick.
20-Oct: fixed various instruments that were using mus-location with src (left-over from clm-2):
          backandforth.ins, scratch in ug1.ins, granular.ins, various test ins.
        added optional srate envelope arg to fullmix.
17-Oct: changes for ACL 8.0.
2-Sep:  changes in makefile.in/configure.ac for 64 bit systems.
2-May:  vowel.ins thanks to Michelle Daniels and Oded Ben-Tal.
8-Apr:  replaced mixer-scale with generic args to mixer*.
        frame*, frame+, mixer*, and mixer+ take either generator or float args.
5-Apr:  run and run* bugfixes.
        fullmix handles some of its arguments differently now.
23-Mar: changed interpretation of frame->frame args.
21-Mar: removed bessel.lisp -- it's very much out of date and should be rewritten to use the polynomial function and libm.
          the libm part of this is in place (bes-j0, etc).
16-Mar: polyshape generator (oscil + polynomial).
        mus-chebyshev-first|second-kind (as "kind" arg to partials->polynomial).
        partials->waveshape no longer normalizes the partials.
14-Mar: sbcl can now handle multi-instrument files, and no longer uses the *.sbcl kludge.
11-Mar: removed ACL 4.n support.
        removed acl4-3.c.
8-Mar:  removed clisp-exec.c.
        added ffi-test.lisp.
7-Mar:  run* support cleaned up.
        ffi stuff cleaned-up.
        removed sound-set-loop-info.
        removed (unusable in clm3 -- I should have done this a year ago!):
          mus-set-header-type, mus-set-data-clipped, mus-prescaler, mus-set-prescaler
          sound-open-input, sound-open-output, close-sound-input, close-sound-output, mus-sound-override-header,
          mus-data-clipped, read-sound, write-sound seek-sound-frame, mus-file-header-type,
          mus-file-set-chans, open-sound-output, reopen-sound-output, mus-file-open-read, mus-sample-bits,
          mus-file-open-write, mus-file-create, mus-file-reopen-write, mus-file-close, mus-file-seek-frame,
          mus-file-read, mus-file-read-chans, mus-file-write-zeros, mus-file-write, mus-file-read-any,
          mus-file-read-file, mus-file-read-buffer, mus-file-write-file, mus-file-write-buffer,
          mus-file-fd-name, mus-file-set-chans, mus-header-write-next-header, mus-header-update-with-fd,
          mus-audio-close, mus-audio-open-input, mus-audio-open-output, mus-audio-read, mus-audio-write.
        deprecated (these are old names -- use the "mus-" style name in new code):
          sound-format-name, sound-type-name, mus-set-raw-header-defaults, describe-audio
          sl-dac, format->bytes, sound-loop-info, initialize-sndlib, read-audio-state,
          write-audio-state, mus-data-format-to-bytes-per-sample, set-osss-buffers
1-Mar:  openmcl sndplay bugfix thanks to Rick Taube.
21-Feb: move-locsig bugfix, thanks to Fernando.
18-Feb: *output* is now the default stream in out-any.
8-Feb:  one-cut.ins (Fernando Lopez-Lezcano)
7-Feb:  mus-reset method.
25-Jan: updated scanned.ins and circular-scanned.ins (thanks Juan Reyes!).
24-Jan: make-readin channel/start confusion bug fixed (thanks Bill Sack!).
10-Jan: removed sndsine.c, fixed sound_loop reference in ffi.lisp
          (thanks to Fernando Lopex-Lezcano).

-------------------------------- 2005 --------------------------------------

27-Oct: ACL 6.2 bugfix thanks to Heinrich Gudenus.
25-Oct: run signum bugfix.
1-Oct:  added scentroid.ins, autoc.ins, rmsenv.ins, and sndwarp.ins, thanks to Bret Battey.
24-Sep: signum in run should accept/return floats.
13-Sep: removed buffer generator.
6-Sep:  removed oscil-bank, mus-bank.
1-Sep:  more configure changes thanks to Michael Klingbeil and Anders Vinjar.
24-Aug: removed mus-inspect.
5-Aug:  changes for sbcl 0.8.13 and cmucl 19a.
19-Jul: clm 3.0!
        removed clisp/no-ffi support: rc.c, rc.lisp, no-ffi and clisp switches.
        removed real-time controls support:
            noffishm.c and all "control" functions
            rt.lisp, bess*.cl, preverb.ins, stochasticRT.ins, revon.ins
        removed old Mac OS/MPW support:
            ToolServer.Lisp, maclib.lisp, make-clm.cl, mac.help, aiff.lisp, mac.lisp
        removed lispworks support.
        removed with-psound/defpinstrument support: vp.ins, rcp.c
        removed obsolete files:
            jl.ins, load-clm.cl, make-clm.cl, lispcall.c
            clm-changes.html, translate.cl (CLM-1 stuff)
            fltdes.lisp, moog.ins (moog.lisp appears to be more recent)
        removed obsolete/redundant functions and variables:
            clm-simple-mix, *input* (and all related function args), rec-any,
            cnv-reverb in cnv.ins
        removed clm-help:
            the help subdirectory and indexing support for it
            clm-help.lisp, menu.lisp, clm-inspect, clm-help, linux-help
            *clm-help-browser* *clm-linux-helper* *clm-helper-choice*
        removed the *explode* feature, and the notion of a lisp instrument
            (definstrument now assumes C -- use "defun" if you want lisp).
            also removed save-signal.
        removed mus-open-write, mus-reopen-write, mus-open-read, mus-create, mus-close.
            These are available under more perspicuous names in clm1.lisp.
            Also mus-file-open-descriptors, mus-write-zeros, mus-write-float.
            Also mus-file-to-array (use file->array), mus-array-to-file (use file->array)
        removed def-clm-struct support for generator fields (CL only)
        removed instrument-let
        open-input and close-input are almost no-ops -- they just save their
            arguments for some future reference (backwards compatibility).
            removed open-output, reopen-output, close-output (you can
            get the same functionality from sndlib's mus-sound-open-ouput).
        added run support for:
            mus-name, mus-file-name, mus-xcoeff, mus-ycoeff, mus-equal
            mus-describe, mus-inspect, polar->rectangular,
            sound-comment, mus-header-type-name, mus-data-format-name
        added "declare" types (run macro): string, mus-any, bignum
            strings and bignums can be passed through run.
        added off_t sample number support.
        added generators: ssb-am, phase-vocoder
        added classes: sample->file, frame->file
        changed pv-* to phase-vocoder-*
        added play and stop-playing (synonyms for dac and stop-dac)
30-Jun: clm.c now included in libclm.so.
        sndlib-use-floats is now always on -- the *feature* has been removed.
15-Jun: config* changes -- CLM now runs configure to set C flags correctly.
        removed good-*.ins until I can get around to fixing them.
14-Jun: ssb-am generator.
        replaced mus-a*|b* with mus-x|ycoeff.
9-June: some Windows-related bugfixes thanks to Michael Edwards.
7-June: removed export of mus-x1|x2|y1|y2.
24-May: distribution arg to make-rand, make-rand-interp.
17-May: delay-tick.
13-May: ACL 7 changes.
10-May: mus-linear renamed mus-interp-linear, mus-sinusoidal renamed mus-interp-sinusoidal.
        Connes and Hann-Poisson windows
        type arg to mus_make_delay|all_pass|comb|notch.
3-May:  envelope arg to make-rand and make-rand-interp to give any arbitrary random number distribution.
26-Apr: slight change to sum-of-cosines calculation (less DC in new form).
        changed sum-of-sines to sine-bank.
        new sum-of-sines parallels sum-of-cosines.
12-Apr: average generator (average, make-average, average?): moving window average.
9-Apr:  implemented start/end args to dac function if sndplay used.
19-Mar: (run (loop while t do...)) no longer invents an end sample.
10-Mar: MCL FFI bugfix thanks to Pierre Klanac.
27-Feb: another openmcl FFI bug (thanks to Juan Pampin).
        clm-swap-ints and clm-swap-doubles
24-Feb: another openmcl FFI bug (thanks to Juan Pampin).
20-Feb: openmcl FFI bugfix thanks to Gary Beyers.
17-Feb: with-mix bugfixes thanks to Fernando.
12-Feb: new strad.ins from Juan Reyes that adds vibrato
        rc.c statistics bugfix (Michael Edwards).
10-Feb: clisp/no-ffi mus-l24int riff bugs thanks to Michael Edwards.
5-Feb:  in clisp, libmus.a no longer rebuilt unless something changed.
27-Jan: Fernando found a way to turn off the idiotic CMUCL blathering.
22-Jan: Linux/Jack support thanks to Fernando.
        openmcl connection to mus_sound_reopen_output was screwed up causing sound-let to complain about a full disk.
19-Jan: openmcl dac improvement from Rick.
12-Jan: -bufsize arg to sndplay for OSX CLM special cases.
5-Jan:  Todd Ingalls got the clm/snd connection to work in OSX/openmcl/X11.
        dur arg to make-env for duration in samples.

-------------------------------- 2004 --------------------------------------

19-Dec: check for empty name in lisp->c-name.
        Bill Sack's stochastic.ins and stochasticRT.ins.
2-Dec:  *definstrument-hook* from Rick.
28-Nov: all.lisp fixups for openmcl thanks to Daniel Starr.
        sbcl seems to work in ffi mode.
18-Nov: bugfix for a cmus (run) macro-related addressing problem pointed out by Bob Coyne.
27-Oct: various ACL 6.2/Windows bugfixes thanks to Kenny Tilton.
22-Oct: fix help directory filenames thanks to Marco Trevisani.
29-Sep: run.lisp loop counter bugfix thanks to Oded Ben-Tal.
2-Sep:  frame->frame mixer addressing bugfix (thanks to Anders Vinjar)
21-Jul: some changes for linuxppc/openmcl.
15-Jul: protect against (log 0.0) -- thanks to Bret Battey.
10-Jul: io.lisp *clm-delete-reverb* bugfix thanks to Michael Scholz.
7-July: removed old-style.*.
        clm-read/write-floats bugfix (thanks to Juan Pampin).
        double-float replaced by double, make-double-float-array by make-double-array.
3-July: removed just-lisp support.
30-Jun: removed long-int support.
        openmcl exception bugfix (thanks to Juan Pampin).
20-Jun: mix (with-mix) bugfix (thanks to Oded Ben-Tal).
13-Jun: noffi.lisp sound-frames can take IO struct arg (thanks to Jon Monroe).
6-June: more error checks to with-mix.
28-May: rt.lisp ported to cmucl by Michael Scholz.
27-May: frame->frame chan check improved (thanks to Michael Klingbeil).
19-May: optional size arg for make-table-lookup, make-wave-train, make-waveshape.
28-Apr: header constants changed to reflect sndlib changes.
18-Apr: some old Mac OS/MPW changes (thanks to Michael Klingbeil).
28-Mar: grani.ins: option for sending grains to all channels (thanks to Fernando)
26-Mar: bugfixes from Rick Taube and Michael Klingbeil.
          the freeverb bugfix may change levels of reverb.
17-Mar: removed -DCLM switches -- sndlib is the same in all cases now.
10-Mar: make-granulate IO start point bugfix thanks to Michael Klingbeil.
4-Mar:  export def-optkey-fun.
28-Feb: some instruments written by David Lowenfels:
          good-saw.ins, good-sqr.ins, good-tri.ins
          and Vellocet-style oversampled state-variable filter: svf.lisp 
27-Feb: mus-width bugfix in mus.lisp. mus-length for env dur in samples.
        power-env bugfixes thanks to Michael Klingbeil.
21-Feb: setf method for sum-of-cosines cosines, run support for mus-x1|x2|y1|y2.
19-Feb: run support for seconds->samples.
17-Feb: tap/delay optimization bugfix (thanks to Cris Ewing).
3-Feb:  mus-data and mus-offset for envs. mus-width for square-wave (suggested by David Lowenfels).
27-Jan: openMCL bugfix thanks to Rick.
21-Jan: moved some constants from sndlib2clm.lisp to initmus.lisp for Clisp.
17-Jan: Date/Time bugfixes thanks to Michael Scholz.
14-Jan: FreeBSD/CmuCL changes thanks to Michael Scholz.

-------------------------------- 2003 --------------------------------------

4-Dec:  cmucl ffi.lisp typo for clm-receive-snd.
1-Dec:  add -nostdin to snd invocation in start-snd.
18-Nov: tried to rationalize the endless ACL *features* switches.
11-Nov: removed clmnet.c|lisp.
8-Nov:  maxf.ins thanks to Juan Reyes.
1-Oct:  various fixups to reflect sndlib changes.
24-Sep: changed rc.c to use mus_sound_maxamps.
20-Sep: strad.ins thanks to Juan Reyes.
21-Aug: bugfix for locsig-reverb-set! thanks to Michael Edwards.
20-Aug: phase-vocoder internal array accesssor names changed.
12-Aug: openmcl ffi support thanks to Michael Klingbeil.
25-Jul: clmnet is now included only if you pushnew :clmnet on *features*
24-Jul: openmcl ffi linkages.
19-Jul: grani.ins improvements from Fernando.
3-Jul:  just-lisp timings in README.clm, san.ins just-lisp fixups.
        DotEmacs thanks to Fernando.
26-Jun: cmucl support in clm1.lisp.
18-Jun: sndlib related bugfixes from Fernando.
29-May: Mac improvments from MK.
23-May: more Mac bugfixes and improvments from Michael Klingbeil.
9-May:  make sure instruments can return a value (tagbody confusion).
        more Mac-related improvements from Michael Klingbeil.
6-May:  clear-array of integer array bugfix (thanks to Bret Battey)
3-May:  more Mac-related improvements thanks to Michael Klingbeil.
2-May:  idiotic changes to make Clisp 2.28 happy.
1-May:  maclib.lisp, many other Mac-related bugfixes thanks to Michael Klingbeil.
        dlocsig reverb bugfix thanks to Fernando.
30-Apr: changed MCL sound-comment (thanks to Bret Battey).
16-Apr: clm.lisp (old clm1.cl) to list of files compiled and loaded by all.lisp.
11-Apr: scanned.ins and circular-scanned.clm thanks to Juan Reyes.
4-Apr:  backandforth.ins cleanup thanks to Anders Vinjar.
3-Apr:  ACL 6.2.
27-Mar: negative degree locsig bugfix.
18-Mar: move-locsig for run-time panning etc (suggested by Juan Reyes).
15-Mar: *clm-locsig-type*, n-chan locsig (and reverb scalers).
6-Mar:  mus-scaler in asymmetric-fm now refers to the "r" parameter, "a" in sine-summation.
5-Mar:  typo in asymmetric-fm gen fixed.
1-Mar:  fir/iir-filter bugfix (thanks to Juan Pampin).
26-Feb: clm1.cl (clm-write-floats et al from clm1).
24-Jan: removed "max-amp" functions -- use "maxamp".
23-Jan: all.lisp changes for ALSA etc.
        removed old-names.cl, old-sndlib.h.

-------------------------------- 2002 --------------------------------------

25-Oct: Mac OSX port (Clisp).
27-Sep: stop-dac implemented for cmucl by Fernando.
24-Sep: changed sound-max-amp to sound-maxamp to match all other such cases (old name still exists)
1-Sep:  polar->rectangular.
29-Aug: reverb-amount to fullmix.
26-Jul: sndlib2clm.lisp updates.
12-Jul: ALSA 0.9 support from Fernando.
16-Jun: clisp 2.26 changes.
15-May: maclib.lisp from John Wiseman.
19-Apr: *clm-safety* type check for setf mus-formant-radius.
17-Apr: removed BeOS support.
31-Mar: extended fullmix to include src.
29-Mar: while/until loop clause checks for outer loop.
22-Mar: *clm-delete-reverb* (default t) to clean up with-sound reverb stream.
        make-convolve now takes its filter-size argument into account.
14-Mar: setf mus-formant-radius bugfix (thanks to Michael Edwards).
26-Feb: libclm.def fixup; many cosmetic changes.
14-Feb: if no output in no-ffi versions, lower portion of datai clobbered! (found by Michael Edwards).
10-Feb: sndlib2clm.lisp (all macro names, etc).
9-Feb:  output_device arg to sl_dac.
30-Jan: bug in CLM->C clm-print number auto-conversion fixed (thanks to Michael Edwards).
        arrayp (was named array? but I forgot to export it).
        bug in numberp fixed (~:^ replaces ~^ in format).
        panning.lsp thanks to Michael Edwards.
        removed (bogus) mus_write calls in cmus.lisp save-signal and clm-print.
29-Jan: port to ACL 6.0 thanks to Ken Wauchope.
26-Jan: freeverb/*: Fernando Lopez Lezcano's translation of Jezar Wakefield's reverberator
25-Jan: mus-bank.
22-Jan: simplified zipper.ins.
        new dlocsig thanks to Fernando Lopez-Lezcano.
16-Jan: *clm-helper-choice* from Marco Trevisani.
        removed Play.java, audio/*, fm.html play buttons.
15-Jan: plotter.lisp improvements thanks to Marco Trevisani.
        typo in run translation of frame-set! fixed.
11-Jan: FFI support in cmucl.
2-Jan:  mus-run and mus-apply.

-------------------------------- 2001 --------------------------------------

13-Dec: plotter.lisp thanks to Michael Edwards.
11-Dec: snd-envelope bug fixed (send-and-receive-snd no longer defaults to calling eval).
8-Dec:  clm1 leftovers removed from fltsnd.ins and ring-modulate.ins (thanks to Oded Ben-Tal).
5-Dec:  fixed bugs in clm->snd connection and mix duration (thanks to Ketty Nez).
9-Nov:  fixed scaled-to -> "no previous file" bug in no-ffi versions.
6-Nov:  clm-test fixups for clisp.
        moved old sndlib names to old-names.cl.
27-Oct: error exit wasn't closing with-sound output!
17-Oct: Todd provided a mac.lisp documentation tie-in for icab.
16-Oct: update libclm.def to reflect recent error handler changes.
12-Oct: formant-bank takes one input (rather than an array).
5-Oct:  cmucl endian bugfix thanks to Marco Trevisani.
26-Sep: removed references to obsolete mus-audio-error.
18-Sep: reflect sndlib name changes (error handling changes).
31-Aug: formant radius/frequency run-time setters changed (thanks to Anders Vinjar).
29-Aug: clmnet.lisp is no longer included by default in MCL -- push :clmnet on *features* to include it.
28-Aug: excl versions of sound-format-name and sound-type-name were confused about their argument (thanks to Anders Vinjar)
25-Aug: mus-sound-duration checks for internal floating-overflow.
17-Aug: translate.cl and other improvements from Anders Vinjar.
14-Aug: sc.c didn't need the XmAll.h header. Port to BeOS in Clisp.
27-Jul: Gustavo A. Delgado found a bug in src which I think is fixed.
17-Jul: sl-dac to access sndlib dac code from lisp as a function (not via sndplay).
        sum-of-sines, phase-vocoder.
5-Jul:  missing sc.o directory in all.lisp (Anders Vinjar).
29-Jun: removed Code Warrior support (cw.lisp etc).
16-Jun: sound-loop-info.
13-Jun: load sequence bugfixes.
29-May: restored explode with-sound support, run-time send-snd.
26-May: removed old clm-to-snd communication stuff (pipe.c replaced by sc.c).
        snd-env and clm-env -> snd-envelope and clm-envelope.
        stop-snd, through-snd removed
10-May: bugfix involving large prescalers.
2-May:  formant-bank.
28-Apr: linux microphone input (with-psound) bugfix(?).
24-Apr: fixed libclm.def acl/windoze bug.
20-Apr: convolve-files.
18-Apr: updated moog.lisp. oscil-bank and rewrote pvoc.ins to use it.
17-Apr: Marco Trevisani translated pvoc.ins to clm-2.
        Fernando Lopez-Lezcano provided a new ALSA port.
11-Apr: new sndlib names.
10-Apr: commented out the explode stuff in with-sound until I can get it re-implemented.
6-Apr:  Marco Trevisani's linux-help and associated *clm-linux-helper*.
        changed clm-src-width to *clm-src-width* and exported it; also the scr gen width
          is now set to (max width (* 2 srate)).
        many minor changes to make clm (mus.lisp) match the scheme/C versions (clm.c)
27-Mar: version 15 of clm: changed sndlib to allow floats as internal sample representation,
           this means version 15 is not backwards compatible with version 14.  Removed
           shift-24-choice (now useless), add :sndlib-use-floats compile-time *feature*.
        Bug in exponential envs fixed (introduced a while ago while trying to get offsets to work).
16-Mar: random in run-loop returns int if arg is int.
14-Mar: changed internal 24-bit chooser names.
9-Mar:  current-real-time for with-psound.
2-Mar:  Ludger Brummer found yet another bug -- this time in setf locsig-reverb-ref.
28-Feb: mus-prescaler, mus-set-prescaler for problematic float files.
24-Feb: filename expansion to sound-srate and others.
        mus-set-raw-header-defaults.
18-Feb: new version of pvoc.ins thanks to Michael Edwards, old is pvoc-original.ins.
15-Feb: setf (mus-location env) fixed.
8-Feb:  (run-time) convolution and buffer-full? at suggestion of Fernando Lopez-Lezcano.
31-Jan: support for /etc/clm.conf.
        fixed statistics bug introduced in December by change for Snd.
        removed phase-partials->waveshape.
27-Jan: env.lisp (clm-1 to clm-2 transition) bugs.
3-Jan:  change to granulate generator suggested by Marc Lehmann.

-------------------------------- 2000 --------------------------------------

9-Dec:  srate and other args to dac in mcl.
3-Dec:  gif -> png.
1-Dec:  dlocsig directory thanks to Fernando Lopez Lezcano.
30-Nov: clm1.html, clm-contents.html.
26-Nov: aiff-sound-file is now distinguished from aifc-sound-file, so mus-set-aifc-header has been removed.
24-Nov: frame->list.
15-Nov: aiff.lisp improvements thanks to Michael Klingbeil and Rick Taube.
18-Oct: env.lisp bugfix thanks to Anders Vinjar.
8-Oct:  use 'locally' rather than 'progn' in definstrument to support declarations.
4-Oct:  port to ACL in LinuxPPC.
29-Sep: a few internal name changes.
20-Sep: MCL/CW improvements thanks to Michael Klingbeil and Rick Taube.
17-Sep: ALSA audio port thanks to Paul Barton-Davis.
15-Sep: delay and tap bugfixes.
13-Sep: CLM-2!  Major changes -- see clm-changes.html.
   clm-changes.html has disappeared.  I'll insert it here as text:

          Why CLM-2? or How CLM Got Its Wrinkly Skin
        
        CLM started out in 1990 as a replacement for CCRMA's PDP-10 based Mus10
        program. At the time we were running on NeXTs where the only hope for
        sound synthesis required use of the 56000. As years went by, CLM slowly
        spread out, becoming fat and sloppy. The 56000 passed away (this
        involved about 25000 lines of code at one time in CLM), but its legacy
        was a bunch of functions and types that were no longer sensible; a
        foreign function interface to C was added and ported to numerous lisps,
        but this too introduced oddities; machines got much faster, making many
        of the cut-corners of the original version look quaint; and many new
        ideas were implemented, each grafted in wherever it could fit. For
        nearly 10 years, I was so busy with new features, new ports, new
        programs, that I never really noticed what was happening.
        
        Around the end of May, 1999, I reached a major "inflection point" in the
        Snd editor, and started thinking about using CLM and Snd together. This
        led to clm.c, clm.h, and clm2xen.c, a reimplementation of CLM in C and
        Scheme, loadable into Snd (or any C program). At first, I intended to
        reimplement CLM without changes, but the more I typed, the less happy I
        became. I finally wrote out a list of the CLM flaws that came
        immediately to mind:
        
            * *Run-time types are limited and poorly implemented*. Strings are
              only half-implemented, lists, double floats, and complex numbers
              aren't implemented, and even integers are often turned into floats
              for no good reason. Due to the lack of run-time typing, you can't
              implement generic operations on generators, and worse, the run
              macro often can't tell what type to assign something. And the lisp
              foreign function implementations are a complete nightmare in this
              regard; all the short-float junk in CLM is an attempt to kludge
              around this.
        
            * *Generic functions make struct accessors useless*. All the struct
              accessors assume a particular implementation (which should be
              hidden), and to use them requires the user to know details of that
              implementation (whether a given field is in radians, and so on).
              Now that the DSP is gone, and machines are faster, many of these
              structs don't serve any purpose (frmnt, for example).
        
            * *Inconsistent and bad names*. default-table-size, for example,
              doesn't fit with most other naming conventions in CLM. clear-block
              never actually referred to blocks, and make-block really meant
              make-run-block; I can't remember now why dly-pline is not
              dly-line; in-hz (or frequency-mag) should be hz->radians; etc.
        
            * *No way to debug an instrument*. If the poor user gets an error
              from an instrument (especially a segfault or some other C error),
              or gets incorrect (undesired) results, he's not going to get much
              help from CLM; the entire run-time world is largely opaque. This
              ultimately is the fault of Lisp -- it is so slow at numerical
              computations that I was forced to write translators; then the
              foreign function interfaces of Lisp are so kludgey and broken,
              that if anything goes wrong, the lisp debuggers are useless, and
              gdb in this context is so cumbersome to set up as to be unusable.
        
            * *Generator input opaque*. All the generators that take input as
              needed (src for example) either assume they're reading a file
              (using readin), or they use run-block; run-block's triggering
              mechanism is completely impossible to understand, and a mess to
              program; the code should be something like: (src gen fm #'(lambda
              (x) (oscil os))) to get input as needed from an oscillator (this
              is much prettier in scheme where it's natural to pass closures
              around).
        
            * *Output opaque*. It should be possible to re-direct generators
              that normally do file output to write elsewhere; locsig, for
              example, should be able to write to an array in an instrument.
        
            * *CLM can't easily be included in other contexts*. This was
              actually the starting point of all the changes -- I wanted the CLM
              generators loadable into Snd, but lisp implementations assume lisp
              is the "main program" -- there is no support for a lisp program as
              a module loadable into a C program. Pipe-oriented work-arounds
              could not be tied tightly into Snd's Scheme world; for example,
              say the Snd user wanted to use CLM's env generator; his only
              option would be to reimplement the entire generator when both lisp
              and C implementations were already available, but not loadable. It
              should also be possible to use CLM instruments and generators
              without making any assumptions about file IO or memory allocation.
              For example, within Snd, a CLM instrument might get its "file"
              input from the Snd edit trees, and write its "file" output to a
              Snd graph, with no underlying file.
        
            * *File input too complicated*. The readin generator is hard to
              control and understand; in-any assumes file IO. Since each make-
              function has to know about input choices, arguments proliferate;
              then there are ridiculous synonyms like "start" and
              "input-file-start" -- I think I was being too accommodating.
        
            * *Recursion unimplemented*. The DSP, array-processing orientation
              of the original CLM was so pervasive that no recursion is possible
              within the run loop.
        
            * *Useless functions*. I doubt that anyone has ever found a run-time
              use for gcd or logeqv. fft-filter is a useless optimization of
              convolve. lattice and ladder filters were a mistake; the Hartley
              transform (fht) is a pointless micro-optimization. There are lots
              more of these; they need to be moved to some auxiliary file.
        
            * *Useless arguments*. Many of the functions take either synonymous
              arguments (make-filter has as many as 3 names for the same
              thing!), or silly arguments (all the phase-error-p arguments).
              These need to be weeded out.
        
            * *Useless structures and types*. Formnt, for example, or zdly,
              cmbflt, allpassflt; all are mistakes; blocks and tables -- these
              should just be arrays.
        
            * *Phrasing opaque.* The make-phrase mechanism was developed for
              multi-DSP hardware, and makes little or no sense anywhere else.
              But there's no reasonable way to do phrase-level stuff in CLM (I
              added windowed-env, but it's only a first step).
        
            * *Inter-instrument communication opaque*. The def-clm-var business
              and run* are unfortunate kludges. One instrument should be able to
              pass-off control to another, so that the user has some hope of
              knowing what order things happen in.
        
            * *Instruments should be callable as generators*. Sound-let is a
              kludgey way to get around this. Instrument-let is also a kludge --
              it should be possible to conjure up instruments on the fly, and
              embed them in other instruments, especially to serve as
              "as-needed" input for src and its friends.
        
            * *Real-time controls and parallel processing*. These were late
              add-ons that have never been fully implemented, due mainly to lack
              of time.
        
            * *Name collisions*. If CLM is to be consistent across C, Scheme,
              and Common lisp implementations, and loadable into other arbitrary
              contexts, various names need to be changed. set-srate, for
              example, collides with Snd's srate variable. Even within lisp, I
              was using angle in place of phase. 
        
        This was a start! It convinced me that enough needed fixing that a
        simple reimplementation was, aesthestically speaking, not an option.
        Once I decided to fix the damned thing, the floodgates opened; the rest
        of this document describes the changes in detail. The pure C version is
        clm.c (clm.h); the Scheme version is clm2xen.c.
        
        
          CLM Gets a Facelift
        
        
            Major Changes
        
        These are the changes that will probably affect the most CLM users.
        
            * ACL 3.1.20 is no longer supported, nor any version of GCL; CLM
              assumes it is running in a lisp with row-major-aref (added in
              cltl2), and CLOS.
            * make-env needs an explicit duration indication (the previous form
              tried to infer the envelope duration, but that could only work if
              a loop statement was the outer statement in the run macro, and if
              the envelope was then called on every sample).
            * snd-duration (and friends) => sound-duration (this is the sndlib
              name propagated to Snd and the Scheme version of CLM; also I'm
              trying to keep "snd-" for Snd-related functions.
            * a breakpoint list is now called an "envelope", whereas the result
              of make-env is called an "env"; this name change affects all the
              functions in env.lisp.
            * fasmix and the related dax functions removed -- the mix function
              no longer tries to handle filtering, sampling rate conversion, etc.
            * The C side of CLM now uses doubles, not floats, so the lisp side
              use double-float in place of make-short-float; and
              make-double-float-array for make-short-float-array.
            * randh => rand, randi => rand-interp.
            * zdelay => delay.
            * make-table => make-double-float-array.
            * run-block => buffer->sample.
            * expand => granulate.
            * sampling-rate => *srate*.
            * clear-block => clear-array.
            * fft-filter => convolve. (FFT-filter was really just an
              optimization of an FIR filter, and never worked the way anyone
              expected when a time varying filter was desired).
            * resample => src. (Resample used linear interpolation because there
              was a time, long long ago, when computers were so slow that "real"
              sampling rate conversion was only a last resort).
            * zpolar and ppolar => two-zero and two-pole.
            * start-time keyword removed -- use /start/.
            * file and run-block args completely changed; in make-src,
              make-granulate, and make-convolve, the input-related arguments are
              all different.
            * revin removed (use ina with *reverb*). 
        
        
            Names Changed
        
        In Sndlib, I added the prefix "SNDLIB_" to most of the macro names (to
        avoid name conflicts with other libraries), and "mus_" to most of the
        function names, replacing either "clm_" or "c_snd_". The "mus" prefix is
        also used in the Scheme version, so wherever something is not specific
        to clm or is considered a "generic function", it starts with "mus-".
        Wherever the C name has "_", the corresponding Lisp name has "-".
        Similarly, I'm using "->" for a conversion of some sort in Lisp, "2" in
        C; and "?" for a type check in Lisp becomes "_p" in C. As to variables,
        where possible, the default value of 'var' is *clm-var* and the current
        value (dependent on with-sound arguments and so on) is *var*. For
        example, the default number of output channels is *clm-channels*, and
        its current value is *channels*. Most of the name changes can be handled
        by the translate function in translate.cl.
        
            * angle => mus-phase
            * frequency => mus-frequency
            * size-of => mus-length
            * data => mus-data
            * read-position => mus-location
            * read-direction => mus-increment
            * channel => mus-channel, channels => mus-channels
            * make-block => make-buffer
            * run-block => buffer->sample
            * clear-block => clear-array
            * default-table-size => *clm-table-size*
            * internal compiler temporary DSP-* names => CLM-*.
            * randh => rand, randi => rand-interp (and make- forms)
            * sampling-rate => *srate* (default value is *clm-srate*)
            * file-buffer-size => *clm-file-buffer-size*
            * default-sound-file-type => *clm-header-type*
            * default-sound-file-data-format => *clm-data-format*
            * default-sound-file-name => *clm-file-name*
            * *sound-player* => *clm-player*
            * *clm-default-table-size* => *clm-table-size*
            * *current-instrument-name* => *clm-ins*
            * *clm-default-channels* => *clm-channels*, *clm-default-srate* =>
              *clm-srate*, *clm-default-play* *clm-play*
            * load-synthesis-table => partials->wave,
              load-synthesis-table-with-phases => phase-partials->wave
            * make-waveshape-table => partials->waveshape, make-phase-quad-table
              => phase-partials->waveshape
            * get-chebychev-coefficients => partials->polynomial
            * formant-radius => mus-formant-radius
            * channels are always integers, numbered from 0 -- the :A for 0
              stuff has been removed.
            * get-beg-end => times->samples. Also seconds->samples. I'm trying
              to avoid the word "get".
            * describe-audio-state => describe-audio, describe-ins-state =>
              describe-instrument
            * c-srand => mus-set-rand-seed
            * *current-output-file* => *output*, *current-input-file* => *input*
            * divseg => stretch-envelope -- every "env" function that actually
              referred to a list of breakpoints, now uses "envelope". I'm using
              "env" only to refer to the result of make-env.
            * snd-duration => sound-duration, snd-srate => sound-srate,
              snd-chans => sound-chans, snd-samples => sound-samples, snd-frames
              =>sound-frames, snd-max-amp => sound-max-amp
            * list-interp => env-interp (if arg is seg struct) and
              envelope-interp (if arg is list)
            * clm-open-input* => open-input*
            * fft-magnitude => rectangular->polar (fft-magnitude seemed ambiguous)
            * fft-window => multiply-arrays (to avoid name collisions with Snd
              -- actually a toss-up which program should change, but CLM-2 is
              clearly not trying to be backwards compatible, so it has to give
              way).
            * expand => granulate ('expand' came from mixer.sai, written around
              1986 for "Leviathan"; in CLM, it did not really describe the
              generator; when it collided with Snd's expand function, away it
              went).
            * *clm-file-search-list* => *clm-search-list*
            * clm-random => centered-random
            * make-short-float => double-float, make-short-float-array =>
              make-double-float-array
            * make-sound-file => array->file; also the parallel file->array added.
            * design-fir-from-env => envelope->fir
            * c-open-input-file => mus-open-read, c-open-output-file =>
              mus-open-write, c-create-file => mus-create, c-close => mus-close 
        
        
            Functions and Macros Removed
        
            * all struct accessors. Here are some notes on translation.
                  o *-freq => mus-frequency (the latter is in Hz, not radians)
                  o *-phase => mus-phase (except zdly-phase)
                  o dly-pline (line), tbl-table => mus-data
                  o dly-size, tbl-table_size, flt-m (order) => mus-length
                  o cosp-cosines => mus-cosines
                  o sr-incr => mus-increment
                  o sw-base, cosp-scaler, noi-base => mus-scaler
                  o smpflt-a0 => mus-a0 (similarly for -a1, -b1 , -b2) -- these
                    are now mus-xcoeff and mus-ycoeff
                  o frmnt-tz, frmnt-tp => no-op
                  o frmnt-g => mus-a0
                  o cmbflt-scaler => mus-scaler (also a0 for comb, b1 for notch)
                  o allpassflt-feedback => mus-feedback, feedforward =>
                    mus-feedforward
                  o locs-outn => locsig-ref, locs-revn => locsig-reverb-ref
                  o rblk-buf => mus-data, rblk-siz => mus-length
                  o spd-out-spd => mus-hop, spd-in-spd => mus-increment, spd-len
                    => mus-length, spd-rmp => mus-ramp, spd-amp => mus-scaler. 
            * all struct make-* and *-p functions like make-osc and osc-p (use
              the new ? functions like oscil?)
            * make-formnt, formnt (redundant -- now spelled 'formant')
            * ur-table-lookup
            * lattice-filter, make-lattice-filter, ladder-filter,
              make-ladder-filter
            * all byte ops, gcd, lcm.
            * fft-filter and fftflt (use 'convolve')
            * make-direct-filter, direct-filter (use 'filter')
            * fht (use 'fft')
            * locs-ascl and friends (use locsig-ref)
            * lpc-in, lpc-out, lpc-analyze, lpc-synthesize, etc (all lpc funcs)
            * in-a in-b in-c in-d (use 'ina' or 'in-any')
            * resample, make-resample
            * ppolar, zpolar (use two-pole and two-zero) -- make-ppolar and
              make-zpolar remain
            * make-table (use make-double-float-array)
            * readin-reverse and make-reverse (use readin with dir = -1)
            * read-forward, read-backward (use setf with mus-increment)
            * end-run, phrase, wait-for-phrase, phrase-value, make-phrase
            * snd-bytes, all the snd-header accessors (use sound-*)
            * inverse-fft (sign arg added to fft)
            * mono, stereo, quad -- use mus-channels.
            * sms instrument (sms.lisp) -- was obsolete years ago
            * clm-read-floats/ints/bytes and swapped versions, and seeks
            * divenv (use stretch-envelope or list)
            * mix-in-place, fasmix (use mix or the fullmix instrument)
            * mix-in (use open-input)
            * the snd-info data base (*clm-snd-info*, save-snd-info,
              load-snd-info, auto-save-snd-info, snd-info, snd-maxamp) (use
              sound-*)
            * access-env (use env-interp)
            * eref (use env-interp)
            * snd-header struct and all its fields; all header-related
              functions: evaluate-header (use (eval (read-from-string
              (sound-comment file-name)))), make-header, read-header,
              write-header, update-header, edit-header, update-header-comment,
              make-default-header (use sound-*). Also, all the low-level header
              stuff and snd-default-header like c-snd-header-datum-size (there
              were about 20 of these).
            * describe-ins-vars, describe-clm-var, clm-step, describe-snd
            * revout (use outa with *reverb*), revin (use ina with *reverb*)
            * clm-reopen-output (use reopen-output), clm-open-input,
              clm-open-output (use open-input).
            * def-clm-var and all its friends (this stuff never worked right --
              use control instead).
            * set-srate
            * y-or-n-p, yes-or-no-p within run
            * funcall and break within run
            * array-adjustable-p, array-rank-limit, array-dimension-limit within
              run
            * record, cd-record, describe-pool
            * make-filter-coefficients, make-fir-coeffs
            * defcinstrument (use definstrument)
            * in rc (no-ffi versions) outa and ina (in the with-sound body, not
              the run body) are no longer implemented
            * with-trace, with-verbosity, with-display (see save-signal),
              with-explode, etc 
        
        
            Names Added
        
            * mus-scaler, mus-a0, mus-a1, mus-a2, mus-b1, mus-b2, mus-increment
            * mus-order, mus-xcoeffs and mus-ycoeffs for filters
            * mus-ramp and mus-hop for granulate
            * hz->radians, radians->hz, degrees->radians, radians->degrees
              (in-hz still exists)
            * linear->db, db->linear
            * make-iir-filter, iir-filter
            * integerp, floatp, numberp, realp (in run)
            * frame data type added (represents multi-channel data at a given loc)
            * mixer data type added (represents a mixing console, effectively)
            * sample->buffer, buffer->frame, frame->buffer, frame?, make-frame,
              buffer?
            * file->sample, file->frame, sample->file, frame->file.
            * file->array, array->file.
            * mus-input?, mus-output?
            * sound-duration, sound-chans, sound-comment, sound-data-format,
              sound-data-location, sound-datum-size, sound-format-name,
              sound-header-type, sound-length, sound-samples, sound-frames,
              sound-srate, sound-type-name, sound-type-specifier, sound-max-amp
            * *clm-clipped*
            * print-object; print also can take an instance as an argument,
              printing out the various fields -- this should greatly improve the
              debugging situation.
            * snd-memo, add-region. 
        
        
            Arguments Removed
        
            * All the phase-error-p arguments to various make-<gen> functions.
            * All the fortran synonyms in make-filter and friends, and the type
              argument
            * wave-table to make-table-lookup
            * start-time to make-env (use either duration or start and end as a
              pair)
            * revscale to make-locsig (use reverb)
            * increment to make-rand
            * start-time input-file-start-time input-file-start to make-readin,
              make-convolve, make-granulate, make-src. The start-time argument
              was removed because it has caused confusion over which "time" is
              intended when the input file has a different sampling rate than
              the current output file. The only "input file start point"
              argument is now "start" in frames.
            * file, channel, and size to make-src. The file, channel, and start
              location are now wrapped up in an explicit function call that
              makes an input process (make-readin for example). The size
              argument referred to run-block, also now brought out to the top
              level.
            * file, channel, size, start, block-length args to make-granulate.
            * file, channel, size, start, filter args to convolve.
            * endhook, sndfile, type, player-options, save-stats, trace,
              display, break, marks (see add-mark) to with-sound and clm-load
            * player-options to init-with-sound
            * all args except the filename to dac
            * The "ignored" argument (2nd arg) to make-controller.
            * The "end" argument to add-mark, chan arg added, add-mark,
              mark-sample, marks callable within run. *clm-mark* is now just t
              or nil.
            * reverb-func and reverb-args to clm-load 
        
        
            Arguments Added
        
            * initial-contents and initial-element to make-comb, make-notch,
              make-all-pass
            * pm arg to delay, tap, notch, comb, and all-pass (replacing zdelay
              and friends)
            * max-size arg to make-delay and friends -- if /max-size/ is greater
              than /size/, zdelay behavior is accessible. make-delay now needs
              to give the max delay size explicitly (it used to try to choose
              some plausible default length).
            * direction arg to make-readin
            * input to make-src, make-granulate, and make-convolve,
              input-function to src, granulate, and convolve 
        
        
            Arguments Changed
        
            * make-formant g => gain, r => radius
            * make-fir-filter coeffs => xcoeffs
            * declare type names are changed (integer, float, bignum, fixnum,
              single-float, short-float, double-float, and perhaps complex later).
            * table argument to partials->wave is optional
            * b-ratio to make-sine-summation => ratio.
            * all make-granulate and make-convolve arguments re-named.
            * in make-env, duration is no longer optional, and is now the 3rd
              optional-key argument.
            * open-output args now specify the header info, also open-output and
              open-input are def-optkey-funs now.
            * play-options to with-sound => player-options
            * accuracy to make-granulate is now a multiplier, not a divider, and
              is called "jitter".
            * in 'declare' in run, the only types recognized now are integer and
              float.
            * type argument to with-psound and with-dac is now data-format
            * type argument to wsm, with-sound, and clm-load is now header-type 
        
        
            Type Changes
        
            * no struct-specific stuff is exported, not even the struct names
              (so make-array and def-clm-struct should not use them). What were
              structs are now CLOS classes.
            * flt completely changed -- no fields are the same. There are no
              filter types now. make-filter creates a full direct form filter;
              use make-fir-filter and make-iir-filter for other forms. lattice
              and ladder filters have been removed.
            * dly has several new fields.
            * cmbflt, allpassflt, zdly, frmnt, and smp removed.
            * envelope => seg (envelope now refers to a list of breakpoints)
            * IO struct hdr field removed (as well as a few others), siz => frames
            * In the C code produced by the run macro, everything uses doubles
              now rather than floats.
            * sound-files-in-directory now returns a list rather than an array
              (of filenames). 
        
        
            Variables Removed
        
            * frequency-mag, three-half-pi, one-pi, half-pi
            * c-version-number, *c-version-date*, *fix-header-errors*,
              *ignore-header-errors*, *dac-worries*
            * *clm-beg-offset* (use with-offset or clm::*offset*)
            * *clm-eat-raw-sound*, *clm-muffle-warnings*
            * *trace*, *explode*, *break*, *break-fun*, *trace*, *display*
            * *clm-play-options* 
        
        
            Variables Changed
        
            * *clm-version* is now an integer, not a string. 
        
        
            Files Removed
        
            * headers.lisp, unix.lisp, globals.lisp, list.lisp, dacs.lisp
            * dacs.c, unix.c, merge.c, fft.c, mat.c
            * sms.lisp
            * lpc.h, lpc.c, lpc.ins
            * cd.c, cd.lisp
            * struct.ins, fusion.ins, pfas.ins
            * vibstr.cl
            * clm-tutorial files (needs rewrite) 
        
        
            Files Added
        
            * clm-changes.html
            * translate.cl
            * clm.c, clm.h, clm2xen.c, sndlib-strings.h, sndlib2xen.c,
              old-sndlib.h, vct.c, vct.h.
            * cmus.lisp replaces cmus0.lisp and cmus1.lisp
            * fullmix.ins
            * mcl-doubles.cl 
        
        
            Ports Removed
        
            * NeXT, all versions (ACL and GCL in this case were cltl1).
            * Any cltl1 version (some Clisp, all GCL, ACL 3.1.20) (CLM-2 needs
              row-major-aref). 
        
        
            Unfinished business
        
        Ideally, the frame data type could be completely embedded in CLM,
        allowing multi-channel operations to take place in any generator.
        Currently, for example, if an instrument wants to handle multi-channel
        input, it typically sets up an array of readins, or has elaborate
        run-time checks for the channel number, then does each channel in
        parallel. It would be nicer to use constructs like (* amp (oscil osc
        (src sr (file->frame file loc)))) where each operation in the chain
        knows how to handle the frame data returned by file->frame. In the
        "just-lisp" case, this would require that common lisp allow us to
        specialize the "*" operator, for example, which is not currently
        possible (it is possible in Scheme).
        
        There are still some less-than-ideal names floating around. I'd like to
        refer only to arrays, not tables and buffers, but the name
        "table-lookup" is too embedded in computer music to remove. And "readin"
        is not a good name. hz->radians is slightly inaccurate (it's actually
        hz->radians/frame which is too hard to type).
        
        Some of the special case generic functions are an eyesore -- mus-a0 and
        friends; users of the simple filters need access to these fields for
        time-varying filtering, but it's not obvious to me what the field names
        should be. And mus-formant-radius bothers me -- perhaps it should be
        formant-radius (I was thinking I might extend it to other generators,
        but that now looks unlikely).
        
        The "~A" format directive should call print-object when appropriate, but
        doesn't.
        
        
          CLM Visits Foreign Lands
        
        Besides its original Common Lisp version, CLM now exists as a pure C
        module (clm.c, clm.h) loadable into any C or Lisp program, and as a
        Guile/Scheme module (clm2xen.c), usable in any C program that includes
        Guile (Snd is the obvious target). See extsnd.html
        <extsnd.html#sndwithclm> and sndlib.html <sndlib.html#music5> for some
        brief examples and discussion; this is an area that I expect to work on
        in the near future.
        
   end of clm-changes.html

31-May: yet more effort toward Harlequin Lispworks port.
24-May: I finally thought of a perspicuous name for "in-hz": hz->radians.  radians->hz.
14-Apr: windowed-env to env.lisp
13-Apr: changed (improved!) convolution-reverb instrument (cnv.ins).
9-Apr:  header changes for 30-Mar version mistakenly omitted jv_exp messing up exponential envelopes.
6-Apr:  snd-region.
30-Mar: incarray typecast bugfix.
22-Mar: changes to support ACL-Lite in Windoze.
16-Mar: pvoc.ins, a phase vocoder implementation, thanks to Michael Klingbeil.
8-Mar:  real-time output change to accomodate OSS (Linux) AudioPCI bug.
26-Feb: array of arrays run loop bugfixes.
12-Feb: Mac fasmix bugfix.  C++ fixups.  set-aifc-header.
8-Feb:  linux rec-any bugfix.
1-Feb:  more sun stuff, nist-sphere headers are writable.
27-Jan: Sun Clisp port.
22-Jan: Sun-related word alignment bugfixes.
18-Jan: file-based convolution for Macs with little memory.
14-Jan: MPW Mac port thanks to Daniel Stahl.
11-Jan: c-io.lisp renamed ffi.lisp, lisp-io.lisp to noffi.lisp.
6-Jan:  port to ACL 5.0 in Windows using MS C.
1-Jan:  attempt to get CMU-CL FFI to work; CMU-CL no-ffi linux port.

-------------------------------- 1999 --------------------------------------

28-Dec: removed with-scaling macro, with-current-sound, scaled-to, and scaled-by.
        ACL 5.0 Windoze no-ffi port.  added this change log.
        moog.lisp, filter-noise.ins, and grani.ins thanks to Fernando Lopez-Lezcano.
17-Dec: shift-24-choice for 24-bit IO.
11-Dec: restart-clm for saved image restarts (see new appendix in clm.html).
8-Dec:  mac improvements from Rick Taube. Aiff/Riff extensions.
3-Dec:  acl5 sgi shared object bugfix?
30-Nov: sonorus studi/o support.
23-Nov: removed 16-chan limit in fasmix.
19-Nov: src/resample read-direction/position bugfixes.
16-Nov: run null/not bugfix.
13-Nov: linux audio work, mix filter coeffs cdata-size bug, a few more windoze fixups.
2-Nov:  bugfix in src involving negative srates.
26-Oct: c-cleanup-clm-file-descriptors to try to handle disk-full scaled-to error case.
20-Oct: minor bugfixes (stay in sync with snd), new (annoying) clisp changes.
6-Oct:  various linux-audio bugfixes.
27-Sep: with-mix and sound-let implemented in no-ffi cases. Added :ins-args to definstrument properties.
24-Sep: clisp/gcl/cmucl clm/Snd interface implemented.
21-Sep: asf headers. Merged mac.help into README.clm.
17-Sep: gcl-related cleanups.  describe-audio-state and set-oss-buffers to linux/no-ffi version.
1-Sep:  mkLinux port.
28-Aug: minor bugfixes in wave (RIFF) header format.
24-Aug: set-env-base for compatibility with Snd envelopes.
18-Aug: changed snd/clm fifo names and other Snd-related bugs.
17-Aug: snd-env and clm-env for clm-snd envelope handling.
10-Aug: snd-clm changes.  control-allocate and control-free for real-time
        shared memory allocation.  2nd arg to make-controller now ignored.
29-Jul: bugfix involving outa when using expression for sample number.
22-Jul: *.marks and *.explode clm/snd files now omit the intermediate extension.
        many snd-related changes (interface completely rewritten to use Guile in Snd).
        serious bug in readin forced a new dsp-version (12).
25-Jun: DEC Alpha port.
16-Jun: rc bugfix courtesy of Leigh Smith.
15-Jun: long/double support in run. Fixed(?!) dac mac bug.
12-Jun: bugs in run-time prog1 handling and non-sequential locsig output fixed.
9-June: optional channel argument added to revin and revout.
2-June: removed rh5aclbugfix switch (obsolete). Multi-card support in linux (quad via 2 cards).
26-May: wsm syntax changed slightly (helpers arg is now evaluated).
22-May: MCL multi-host support.
21-May: removed aclpc support, case sensitive lower and FFI declaration changes for ACL 5.0.
19-May: ACL multi-host support (clmnet.lisp/c): wsm and be-helpful.
5-May:  prettified struct printout, negative srates work in src (new example ins in clm.html).
27-Apr: menu.lisp for MCL CLM menu.
23-Apr: sndlib changes. RedHat 5 ACL loader bug workaround.
15-Apr: Perry Cook's maraca instrument.  Mac changes (version change).
8-Apr:  long-int-pointer support.
1-Apr:  def-clm-var implementation completely revised.
30-Mar: sndplay is now assumed to be available in many cases (as opposed to sfplay, audioplay, etc).
        dac on the next is no longer limited to Next 16-bit files.
        env calc in C changed to increase precision (changed c-version-number, so instruments must be recompiled).
23-Mar: sound.c, sndlib.h, sndlib.html. cmus_prototypes.h and sound_types.h removed.
16-Mar: with-scaling, scaled-by arg to with-sound. convolve overflow problem fixed (internal normalization added).
10-Mar: more audio.c changes, mac-related bugfixes from Rick Taube. exp-env and friends thanks to Fernando Lopez-Lezcano.
26-Feb: Windows 95/Clisp support.  dacs.c rewritten as audio.c.
        wait-for-dac, jrdac, dac-is-in-use, dax, mic-volume, line-volume removed.
        dac-n -> times arg to dac.  volume and dac functions changed. play.c.
        lpc.lisp/c/h merged into cmus.c and others.  lpc.ins now has the examples.
15-Feb: bug in snd-insert fixed.
5-Feb:  8 new header formats.
1-Feb:  clmnet.c/lisp (multi-host with-sound support).  prc96.ins.  dacs.c new SGI AL support.
26-Jan: Butterworth filters (butterworth.cl/html) thanks to Sam Heisz.
23-Jan: balance.html and balance.cl courtesy of Sam Heisz.
21-Jan: CSRE adf header support. 
19-Jan: amplitude fixup to new src.  MCL bugfixes courtesy of Rick Taube.
15-Jan: sr-filt field removed, src gen rewritten to use Perry Cook's 'warped' sinc interpolation.
12-Jan: with-psound bufsize arg.
1-Jan:  formnt renamed formant, formnt-R -> formant-radius.

-------------------------------- 1998 --------------------------------------

23-Dec: loop point readers for AIFF files.
22-Dec: formnt-R for run-time pole-radius changes.
18-Dec: changes for new linux OSS. Better Sun support.
10-Dec: *features* related changes for acl 4.3.1.  acl.cl.
4-Dec:  sndclm.h to write clm/snd fifos to /zap at ccrma.
25-Nov: clipped argument to with-sound.
27-Oct: compilerless clm on Mac. dac-pause from Marco Trevisani.
20-Oct: -32 switch for sgi, removed -mips2.  Improved run-loop type checking. new cw12 support on mac from Rick Taube.
8-Oct:  initial-phase bounds check is now dependent on *clm-initial-phase-bounds-check* with phase-error-p args added.
        CyberDog added to Mac help choice (*clm-help-browser*) courtesy Marco Trevisani.
        expand generator changed slightly to fix trailing garbage bug.
25-Sep: initial multi-host-related changes.  PPC C switch changed to MACOS (to distinguish from BEOS).
22-Sep: attempted workaround for Linux/ACL def-clm-var linker bug.
1-Sep:  RIFF scaled-to fixed.
14-Aug: smpflt simplified. power-env added to env.lisp.
11-Aug: more Linux-related stuff (cd.c, dacs.c).
6-Aug:  dax and record in Linux.  'q' can be used now to stop (dac) or (record) in Linux/SGI.
4-Aug:  instrument-let in Linux.
21-Jul: acl mp process support for snd to clm communication.
14-Jul: real-time (and midi) control support in rt.lisp, cmus.c via control function, with-dac, etc. 
        Removed no-rld switch. GCL on SGI now only no-ffi -- removed gcl-clm.lisp, libm.
        Removed clm-status, status.c (these are replaced by the realtime readback stuff).
        no-ffi mode no longer fusses about short-floats. various realtime examples: bess.cl and friends.  
        New c-version-number so all instruments need to be recompiled (many function prototypes changed).
        end-run call removed from instruments where it was handling delay deallocation (56000 related, so obsolete).  
        Spectrum function arguments changed to give linear results if desired.  anoi.ins added.
        Line choice added to open_adc for line-input (digital or analog) in with-psound and with-dac.
        with-psound and defpinstrument now work in no-ffi implementations.
        :ins-vars property is now a list, not a hash table (affects :print-function in definstrument)
        run safety >= 1 adds array index bounds checks
18-Jun: explode changes for Snd. MCL bug fixes.
16-May: gcl/next/no-ffi nits.
5-May:  explode (*clm-explode*) in with-sound implemented.
25-Apr: more MCL changes. run-time locsig arrays implemented.
17-Apr: MCL heap array bugs fixed.
14-Apr: MCL 4.1, run-time display of arrays.
24-Mar: ACL 4.3 run-loop funcall improved.
20-Mar: DiamondWare headers.
11-Mar: fp underflow code added for SGI. rt.lisp linux support (still gets clicks).
        support added for Gravis Ultrasound patch files, Sonic Resource Foundry headers,
           Comdisco SPW headers, and Goldwave sample files. init-with-sound for CM updated.
1-Mar:  through-snd, c-srand and c-random extended to all cases (lisp-io.lisp).
25-Feb: c-random (for clm-example.lisp in acl 4.3).
14-Feb: new instrument output names (see *clm-combo*).
10-Feb: netscape support on Mac courtesy of Rick Taube.
5-Feb:  rec-any for real-time input from microphone. removed :voxware switch.
4-Feb:  Play.java applet for clm.html and fm.html, audio subdirectory for it.
3-Feb:  *rc-directory*. zipper.ins. fade.ins. formnt optimized (now a function).
27-Jan: help directory and clm-help function.  Removed dac-filter.
17-Jan: unlinked c-close bug fixed.
13-Jan: swapped.cl folded into io.c and c-io.lisp.
9-Jan:  ACL 4.3 on Linux.
7-Jan:  exp-env in env.lisp by Anders Vinjar.  grapheq.ins by Marco Trevisani.
6-Jan:  wavelets.cl, ins-var, :print-function arg to definstrument.
1-Jan:  Gcl + NextStep now runs only :no-ffi (no libmus.a, no libm).
        clm-snd debugging and display improved (:display, :trace, :break in with-sound).
        *clm-break-fun*, debugdata.

-------------------------------- 1997 --------------------------------------

23-Dec: SoundFont headers.
17-Dec: to-snd macro. Just-lisp RIFF output. Removed clm-total-duration stat. 
        clm-get-default-header -> snd-default-header, clm-get-max-amp -> snd-max-amp.
16-Dec: snd-test.cl.  CLM-Snd snd-sound and others (see new appendix in clm.html).
6-Dec:  dlocsig.html courtesy Anders Vinjar. Linux dacs (and other bugs) fixed.
21-Nov: better support for non-standard channel numbers. make-sound-file. display and undisplay for snd.
19-Nov: removed sgi-walk.lisp, runpcl.cl. clm-get-duration removed (use snd-duration).
        clm-step and :marks in with-sound (for auto-marks in Snd).
12-Nov: removed *clm-history*. matrix arguments in mix/fasmix were backwards.
7-Nov:  removed all 56000 and QP support.  Also removed the internal types fraction, table, x-table, y-table, long-int.
        expsnd-related bugs fixed.  read-position works on envelopes. loop.lisp renamed clm-loop.lisp. More example
        instruments in ug.ins (envelope stick points, etc).
1-Nov:  removed gcl-nsi, ACL3.1-on-NeXT3.n, GCL-2.1-on-NeXT, liblinux, libm2. run enhanced (no need for loop).
18-Oct: Linux port (Clisp and GCL). MCL 4.0.
4-Oct:  MCL version supports vibstr.cl, defpinstrument, etc.
1-Oct:  clm-print accepts file as 1st arg. removed trans.lisp and untext.c (now in snd.tar.gz).
21-Sep: 4Front OSS support.
12-Sep: raw-sound-file output.
10-Sep: David Soley's mac.help; snd.tar.gz = new sound editor.
19-Aug: various Linux changes.
21-Jun: acl4-3.c for final version of ACL 4.3 (lisp_call replacement).
14-Jun: piano.ins -- Scott Van Duyne's physical model of a piano.
1-June: clm_signal for SGI SIGINT handling. cd.c, cd.lisp (SGI CD functions).
29-May: clm-status on NeXT, in gcl. pipe.c, unix.c, unix.lisp. sound-files-in-directory. revsnd (useful.lisp).
22-May: changed all C variable names that started with '_': _datar_ => clm_float_data and
        _datai_ => clm_int_data.  Table-interp generator removed. Snd-info data base.
17-May: removed mix the macro, and added an optimized fasmix/mix-in-place function named mix.
10-May: mix-in-place. 
6-May:  sound-files-in-directory (useful.lisp). fasmix/run optimizations.
1-May:  MCL PPC/Codewarrior support courtesy Rick Taube.  aifc mulaw/alaw output. *clm-eat-raw-sound*. Vax floats.
25-Apr: clm-cerror.
22-Apr: fm.wn replaced by fm.html (and associated *.gif). &optional-key args.
15-Apr: removed start-time, duration, and restartable arguments to make-env.
10-Apr: removed gcl-c56.lisp. 56k code no longer loaded by default.
7-Apr:  restart cases sprinkled about. More debugging aids.
5-Apr:  defcinstrument compilation sped up. c56-debug and c-debug replaced by *clm-debug* and *clm-history*.
26-Mar: default-sound-file-data-format. First stage of port to PPC courtesy Rick Taube.
25-Mar: ins. green.cl. new ins: hello-dentist, inside-out, cross-synthesis.
4-Mar:  clm-open-input*, bug in arrays of user-defined structs fixed.
26-Feb: fht in lisp, *clm-speed* removed, among others (make-func, c56-stubs.lisp, room.ins, clm.wn). def-clm-var improvements.
18-Feb: c-create-file, rewrote fusion.ins.
12-Feb: with-psound (rt.lisp, pfas.ins, preverb.ins). removed v and jcrev preloads from all.lisp.
11-Feb: def-clm-var.
5-Feb:  Hartley transform (fht), spectrum.  Changes to all fft-related functions, window names, fft-data structure removed.
31-Jan: fixed aiff header update bug that confused mix/with-mix.
28-Jan: *clm-beg-offset*.  rfasmix, add, cut in useful.lisp.
22-Jan: removed fm.rtf, ins.txt, clm.wn, and 220ins.lisp. fmex.ins and ugex.ins.
19-Jan: sambox-random removed, also the ran-op field of the noi struct, and the :type argument to randh.
10-Jan: aclpc/windows support.
4-Jan:  no-ffi switch.

-------------------------------- 1996 --------------------------------------

3-Nov:  ACL/SGI p-ins.
1-Nov:  Linux GCL-2.1 and NeXT GCL-2.1 support.
25-Oct: all references to KCL and AKCL -> GCL.  SGI/ACL/CLM-in-CM loader problem fixed(?).
23-Oct: endhook in with-sound, macintosh-io.lisp merged into c-io.lisp, ilink and ariel pc56d support removed.
11-Oct: clm-notehook changed to *clm-notehook*.
5-Oct:  phase renamed to angle (name collisions).  in-any and out-any gens.  *dac-worries*.  n-channel input/output.
        clm-get-channels is now channels or IO-chans.  clm-get-samples is now IO-siz.  Many changes to IO, locs, and rblk structures.
        Computed input for fft-filter, convolve, resample, src, spd.  mix-sound removed.  Dax for SGI. 
        fasmix arguments changed for channels>2 cases (matrix arg), output arg added.
        rblk-triggered field added, with trigger and triggered 'generic' functions [not DSP]. clm.rtf omitted (replaced by clm.html).
        make-expand file argument now on the :file keyword.  make-locsig arguments changed. Locsig can handle n-channel output.
        print/princ of array [not DSP].
17-Sep: useful.lisp contributions from Marco Trevisani.
1-Sep:  scales.cl. *clm-file-search-list*. Tandy headers. AIFC ulaw/alaw. mat.c for Matlab data files. ADC headers.
21-Aug: record function. start/end args to open-input.
12-Aug: prc-toolkit95.lisp (physical modelling examples).  SGI CMU-CL.
21-Jul: SGI ACL 4.2, CLisp.
17-Jul: SGI GCL-2.1. Removed set-dac-ramp.
26-Jun: AIFF 24-bit data.
21-Jun: FIR-filter, clm-read-swapped-{ints|floats}, coeffs arg (FIR filter) to fasmix.
16-Jun: srate arg to fasmix.
7-Jun:  run*. rms.ins courtesy Michael Edwards.
2-Jun:  bessel.lisp.  optional start arg in C-side dot-product.
2-May:  size generic function changed to size-of (name collision with CMN).
1-May:  pure lisp defpinstrument. Shared vibrato streams in vibstr.cl and vibexamp.cl.
24-Apr: generic functions.
19-Apr: Moved LPC mus.lisp stuff to lpc.lisp.  Deleted LPT display stuff.
10-Apr: byte and more array ops.
6-Apr:  make-clm.cl and changes to all.lisp to try to make clm build simpler.
3-Apr:  revision of boolean handling. New switch :just-lisp (if no FFI available). read-position setf-able.
3-Mar:  removed references to AKCL and updated GCL directions.  expsrc.ins and zd.ins.
27-Feb: singer.ins (Perry Cook). removed addons.lisp. Changed C ins name in defins and removed *clm-mac-array-size-limit*.
13-Feb: Sound Designer 1, SPPACK, MAUD, PSION. untext.c.
30-Jan: asymmetric-fm and sine-summation as C gens.
25-Jan: mcl fixups courtesy Richard Holmes.
23-Jan: clu-reset renamed to clm-reset. cnv.ins -- example of reverb via convolution. convolve disabled on 56k.
16-Jan: lpc.c/h/lisp.  FOF example in ins.lisp.  sms.lisp improved.

-------------------------------- 1995 --------------------------------------

31-Dec: data-format in with-sound => output floats, longs, etc. scaled-to argument to with-sound. fasmix arg names changed.
14-Dec: write_ircam_header for Sun port. Save-stats in with-sound on NeXT.  *clm-interrupt-action*.
9-Dec:  Sun support (excl).  CLM compatible with case-sensitive-lower lisps. clm-packages.lisp replaced by export.lisp.
17-Nov: in excl, several byte counters changed to use arrays to get full 32 bit integers (for very long sound files).
2-Nov:  dlocsig courtesy Fernando Lopez Lezcano. clm.html (check out the index!)
31-Oct: ranges of random/randh/randi consistent across 56k, c, lisp.  Compile-ins removed.
25-Oct: flute.ins (Nicky Hind). NS 3.3.
17-Oct: parallel instrument scheduling (defpinstrument) for C instruments.
4-Oct:  c-lisp callbacks fixed in C ins excl/kcl. instrument type can be :lisp.
28-Sep: gcl/akcl definstrument C source file pathname confusion.
19-Sep: resample and src bug fixed that caused trouble in long files.
9-Sep:  bicsf header support fixed. Old version is called the ircam header.
2-Sep:  AVR header support.
5-Aug:  list.lisp.
3-Aug:  support for eref and nth to C run loop.
14-Jul: updated env.lisp, env-repeat suggested by Michael Edwards.
22-Jun: Sound Designer II format.
21-Jun: reverb maxamp data to stats
7-Jun:  tap on 56k implemented; sigint support and sms.lisp improved.
2-Jun:  c_env inlined.
27-May: sms.lisp updated. sigint trap to C instruments.
20-May: cmus.h-directory (defaults.lisp).
2-May:  jrdac support for play program of Jean Laroche on the NeXT
25-Apr: describe-ins-state, phrasing stuff in C ins.
15-Apr: Ilink board (NSI).  c-mus-gens switch removed.
14-Apr: ztap, tap.
11-Apr: c-mus-gens optimized.  describe-structure-for-run replaced by def-clm-struct.
        describe-c-state and print-hash. fftflt-saved-env removed.
        All C envelopes are restartable. blk struct renamed rblk. c56 and clu packages removed.
9-Mar:  support non-word aligned data (NeXT files).
7-Mar:  more header readers and alaw/mulaw translation.
3-Mar:  AFsp header reader, completion of PC56D port
21-Feb: support for Ariel PC56D card on NeXTStep/Intel.
9-Feb:  several changes to dsp open/close code
1-Feb:  NS/Intel port for c unit generators, init-with-sound and finish-with-sound for Common Music
28-Jan: port to NSI (using c ugs).
3-Jan:  mac revisions, header/dac support for other file formats, DAC stuff, volume function

-------------------------------- 1994 --------------------------------------

24-Nov: complete revision of c unit generator code
1-Nov:  fm arg to waveshape unit generator.
12-Oct: read-forward, read-backward, read-position -- see the instrument 'backandforth' in ins.lisp
1-Oct:  56K monitor change; SFSU 192K DSP memory expansion support; yoffset.h removed, dsp32K switch removed
29-Jun: fusion.ins
17-May: input-file-start-time and input-file-start as synonyms of start-time and start respectively.
14-May: mix/with-mix notice interrupted section computations and recompute them.
16-Apr: quad case to fasmix.
25-Mar: removed NeXT OS 1.0 files, *grab-dac*.
22-Mar: save-body in with-sound.
18-Mar: seg-func, map-func, and exp-func coalesced into envelope, restart-env, compile-ins
15-Mar: more fasmix options; c-merge-sound removed, sound-let uses let*, convolve in kcl
8-Mar:  mix-in dur, fasmix for much faster sound file mixing
3-Mar:  spectral modelling san.ins
1-Mar:  physical modelling clarinet, waveguide removed, outa improved
24-Feb: with-mix, clm-get-max-amp improved
8-Feb:  nrev improved, sound-let, window changed to fft-window (name conflicts on Mac)
3-Feb:  C unit generators (for SGI), ACL 4.1 support on the NeXT

-------------------------------- 1993 --------------------------------------

24-Nov: (print var) works in Run, princ
16-Oct: ACL 3.1 on NeXT 3.0; *clm-instruments*
1-Oct:  *sound-player* for special dac routines; akcl on 3.0 NeXT.
10-Sep: ztap; multiple locsigs/outns.
7-Aug:  dsp32K for NeXT 32K DSP memory upgrade, NeXT 3.0.
18-Jun: multidimensional arrays.
8-Jun:  def-run-fun
1-Jun:  mix
28-May: let/let* do/do* local variables in loop statements, psetf/psetq
26-May: zdelay arrays and external functions, more than one outa/locsig
1-May:  open-input smarter, *clm-verbose*, fft in run, pm input to oscil, polynomial
10-Apr: Instrument-let
30-Mar: Mac II/Sound Accelerator support re-implemented
16-Mar: external envelopes, print, error, warn, funcall in Run
17-Feb: readin-reverse, clmrpc for QP access across Ethernet
5-Feb:  convolve two big files

-------------------------------- 1992 --------------------------------------

4-Dec:  Mac Audiomedia 56k
8-Nov:  quad IO support
18-Aug: KCL port
12-Aug: zdelay, describe-structure-for-run
8-Jul:  fft-env-simplify and others
25-Jun: phrasing, fm.wn
16-May: delay preload, sansy
12-Apr: 56k trig funcs, dpysnd
7-Apr:  56k floor, round, etc
3-Apr:  more envelope funcs
25-Mar: statistics in with-sound
4-Mar:  56k convolve, spectrum, windows
26-Feb: src, fft-filter
16-Feb: 56k ina bugfixes, more fltdes
13-Feb: fltdes FIR programs, divenv
10-Feb: fltdes remez, dot-product
4-Feb:  notch, waveguide, LPC stuff
1-Feb:  fmviolin.clm
26-Jan: more NeXT 2.0 OS changes
18-Jan: QP system 2.0, run-block, wave-train
11-Jan: 56k struct addressing, roomsig
5-Jan:  array-interp, step functions in make-env

-------------------------------- 1991 --------------------------------------

27-Dec: QP DRAM
24-Dec: QP scheduler
21-Dec: QP implementation, fft, waveshape
13-Nov: lattice and ladder filters
10-Sep: In-Hz, exponential envelopes, kiprev
25-Aug: display-chip-state, definstrument
10-Aug: reopen-output, get/set-dac-filter, get/set-volume, canter and drone, bag.clm.
31-Jul: locsig and with-sound

-------------------------------- 1990 --------------------------------------
