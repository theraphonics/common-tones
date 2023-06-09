This is a description of the files in the clm directory.

add.ins               additive synthesis instrument
addflt.ins            filter instrument (Xavier Serra and Richard Karpen)
addsnd.ins            sound file mixing (see fasmix)
all.lisp              makefile for CLM
anoi.ins              noise reduction
audinfo.c             sndlib audinfo program
audio.c               dac support
autoc.ins             pitch estimation (Bret Battey)
backandforth.ins      readin example
badd.ins              Doug Fulton's additive synthesis instrument
bag.clm               bagpipe demo note list (uses Canter and Drone)
balance.cl/html       balance/rms/gain gens (Sam Heisz)
bell.ins              fm bell instrument (Mike McNabb)
bigbird.ins           waveshaping bird instrument
bird.clm              bird calls (a note list)
bird.ins              sine wave for bird calls
butterworth.cl/html   Butterworth filters (Sam Heisz)
canter.ins            part of bagpipe instrument (Peter Commons)
cellon.ins            feedback fm instrument
circular-scanned.ins  scanned synthesis (Juan Reyes)
clm-example.lisp      example of running CLM outside Common Music
clm-help.lisp         index into help files
clm-loop.lisp         loop.lisp -- the loop macro (can't exist under that name)
clm-package.lisp      CLM package definition
clm-snd.lisp          connection between CLM and Snd
clm-test.lisp         various "regression" tests
clm.c/h               CLM
clm2xen.c/h           C->Scheme (Guile) or Ruby connection
clm.html              CLM documentation (HTML format)
cmus.c/h              adjunct to clm.c
cmus.lisp	      run macro stuff
cnv.ins               convolution-based instruments (including reverb)
config*               C configuration files
cream.clm             from "You're the Cream in My Coffee"
defaults.lisp         various useful global variables
defins.lisp           definstrument and friends
dlocsig/*             Fernando Lopez Lezcano's moving sound source simulator
drone.ins             part of the bagpipe demo (Peter Commons)
env.lisp              envelope handlers
export.lisp	      export statement for clm
expsrc.ins	      examples of the expand generator
fade.ins              cross-fades in the frequency domain
ffi.lisp              foreign function linkages
ffi-test.lisp         FFI tests
filter-noise.ins      instrument using moog.lisp by Fernando Lopez-Lezcano
fltnoi.ins            filter white noise
fltsnd.ins            filter a sound file
flute.ins	      physical model of flute (Nicky Hind) 
fm.html               introduction to FM (uses *.gif)
fmex.ins              examples of FM
fmviolin.clm          examples of FM violin (see v.ins)
freeverb/*            Fernando Lopez Lezcano's translation of Jezar's reverberator
fullmix.ins           mixer 
grani.ins             granular synthesis instrument by Fernando Lopez-Lezcano
grapheq.ins           graphic equalizer instrument by Marco Trevisani
green.cl              bounded brownian noise
headers.c             sound file header readers/writers
help/*                various help files and index.cl
HISTORY.clm           brief change log for clm
index.cl              documentation index creator
initmus.lisp          package initialization, CLM change history
ins                   list of some instruments included in clm.tar.gz
insect.ins            fm insect
io.c                  low-level IO routines
io.lisp               higher-level IO routines
jcrev.ins             John Chowning's ancient reverberator
jlrev.ins             same but set up for 44.1KHz
jcvoi.ins             John Chowning's FM voice instrument
kiprev.ins            Kip Sheeline's reverberator
lbjPiano.ins          Doug Fulton's additive synthesis piano
libclm.def	      export declarations for MS C
maraca.ins	      Perry Cook's maraca instrument
maxf.ins              Modal synthesis (Juan Reyes)
mcl-doubles.cl        double arrays to C in openMCL
menu.lisp             MCL menu
midi.c                MIDI support (not yet tied into CLM)
mlbvoi.ins            Marc Le Brun's voice instrument (uses FM here)
moog.lisp             Moog-style filter generator by Fernando Lopez-Lezcano
mus.lisp              basic CLM unit generators in Lisp
noise.ins	      noise maker
nrev.ins              a popular reverberator (Mike McNabb)
panning.lsp           placement using sin/cos (Michael Edwards)
piano.ins             Scott Van Duyne's physical model of a piano
pitches.cl            pitch names
plotter.lisp          gnuplot output (Michael Edwards)
pluck.ins	      David Jaffe's Karplus-Strong synthesis instrument
popi.clm	      fugue on "Popeye"
pqw.ins		      Marc Le Brun's example of phase-quadrature waveshaping
pqwvox.ins	      the same extended for vocal effects
prc-toolkit95.lisp    Perry Cook's physical modelling toolkit translated to lisp. (Several instruments)
prc96.ins             Perry Cook continued (hasty translations of many example instruments)
pvoc.ins              phase vocoder (Michael Klingbeil)
README.clm            how to get CLM up and running, fascinating prose
refs		      references consulted while slaving away at this code
resflt.ins	      filter instrument (Xavier Serra)
reson.ins	      FM voice (John Chowning)
rev2.ins              example of 2 reverbs running at once
ring-modulate.ins     Ring modulation instrument (Craig Sapp)
rmsenv.ins	      rms envelope (Bret Battey)
roomsig.cl            ray tracing
run.lisp	      the run macro
san.ins		      spectral modelling (Xavier Serra)
sc.c                  snd clm connection
scales.cl	      many scales, courtesy John H. Chalmers and Manuel Op de Coul
scanned.ins           scanned synthesis (Juan Reyes)
scentroid.ins         spectral scentroid envelope (Bret Battey)
shell.c               shell wrapper
singer.ins	      Perry Cook's vocal tract physical model
singer.clm            examples of singer.ins
sndinfo.c             sndlib sndinfo program
sndlib.h              header for sndlib
sndlib.html           documentation of sndlib
sndlib-strings.h      more sndlib stuff
sndlib2clm.lisp       tie sndlib into CL (CLM)
sndlib2xen.c/h        tie sndlib into Scheme (Guile) (also sndlib-strings.h)
sndplay.c             sndlib play program
sndrecord.c           sndlib record program
sndwarp.ins           sndwarp (from Csound) by Bret Battey
sound.c               wrapper for sndlib
sound.lisp	      with-sound and other top-level macros
spectr.clm	      steady state spectra of many (acoustic) instruments (J. A. Moorer)
stochastic.ins        Bill Sack's stochastic synthesis implementation
strad.ins             violin physical model (Juan Reyes)
svf.lisp              'Vellocet'-style oversampled state-variable filter (David Lowenfels)
tc.ins                example of trigger
track-rms.ins	      rms tracker for sound files (Michael Edwards)
trp.ins		      FM trumpet (Dexter Morrill)
ugex.ins              various example instruments
ug(1,2,3,4).ins       various test instruments
useful.lisp	      various neat hacks for CLM
v.ins		      FM violin
vct.c/h               vector handlers for CLM in C in Guile
vox.ins		      voice synthesis (uses FM)
walk.lisp	      PCL's code walker
wavelets.cl           wavelet transforms
zd.ins                interpolating comb-filter gens (zcomb etc)
zipper.ins            The 'digital zipper' effect.
