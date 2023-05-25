#ifndef CMUS_H
#define CMUS_H

#include "mus-config.h"

#ifndef _MSC_VER
  #include <unistd.h>
#endif
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <stdarg.h>
#include <signal.h>
#include <stdlib.h>

#include "_sndlib.h"
/* this is setup by configure, so we shouldn't go wandering off to /pkg/opt/sfw or something */
#include "clm.h"

#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI/2.0)
#endif

#ifndef TWO_PI
  #define TWO_PI (2.0 * M_PI)
#endif

typedef void sigfnc(int);

#define SND_CLM_FIFO "snd_clm_fifo"
#define CLM_SND_FIFO "clm_snd_fifo"

#define CLM_AREF_TYPE 0
#define CLM_AREF_IBLOCK 1
#define CLM_AREF_RBLOCK 2
#define CLM_AREF_SIZE 3
#define CLM_AREF_DIMS 4

#define CLM_NO_TYPE 0
#define CLM_INTEGER 1
#define CLM_REAL 2
#define CLM_OSCIL 3
#define CLM_SUM_OF_COSINES 4
#define CLM_RAND 5
#define CLM_RAND_INTERP 6
#define CLM_TABLE_LOOKUP 7
#define CLM_SQUARE_WAVE 8
#define CLM_PULSE_TRAIN 9
#define CLM_SAWTOOTH_WAVE 10
#define CLM_TRIANGLE_WAVE 11
#define CLM_ASYMMETRIC_FM 12
#define CLM_WAVE_TRAIN 13
#define CLM_ONE_POLE 14
#define CLM_TWO_POLE 15
#define CLM_ONE_ZERO 16
#define CLM_TWO_ZERO 17
#define CLM_DELAY 18
#define CLM_TAP 19
#define CLM_COMB 20
#define CLM_NOTCH 21
#define CLM_ALL_PASS 22
#define CLM_FILTER 23
#define CLM_FIR_FILTER 24
#define CLM_IIR_FILTER 25
#define CLM_ARRAY 26
#define CLM_ENV 27
#define CLM_LOCSIG 28
#define CLM_SRC 29
#define CLM_GRANULATE 30
#define CLM_READIN 31
#define CLM_CONVOLVE 32
#define CLM_SINE_SUMMATION 33
#define CLM_WAVESHAPE 34
#define CLM_FORMANT 35
#define CLM_REAL_ARRAY 36
#define CLM_INTEGER_ARRAY 37
#define CLM_STRING 38
/* #define CLM_FRAME 39 */
/* #define CLM_MIXER 40 */
#define CLM_PHASE_VOCODER 41
#define CLM_BIGNUM 42
#define CLM_MOVING_AVERAGE 43
#define CLM_SUM_OF_SINES 44
#define CLM_SSB_AM 45
#define CLM_FILE2SAMPLE 46
#define CLM_FILE2FRAMPLE 47
#define CLM_SAMPLE2FILE 48
#define CLM_FRAMPLE2FILE 49
#define CLM_POLYSHAPE 50
#define CLM_FILTERED_COMB 51
#define CLM_MOVE_SOUND 52
#define CLM_NCOS 53
#define CLM_NSIN 54
#define CLM_NRXYCOS 55
#define CLM_NRXYSIN 56
#define CLM_POLYWAVE 57
#define CLM_FIRMANT 58

#define CLM_VAR_TYPE(Arg) clm_int[Arg]
#define CLM_VAR_ADDR(Arg) clm_int[Arg + 1]
#define CLM_ILOC(Arg)     clm_int[Arg + 1]
#define CLM_RLOC(Arg)     clm_int[clm_int[Arg + 1] + 1]

#define CLM_ARR_IBLOCK(Arg)   clm_int[Arg + CLM_AREF_IBLOCK]
#define CLM_ARR_RBLOCK(Arg)   clm_int[Arg + CLM_AREF_RBLOCK]
#define CLM_ARR_SIZE(Arg)     clm_int[Arg + CLM_AREF_SIZE]
#define CLM_ARR_TYPE(Arg)     clm_int[Arg + CLM_AREF_TYPE]
#define CLM_ARR_DIMS(Arg)     clm_int[Arg + CLM_AREF_DIMS]
#define CLM_ARR_DIM(Arg, Dim) clm_int[Arg + CLM_AREF_DIMS + 1 + Dim]

#define CLM_NO_TYPE_P(Arg)          (CLM_VAR_TYPE(Arg) == CLM_NO_TYPE)
#define CLM_INTEGER_P(Arg)          (CLM_VAR_TYPE(Arg) == CLM_INTEGER)
#define CLM_REAL_P(Arg)             (CLM_VAR_TYPE(Arg) == CLM_REAL)
#define CLM_OSCIL_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_OSCIL) &&          (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SUM_OF_COSINES_P(Arg)   ((CLM_VAR_TYPE(Arg) == CLM_SUM_OF_COSINES) && (CLM_VAR_ADDR(Arg) != 0))
#define CLM_RAND_P(Arg)             ((CLM_VAR_TYPE(Arg) == CLM_RAND) &&           (CLM_VAR_ADDR(Arg) != 0))
#define CLM_RAND_INTERP_P(Arg)      ((CLM_VAR_TYPE(Arg) == CLM_RAND_INTERP) &&    (CLM_VAR_ADDR(Arg) != 0))
#define CLM_TABLE_LOOKUP_P(Arg)     ((CLM_VAR_TYPE(Arg) == CLM_TABLE_LOOKUP) &&   (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SQUARE_WAVE_P(Arg)      ((CLM_VAR_TYPE(Arg) == CLM_SQUARE_WAVE) &&    (CLM_VAR_ADDR(Arg) != 0))
#define CLM_PULSE_TRAIN_P(Arg)      ((CLM_VAR_TYPE(Arg) == CLM_PULSE_TRAIN) &&    (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SAWTOOTH_WAVE_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_SAWTOOTH_WAVE) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_TRIANGLE_WAVE_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_TRIANGLE_WAVE) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ASYMMETRIC_FM_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_ASYMMETRIC_FM) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_WAVE_TRAIN_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_WAVE_TRAIN) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ONE_POLE_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_ONE_POLE) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_TWO_POLE_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_TWO_POLE) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ONE_ZERO_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_ONE_ZERO) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_TWO_ZERO_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_TWO_ZERO) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_DELAY_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_DELAY) &&          (CLM_VAR_ADDR(Arg) != 0))
#define CLM_TAP_P(Arg)              ((CLM_VAR_TYPE(Arg) == CLM_TAP) &&            (CLM_VAR_ADDR(Arg) != 0))
#define CLM_COMB_P(Arg)             ((CLM_VAR_TYPE(Arg) == CLM_COMB) &&           (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FILTERED_COMB_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_FILTERED_COMB) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_NOTCH_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_NOTCH) &&          (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ALL_PASS_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_ALL_PASS) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FILTER_P(Arg)           ((CLM_VAR_TYPE(Arg) == CLM_FILTER) &&         (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FIR_FILTER_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_FIR_FILTER) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_IIR_FILTER_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_IIR_FILTER) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ARRAY_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_ARRAY) &&          (CLM_VAR_ADDR(Arg) != 0))
#define CLM_ENV_P(Arg)              ((CLM_VAR_TYPE(Arg) == CLM_ENV) &&            (CLM_VAR_ADDR(Arg) != 0))
#define CLM_LOCSIG_P(Arg)           ((CLM_VAR_TYPE(Arg) == CLM_LOCSIG) &&         (CLM_VAR_ADDR(Arg) != 0))
#define CLM_MOVE_SOUND_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_MOVE_SOUND) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SRC_P(Arg)              ((CLM_VAR_TYPE(Arg) == CLM_SRC) &&            (CLM_VAR_ADDR(Arg) != 0))
#define CLM_GRANULATE_P(Arg)        ((CLM_VAR_TYPE(Arg) == CLM_GRANULATE) &&      (CLM_VAR_ADDR(Arg) != 0))
#define CLM_READIN_P(Arg)           ((CLM_VAR_TYPE(Arg) == CLM_READIN) &&         (CLM_VAR_ADDR(Arg) != 0))
#define CLM_CONVOLVE_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_CONVOLVE) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SINE_SUMMATION_P(Arg)   ((CLM_VAR_TYPE(Arg) == CLM_SINE_SUMMATION) && (CLM_VAR_ADDR(Arg) != 0))
#define CLM_WAVESHAPE_P(Arg)        ((CLM_VAR_TYPE(Arg) == CLM_WAVESHAPE) &&      (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FORMANT_P(Arg)          ((CLM_VAR_TYPE(Arg) == CLM_FORMANT) &&        (CLM_VAR_ADDR(Arg) != 0))
#define CLM_REAL_ARRAY_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_REAL_ARRAY) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_INTEGER_ARRAY_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_INTEGER_ARRAY) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_STRING_P(Arg)           ((CLM_VAR_TYPE(Arg) == CLM_STRING) &&         (CLM_VAR_ADDR(Arg) != 0))
/* #define CLM_FRAME_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_FRAME) &&          (CLM_VAR_ADDR(Arg) != 0)) */
/* #define CLM_MIXER_P(Arg)            ((CLM_VAR_TYPE(Arg) == CLM_MIXER) &&          (CLM_VAR_ADDR(Arg) != 0)) */
#define CLM_PHASE_VOCODER_P(Arg)    ((CLM_VAR_TYPE(Arg) == CLM_PHASE_VOCODER) &&  (CLM_VAR_ADDR(Arg) != 0))
#define CLM_BIGNUM_P(Arg)           (CLM_VAR_TYPE(Arg) == CLM_BIGNUM)
#define CLM_MOVING_AVERAGE_P(Arg)   ((CLM_VAR_TYPE(Arg) == CLM_MOVING_AVERAGE) && (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SUM_OF_SINES_P(Arg)     ((CLM_VAR_TYPE(Arg) == CLM_SUM_OF_SINES) &&   (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SSB_AM_P(Arg)           ((CLM_VAR_TYPE(Arg) == CLM_SSB_AM) &&         (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FILE2SAMPLE_P(Arg)      ((CLM_VAR_TYPE(Arg) == CLM_FILE2SAMPLE) &&    (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FILE2FRAMPLE_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_FILE2FRAMPLE) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_SAMPLE2FILE_P(Arg)      ((CLM_VAR_TYPE(Arg) == CLM_SAMPLE2FILE) &&    (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FRAMPLE2FILE_P(Arg)       ((CLM_VAR_TYPE(Arg) == CLM_FRAMPLE2FILE) &&     (CLM_VAR_ADDR(Arg) != 0))
#define CLM_POLYSHAPE_P(Arg)        ((CLM_VAR_TYPE(Arg) == CLM_POLYSHAPE) &&      (CLM_VAR_ADDR(Arg) != 0))
#define CLM_NCOS_P(Arg)             ((CLM_VAR_TYPE(Arg) == CLM_NCOS) &&           (CLM_VAR_ADDR(Arg) != 0))
#define CLM_NSIN_P(Arg)             ((CLM_VAR_TYPE(Arg) == CLM_NSIN) &&           (CLM_VAR_ADDR(Arg) != 0))
#define CLM_NRXYCOS_P(Arg)          ((CLM_VAR_TYPE(Arg) == CLM_NRXYCOS) &&        (CLM_VAR_ADDR(Arg) != 0))
#define CLM_NRXYSIN_P(Arg)          ((CLM_VAR_TYPE(Arg) == CLM_NRXYSIN) &&        (CLM_VAR_ADDR(Arg) != 0))
#define CLM_POLYWAVE_P(Arg)         ((CLM_VAR_TYPE(Arg) == CLM_POLYWAVE) &&       (CLM_VAR_ADDR(Arg) != 0))
#define CLM_FIRMANT_P(Arg)          ((CLM_VAR_TYPE(Arg) == CLM_FIRMANT) &&        (CLM_VAR_ADDR(Arg) != 0))

#ifdef __cplusplus
extern "C" {
#endif

void initialize_cmus(void);
int clm_scale_file(char *outfile, char *infile, double scaler, int out_format, int out_header);
int clm_file2array(char *filename, int chan, int start, int samples, double *array);
void clm_array2file(char *filename, double *data, int len, int srate, int channels);
long lisp_call(int index);

sigfnc *clm_signal(int signo, sigfnc *fnc);
#ifndef _MSC_VER
  int clm_send_snd(char *command);
  int clm_init_x(char *version);
  char *clm_receive_snd(void);
#endif

int sl_dac(char *name, int output_device);

int cl_clm_file_buffer_size(void);
int cl_clm_set_file_buffer_size(int size);

int clm_sound_samples(const char *arg);
int clm_sound_framples(const char *arg);
int clm_sound_data_location(const char *arg);
int clm_sound_length(const char *arg);
int clm_sound_maxamp(const char *ifile, int chans, double *vals, int *times);
int clm_sound_maxamp_init(const char *ifile, int chans);
int clm_sound_maxamp_time(int chan);
double clm_sound_maxamp_amp(int chan);
int clm_sound_comment_start(const char *arg);
int clm_sound_comment_end(const char *arg);
int clm_header_samples(void);
int clm_header_data_location(void);
int clm_header_comment_start(void);
int clm_header_comment_end(void);
int clm_header_true_length(void);
int clm_samples_to_bytes(int format, int size);
int clm_bytes_to_samples(int format, int size);
int clm_header_write(const char *name, int type, int srate, int chans, int loc, int size_in_samples, int format, const char *comment, int len);
int clm_header_aux_comment_start(int n);
int clm_header_aux_comment_end(int n);
char *clm_sound_comment(const char *arg);

int clm_mus_file_probe(const char *name);
int clm_mus_clipping(void);
int clm_mus_set_clipping(int new_value);
int clm_mus_header_writable(int type, int format);

int clm_clisp_write_ints(int fd, int *buf, int n);
int clm_clisp_write_floats(int fd, double *buf, int n);
int clm_clisp_close(int fd);
int clm_clisp_lseek(int fd, int loc, int type);
int clm_clisp_file2array_init(char *filename, int chan, int start, int samples);
double clm_clisp_file2array(int sample);
int clm_clisp_int(int n);
int clm_clisp_ints_init(int fd, int n);
double clm_clisp_double(int n);
int clm_clisp_doubles_init(int fd, int n);

void *clm_make_genbag(void);
mus_any *clm_add_gen_to_genbag(void *bag, mus_any *gen);
void *clm_free_genbag(void *bag);
mus_float_t clm_as_needed_input(void *arg, int direction);
void clm_mix(const char *outfile, const char *infile, int out_start, int out_frames, int in_start);

bool clm_make_output(const char *filename, int chans, int out_format, int out_type, const char *comment);
bool clm_make_reverb(const char *filename, int chans, int out_format, int out_type, const char *comment);
bool clm_continue_output(const char *filename);
bool clm_continue_reverb(const char *filename);
mus_any *clm_output(void);
mus_any *clm_reverb(void);
bool clm_close_output(void);
bool clm_close_reverb(void);
void clm_set_output_safety(int safety);
void clm_set_reverb_safety(int safety);

mus_long_t clm_to_mus_long_t(int *data, int loc);

int clm_little_endian(void);

mus_any *clm_make_src(mus_float_t (*input)(void *arg, int direction), mus_float_t srate, int width, void *closure);
mus_float_t clm_locsig(mus_any *ptr, mus_long_t loc, mus_float_t val);

#ifdef __cplusplus
}
#endif

#endif