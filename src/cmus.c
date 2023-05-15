/* C support code for CLM */

#include "../headers/cmus.h"
#define MUS_SAMPLE_TO_SHORT(n) ((short)((n) * (1 << 15)))

sigfnc *clm_signal(int signo, sigfnc *fnc) 
{
  return(signal(signo, fnc));
}


static void mus_error2clm(int type, char *str)
{
  /* there's apparently no way to get printout from C in ACL on windows --aclprintf, fprintf,
   *   and lisp_call_address back to lisp fails.  The folks at Franz had no suggestions.
   */
  fprintf(stdout, "%s", str); 
  fflush(stdout);
}


void initialize_cmus(void)
{
  mus_initialize();
  mus_error_set_handler(mus_error2clm);
}


void clm_mix(const char *outfile, const char *infile, int out_start, int out_framples, int in_start)
{
  mus_file_mix(outfile, infile, (mus_long_t)out_start, (mus_long_t)out_framples, (mus_long_t)in_start, NULL, 0, NULL);
}


#define MIX_BUFFER_SIZE 8192

int clm_scale_file(char *outfile, char *infile, double scaler, int out_format, int out_header)
{
  int i, j, k, ofd, ifd, chans, datsize, framples;
  mus_float_t **obufs;

  chans = mus_sound_chans(infile);
  obufs = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));
  for (i = 0; i < chans; i++) 
    obufs[i] = (mus_float_t *)calloc(MIX_BUFFER_SIZE, sizeof(mus_float_t));

  ifd = mus_sound_open_input(infile);
  mus_file_seek_frample(ifd, 0);
  mus_file_read(ifd, 0, MIX_BUFFER_SIZE, chans, obufs);

  ofd = mus_sound_open_output(outfile, mus_sound_srate(infile), chans, (mus_sample_t)out_format, (mus_header_t)out_header, NULL);

  datsize = mus_bytes_per_sample(out_format);
  framples = mus_sound_framples(infile);

  if (chans == 1)
    {
      for (k = 0, j = 0; k < framples; k++, j++)
	{
	  if (j == MIX_BUFFER_SIZE)
	    {
	      mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      mus_file_read(ifd, k, MIX_BUFFER_SIZE, chans, obufs);
	    }
	  obufs[0][j] = (mus_float_t)(scaler * obufs[0][j]);
	}
    }
  else
    {
      for (k = 0, j = 0; k < framples; k++, j++)
	{
	  if (j == MIX_BUFFER_SIZE)
	    {
	      mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      mus_file_read(ifd, k, MIX_BUFFER_SIZE, chans, obufs);
	    }
	  for (i = 0; i < chans; i++) 
	    obufs[i][j] = (mus_float_t)(obufs[i][j] * scaler);
	}
    }

  if (j > 0)
    mus_file_write(ofd, 0, j - 1, chans, obufs);

  mus_sound_close_output(ofd, framples * chans * datsize);
  mus_sound_close_input(ifd);

  for (i = 0; i < chans; i++)
    free(obufs[i]);
  free(obufs);

  return(framples);
}


/* sndplay as function call, added 14-Jul-00, 
 *                           fixed up mus_file_read trailing chunk business 26-Jan-01 (but it was broken again later?)
 *                           added device arg 8-Feb-01
 */

#if __APPLE__
  #define BUFFER_SIZE 256
#else
  #define BUFFER_SIZE 4096
#endif

int sl_dac(char *name, int output_device)
{
  int fd, afd, i, j, n, k, chans, srate, framples, outbytes, curframples;
  mus_float_t **bufs;
  short *obuf;
  mus_sound_initialize();

  afd = -1;
  fd = mus_sound_open_input(name);
  if (fd != -1)
    {
      chans = mus_sound_chans(name);
      if ((chans <= 0) || (chans > (1 << 22))) return(-1); /* 29-Mar-19 to squelch idiotic compiler warning */
      srate = mus_sound_srate(name);
      framples = mus_sound_framples(name);
      outbytes = BUFFER_SIZE * chans * 2;
      bufs = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));
      for (i = 0; i < chans; i++) 
	bufs[i] = (mus_float_t *)calloc(BUFFER_SIZE, sizeof(mus_float_t));
      obuf = (short *)calloc(BUFFER_SIZE * chans, sizeof(short));
      /* assume for lafs that our DAC wants 16-bit integers */
      for (i = 0; i < framples; i += BUFFER_SIZE)
	{
	  if ((i + BUFFER_SIZE) <= framples)
	    curframples = BUFFER_SIZE;
	  else curframples = framples - i;
	  mus_file_read(fd, i, curframples, chans, bufs); 
	  if (chans == 1)
	    {
	      for (k = 0; k < curframples; k++) 
		obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
	    }
	  else
	    {
	      if (chans == 2)
		{
		  for (k = 0, n = 0; k < curframples; k++, n += 2) 
		    {
		      obuf[n] = MUS_SAMPLE_TO_SHORT(bufs[0][k]); 
		      obuf[n + 1] = MUS_SAMPLE_TO_SHORT(bufs[1][k]);
		    }
		}
	      else
		{
		  for (k = 0, j = 0; k < curframples; k++, j += chans)
		    for (n = 0; n < chans; n++) 
		      obuf[j + n] = MUS_SAMPLE_TO_SHORT(bufs[n][k]);
		}
	    }
	  if (afd == -1)
	    {
	      afd = mus_audio_open_output(output_device, srate, chans, MUS_AUDIO_COMPATIBLE_SAMPLE_TYPE, outbytes);
	      if (afd == -1) 
		{
		  for (i = 0; i < chans; i++) free(bufs[i]);
		  free(bufs);
		  free(obuf);
		  return(-1);
		}
	    }
	  outbytes = curframples * chans * 2;
	  mus_audio_write(afd, (char *)obuf, outbytes);
	}
      if (afd != -1) mus_audio_close(afd);
      mus_sound_close_input(fd);
      for (i = 0; i < chans; i++) free(bufs[i]);
      free(bufs);
      free(obuf);
    }
  else return(-2);
  return(0);
}


/* type conversions for lisp FFI's that can't handle mus_long_t */

/* ffi.lisp */

int cl_clm_file_buffer_size(void)
{
  return((int)mus_file_buffer_size());
}

int cl_clm_set_file_buffer_size(int size)
{
  return((int)mus_set_file_buffer_size((mus_long_t)size));
}

int clm_sound_samples(const char *arg) 
{
  return((int)mus_sound_samples(arg));
}


int clm_sound_framples(const char *arg) 
{
  return((int)mus_sound_framples(arg));
}


int clm_sound_data_location(const char *arg) 
{
  return((int)mus_sound_data_location(arg));
}


int clm_sound_length(const char *arg) 
{
  return((int)mus_sound_length(arg));
}


char *clm_sound_comment(const char *arg)
{
  char *comment;
  comment = mus_sound_comment(arg);
  if ((comment) && (*comment))
    return(comment);
  return(" ");
}


/* sndlib2clm.lisp */

int clm_sound_comment_start(const char *arg) 
{
  return((int)mus_sound_comment_start(arg));
}


int clm_sound_comment_end(const char *arg) 
{
  return((int)mus_sound_comment_end(arg));
}


int clm_header_samples(void) 
{
  return((int)mus_header_samples());
}


int clm_header_data_location(void) 
{
  return((int)mus_header_data_location());
}


int clm_header_comment_start(void) 
{
  return((int)mus_header_comment_start());
}


int clm_header_comment_end(void) 
{
  return((int)mus_header_comment_end());
}


int clm_header_true_length(void) 
{
  return((int)mus_header_true_length());
}


int clm_samples_to_bytes(int format, int size) 
{
  return(mus_samples_to_bytes((mus_sample_t)format, (mus_long_t)size));
}


int clm_bytes_to_samples(int format, int size) 
{
  return(mus_bytes_to_samples((mus_sample_t)format, (mus_long_t)size));
}


int clm_header_write(const char *name, int type, int srate, int chans, int loc, int size_in_samples, int format, const char *comment, int len)
{
  return((int)mus_header_write(name, (mus_header_t)type, srate, chans, (mus_long_t)loc, (mus_long_t)size_in_samples, (mus_sample_t)format, comment, len));
}


int clm_header_aux_comment_start(int n) 
{
  return((int)mus_header_aux_comment_start(n));
}


int clm_header_aux_comment_end(int n) 
{
  return((int)mus_header_aux_comment_end(n));
}


int clm_file2array(char *filename, int chan, int start, int samples, double *array)
{
  return(mus_file_to_float_array(filename, chan, (mus_long_t)start, (mus_long_t)samples, array));
}


static double *clm_clisp_samples = NULL;
static int clm_clisp_samples_size = 0;

int clm_clisp_file2array_init(char *filename, int chan, int start, int samples)
{
  if (clm_clisp_samples) free(clm_clisp_samples);
  clm_clisp_samples = (double *)calloc(samples, sizeof(double));
  clm_clisp_samples_size = samples;
  return(clm_file2array(filename, chan, start, samples, clm_clisp_samples));
}


double clm_clisp_file2array(int sample)
{
  if ((clm_clisp_samples) &&
      (sample < clm_clisp_samples_size))
    return(clm_clisp_samples[sample]);
  return(0.0);
}


int clm_clisp_write_ints(int fd, int *buf, int n) {return(write(fd, (char *)buf, n));}

int clm_clisp_write_floats(int fd, double *buf, int n) {return(write(fd, (char *)buf, n));}

int clm_clisp_close(int fd) {return(close(fd));}

int clm_clisp_lseek(int fd, int loc, int type) {return((int)lseek(fd, loc, type));}


static double *clm_clisp_doubles = NULL;
static int clm_clisp_doubles_size = 0;

int clm_clisp_doubles_init(int fd, int n)
{
  if (clm_clisp_doubles) free(clm_clisp_doubles);
  clm_clisp_doubles = (double *)calloc(n, sizeof(double));
  clm_clisp_doubles_size = n;
  return(read(fd, (char *)clm_clisp_doubles, sizeof(double) * n));
}


double clm_clisp_double(int n)
{
  if ((clm_clisp_doubles) &&
      (n < clm_clisp_doubles_size))
    return(clm_clisp_doubles[n]);
  return(0.0);
}


static int *clm_clisp_ints = NULL;
static int clm_clisp_ints_size = 0;

int clm_clisp_ints_init(int fd, int n)
{
  if (clm_clisp_ints) free(clm_clisp_ints);
  clm_clisp_ints = (int *)calloc(n, sizeof(int));
  clm_clisp_ints_size = n;
  return(read(fd, (char *)clm_clisp_ints, sizeof(int) * n));
}


int clm_clisp_int(int n)
{
  if ((clm_clisp_ints) &&
      (n < clm_clisp_ints_size))
    return(clm_clisp_ints[n]);
  return(0);
}


void clm_array2file(char *filename, double *ddata, int len, int srate, int channels)
{
  mus_float_array_to_file(filename, ddata, (mus_long_t)len, srate, channels);
}

int clm_sound_maxamp(const char *ifile, int chans, double *vals, int *times)
{
  mus_long_t *ts;
  mus_float_t *xs;
  int i;
  int res = -1;
  ts = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
  xs = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
  res = mus_sound_maxamps(ifile, chans, xs, ts);
  for (i = 0; i < chans; i++) 
    {
      times[i] = (int)ts[i]; 
      vals[i] = (double)xs[i];
    }
  free(ts);
  free(xs);
  return(res);
}


/* this kludge is for Clisp */

static mus_float_t *maxamp_xs = NULL;
static mus_long_t *maxamp_ts = NULL;
static int maxamp_size = 0;

int clm_sound_maxamp_init(const char *ifile, int chans)
{
  if (maxamp_xs) free(maxamp_xs);
  if (maxamp_ts) free(maxamp_ts);
  maxamp_size = chans;
  maxamp_ts = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
  maxamp_xs = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
  return(mus_sound_maxamps(ifile, chans, maxamp_xs, maxamp_ts));
}


int clm_sound_maxamp_time(int chan)
{
  if ((maxamp_ts) &&
      (chan < maxamp_size))
    return(maxamp_ts[chan]);
  return(-1);
}


double clm_sound_maxamp_amp(int chan)
{
  if ((maxamp_xs) &&
      (chan < maxamp_size))
    return(maxamp_xs[chan]);
  return((double)(-1.0));
}


/* endian converters (clm1.lisp) */

void swap_int_array(int *arr, int size);

void swap_int_array(int *arr, int size)
{
  int i, lim;
  unsigned char *p;
  unsigned char temp;
  p = (unsigned char *)arr;
  lim = size * 4;
  for (i = 0; i < lim; i += 4)
    {
      temp = p[i + 0];
      p[i + 0] = p[i + 3]; 
      p[i + 3] = temp;
      temp = p[i + 1];
      p[i + 1] = p[i + 2]; 
      p[i + 2] = temp;
    }
}


void swap_double_array(double *arr, int size);

void swap_double_array(double *arr, int size)
{
  int i, lim;
  unsigned char *p;
  unsigned char temp;
  p = (unsigned char *)arr;
  lim = size * 8;
  for (i = 0; i < lim; i += 8)
    {
      temp = p[i + 0];
      p[i + 0] = p[i + 7]; 
      p[i + 7] = temp;
      temp = p[i + 1];
      p[i + 1] = p[i + 6]; 
      p[i + 6] = temp;
      temp = p[i + 2];
      p[i + 2] = p[i + 5]; 
      p[i + 5] = temp;
      temp = p[i + 3];
      p[i + 3] = p[i + 4];
      p[i + 4] = temp;
    }
}


/* FFI's also can't handle bool! */

int clm_mus_file_probe(const char *name)
{
  if (mus_file_probe(name))
    return(1);
  return(0);
}

int clm_mus_clipping(void) 
{
  if (mus_clipping())
    return(1); 
  return(0);
}

int clm_mus_set_clipping(int new_value) 
{
  mus_set_clipping((bool)new_value);
  return(new_value);
}

int clm_mus_header_writable(int type, int format)
{
  if (mus_header_writable((mus_header_t)type, (mus_sample_t)format))
    return(1);
  return(0);
}


/* ---------------- CLM.C support ---------------- */

#define INITIAL_GENBAG_SIZE 8

typedef struct {
  int size;
  int top;
  mus_any **gens;
} genbag;


void *clm_make_genbag(void)
{
  genbag *gb;
  gb = (genbag *)calloc(1, sizeof(genbag));
  gb->size = INITIAL_GENBAG_SIZE;
  gb->top = 0;
  gb->gens = (mus_any **)calloc(gb->size, sizeof(mus_any *));
  return((void *)gb);
}


mus_any *clm_add_gen_to_genbag(void *bag, mus_any *gen)
{
  genbag *gb = (genbag *)bag;
  if (gb->top == gb->size)
    {
      int i;
      gb->size += INITIAL_GENBAG_SIZE;
      gb->gens = (mus_any **)realloc((void *)(gb->gens), gb->size * sizeof(mus_any *));
      for (i = gb->top; i < gb->size; i++) gb->gens[i] = NULL;
    }
  gb->gens[gb->top++] = gen;
  return(gen);
}


void *clm_free_genbag(void *bag)
{
  int i;
  genbag *gb = (genbag *)bag;
  if (gb)
    {
      if (gb->top > 0)
	for (i = 0; i < gb->top; i++)
	  if (gb->gens[i])
	    mus_free(gb->gens[i]);
      free(gb);
    }
  return(NULL);
}


mus_float_t clm_as_needed_input(void *arg, int direction)
{
  /* assume readin as arg */
  mus_any *gen = (mus_any *)arg;
  if (direction != (int)mus_increment(gen))
    mus_set_increment(gen, (mus_float_t)direction);
  return(mus_readin(gen));
}


static mus_any *clm_output_stream = NULL, *clm_reverb_stream = NULL;

bool clm_make_output(const char *filename, int chans, int out_format, int out_type, const char *comment)
{
  if (clm_output_stream) return(false);
  clm_output_stream = mus_make_frample_to_file_with_comment(filename, chans, (mus_sample_t)out_format, (mus_header_t)out_type, comment);
  return(true);
}


bool clm_make_reverb(const char *filename, int chans, int out_format, int out_type, const char *comment)
{
  if (clm_reverb_stream) return(false);
  clm_reverb_stream = mus_make_frample_to_file_with_comment(filename, chans, (mus_sample_t)out_format, (mus_header_t)out_type, comment);
  return(true);
}


bool clm_continue_output(const char *filename)
{
  if (clm_output_stream) 
    fprintf(stderr, "request to reopen *output*, but it's already open?");
  clm_output_stream = mus_continue_frample_to_file(filename);
  return(true);
}


bool clm_continue_reverb(const char *filename)
{
  if (clm_reverb_stream) 
    fprintf(stderr, "request to reopen *reverb*, but it's already open?");
  clm_reverb_stream = mus_continue_frample_to_file(filename);
  return(true);
}


mus_any *clm_output(void)
{
  return(clm_output_stream);
}


mus_any *clm_reverb(void)
{
  return(clm_reverb_stream);
}


void clm_set_output_safety(int safety)
{
}


void clm_set_reverb_safety(int safety)
{
}


bool clm_close_output(void)
{
  if (!clm_output_stream) return(false); 
  mus_close_file(clm_output_stream); /* is this needed? */
  mus_free(clm_output_stream);
  clm_output_stream = NULL;
  return(true);
}


bool clm_close_reverb(void)
{
  if (!clm_reverb_stream) return(false); 
  mus_close_file(clm_reverb_stream); /* is this needed? */
  mus_free(clm_reverb_stream);
  clm_reverb_stream = NULL;
  return(true);
}


mus_long_t clm_to_mus_long_t(int *data, int loc)
{
  /* lisp can't always pass us 32-bit quantities, so try 24 + 24 + sign */
  int sign = 1;
  if (data[loc + 1] & 0x2000000) sign = -1;
  /* bizarre!! all these "mus_long_t"'s are needed */
  return((mus_long_t)(((mus_long_t)data[loc] & 0xffffff) + (mus_long_t)(((mus_long_t)data[loc + 1] & 0xffffff) << 24)) * (mus_long_t)sign);
}


/* specializations of some generic functions for the CL-CLM context */

static mus_long_t clm_location(mus_any *ptr)
{
  mus_any *closure;
  if (ptr)
    {
      if (!mus_is_src(ptr))
	return(mus_location(ptr));
      closure = (mus_any *)mus_environ(ptr); 
      if ((closure) &&
	  (mus_is_readin(closure)))
	return(mus_location(closure));
    }
  return(0);
}


static mus_long_t clm_set_location(mus_any *ptr, mus_long_t val)
{
  mus_any *closure;
  if (ptr)
    {
      if (!mus_is_src(ptr))
	return(mus_set_location(ptr, val));
      closure = (mus_any *)mus_environ(ptr);
      if ((closure) &&
	  (mus_is_readin(closure)))
	return(mus_set_location(closure, val));
    }
  return(0);
}


static int clm_channel(mus_any *ptr)
{
  mus_any *closure;
  if (ptr)
    {
      if (!mus_is_src(ptr))
	return(mus_channel(ptr));
      closure = (mus_any *)mus_environ(ptr);
      if ((closure) &&
	  (mus_is_readin(closure)))
	return(mus_channel(closure));
    }
  return(0);
}


mus_any *clm_make_src(mus_float_t (*input)(void *arg, int direction), mus_float_t srate, int width, void *closure)
{
  mus_any *gen;
  gen = mus_make_src(input, srate, width, closure);
  if (gen)
    {
      mus_generator_set_location(mus_generator_class(gen), clm_location);
      mus_generator_set_set_location(mus_generator_class(gen), clm_set_location);
      mus_generator_set_channel(mus_generator_class(gen), clm_channel);
    }
  return(gen);
}


mus_float_t clm_locsig(mus_any *ptr, mus_long_t loc, mus_float_t val)
{
  mus_locsig(ptr, loc, val);
  return(val);
}

int clm_little_endian(void)
{
#if MUS_LITTLE_ENDIAN
  return(1);
#else
  return(0);
#endif
}
