/* sndrecord records a sound */

#include "../headers/mus-config.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "../headers/_sndlib.h"

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
  #include <string.h>
#endif
#include <errno.h>

#define CHANNELS 1

#define BUFFER_SIZE 4096
#define READS 10
typedef short indata;
#ifdef MUS_WINDOZE
  #define FILE_TYPE MUS_RIFF
  #define DATA_TYPE MUS_UBYTE
  #define SAMPLING_RATE 22050
#else
  #if (HAVE_OSS || HAVE_ALSA)
    #define FILE_TYPE MUS_RIFF
    #define DATA_TYPE MUS_LSHORT
    #define SAMPLING_RATE 22050
  #else
    #define FILE_TYPE MUS_AIFC
    #define DATA_TYPE MUS_BSHORT
    #define SAMPLING_RATE 44100
  #endif
#endif

int main(int argc, char *argv[])
{
  int fd, afd, i, err, bytes_per_sample, bytes_per_read;
  float mic_gain[1];
  indata *ibuf;
  if (argc == 1) {printf("usage: sndrecord outfile\n"); exit(0);}
  afd = -1;
  mus_sound_initialize();
  /* make sure the microphone is on */
  mic_gain[0] = 1.0;
  mus_audio_mixer_write(MUS_AUDIO_MICROPHONE, MUS_AUDIO_AMP, 0, mic_gain);
  if (CHANNELS == 2) mus_audio_mixer_write(MUS_AUDIO_MICROPHONE, MUS_AUDIO_AMP, 1, mic_gain);
  /* open the output sound file */
  bytes_per_sample = mus_bytes_per_sample(DATA_TYPE);
  bytes_per_read = BUFFER_SIZE * bytes_per_sample;
  fd = mus_sound_open_output(argv[1], SAMPLING_RATE, CHANNELS, DATA_TYPE, FILE_TYPE, "created by sndrecord");
  if (fd != -1)
    {
      /* prepare and open the microphone input line */
      ibuf = (indata *)calloc(BUFFER_SIZE, sizeof(indata));
      afd = mus_audio_open_input(MUS_AUDIO_MICROPHONE, SAMPLING_RATE, CHANNELS, DATA_TYPE, bytes_per_read);
      if (afd != -1)
	{
	  for (i = 0; i < READS; i++)
	    {
	      err = mus_audio_read(afd, (char *)ibuf, bytes_per_read);
	      if (err == MUS_NO_ERROR) write(fd, (char *)ibuf, bytes_per_read);
	    }
	  mus_audio_close(afd);
	}
      mus_sound_close_output(fd, bytes_per_read * READS);
      free(ibuf);
    }
  return(0);
}
