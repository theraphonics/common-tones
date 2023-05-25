/* audinfo decribes the current audio hardware state */

#include <mus-config.h>

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../headers/sndlib.h"

int main(int argc, char *argv[])
{
  mus_sound_initialize();
  mus_audio_describe();
  return(0);
}