/* ------------------------------- MACOS ----------------------------------------- */

#ifdef MACOS
#define AUDIO_OK

#include <Resources.h>
#include <Sound.h>
#include <SoundInput.h>


/* realloc replacement, thanks to Michael Klingbeil */
Ptr NewPtr_realloc(Ptr p, Size newSize)
{
  Size oldSize;
  Ptr  newP;
  /* zero size means free */
  if (newSize == 0) 
    {
      DisposePtr(p);
      return 0;
    }
  if (p) 
    {
      /* first try to reallocate in place */
      SetPtrSize(p, newSize);
      if (MemError() == 0)
	return p;
    }
  /* if that fails then try to reallocate */
  newP = NewPtr(newSize);
  /* failure */
  if (newP == 0) 
    {
      /* do we do DisposePtr(p) ? */
      return 0;
    }
  /* copy old pointer data */
  if (p) 
    {
      oldSize = GetPtrSize(p);
      BlockMoveData(p, newP, oldSize);
      DisposePtr(p);
    }
  return newP;
}



int mus_audio_systems(void) {return(1);} /* if Audiomedia, multiple? */
char *mus_audio_system_name(int system) {return("Mac");}

static int available_input_devices(void)
{
  unsigned char *devname;
  OSErr err;
  int i;
  Handle h;
  devname = (unsigned char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  for (i = 1; i < 16; i++)
    {
      err = SPBGetIndexedDevice(i, devname, &h);
      if (err != noErr) break;
    }
  FREE(devname);
  return(i - 1);
}

static int input_device_is_connected(long refnum)
{
  OSErr err;
  short connected;
  err = SPBGetDeviceInfo(refnum, siDeviceConnected, &connected);
  return(connected);
}

static int input_device_get_source(long refnum)
{
  OSErr err;
  short source;
  err = SPBGetDeviceInfo(refnum, siInputSource, &source);
  return(source);
}

static int input_device_set_source(long refnum, short source)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siInputSource, &source);
  return((err == noErr) ? 0 : -1);
}

static int input_device_get_sources(long refnum, char **names)
{
  OSErr err;
  short sources;
  Handle h;
  err = SPBSetDeviceInfo(refnum, siInputSourceNames, &h);
  if (err == siUnknownInfoType) return(0);
  sources = (short)(*h);
  /* printf("%d sources: %s ", sources, strdup(p2cstr((unsigned char *)(*(h + 2))))); */
  /* need an example to test this silly thing */
  return((err == noErr) ? sources : -1);
}

static int input_device_channels (long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum, siChannelAvailable, &chans);
  if (err == noErr) return(chans);
  return(-1);
}

static int input_device_get_async(long refnum)
{
  OSErr err;
  short async;
  err = SPBGetDeviceInfo(refnum, siAsync, &async);
  if (err == noErr) return(async);
  return(-1);
}

static char *input_device_name(long refnum)
{
  char *name;
  OSErr err;  
  name = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  err = SPBGetDeviceInfo(refnum, siDeviceName, name);
  if (err == noErr) return(name);
  FREE(name);
  return(NULL);
}

static float input_device_get_gain(long refnum)
{
  OSErr err;
  unsigned long val;
  err = SPBGetDeviceInfo(refnum, siInputGain, &val);
  /* val is a "4 byte fixed value between .5 and 1.5"!! */
  if (err == noErr)
    return((float)val / 65536.0);
  return(-1);
}

static int input_device_set_gain(long refnum, float gain)
{
  OSErr err;
  int val;
  val = ((int)(gain * 65536));
  err = SPBSetDeviceInfo(refnum, siInputGain, &val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_channels(long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum, siNumberChannels, &chans);
  return((err == noErr) ? chans : -1); 
}

static int input_device_set_channels(long refnum, short chans)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siNumberChannels, &chans);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_quality(long refnum)
{
  OSErr err;
  OSType val;
  err = SPBGetDeviceInfo(refnum, siRecordingQuality, &val);
  if (err == noErr)
    {
      if (val == siCDQuality) return(3);
      if (val == siBestQuality) return(2);
      if (val == siBetterQuality) return(1);
      if (val == siGoodQuality) return(0);
    }
  return(-1);
}

static int input_device_set_quality(long refnum, int quality)
{
  OSErr err;
  OSType val;
  if (quality == 3) val = siCDQuality;
  else if (quality == 2) val = siBestQuality;
  else if (quality == 1) val = siBetterQuality;
  else val = siGoodQuality;
  err = SPBSetDeviceInfo(refnum, siRecordingQuality, &val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_srate(long refnum)
{
  OSErr err;
  unsigned long fixed_srate;
  err = SPBGetDeviceInfo(refnum, siSampleRate, &fixed_srate);
  if (err == noErr) return(fixed_srate >> 16);
  return(-1);
}

static int input_device_set_srate(long refnum, int srate)
{
  OSErr err;
  unsigned long fixed_srate;
  fixed_srate = (unsigned long)(srate * 65536);
  err = SPBSetDeviceInfo(refnum, siSampleRate, &fixed_srate);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_sample_size(long refnum)
{
  OSErr err;
  short size;
  err = SPBGetDeviceInfo(refnum, siSampleSize, &size);
  if (err == noErr) return(size);
  return(-1);
}

static int input_device_set_sample_size(long refnum, short size)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siSampleSize, &size);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_signed(long refnum)
{ /* 0 = unsigned */
  OSErr err;
  short sign;
  err = SPBGetDeviceInfo(refnum, siSampleRate, &sign);
  if (err == noErr) return(sign);
  return(-1);
}

static int input_device_set_signed(long refnum, short sign)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siSampleRate, &sign);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_sample_rates(long refnum, int *range, int *rates)
{
  unsigned short num;
  int i, j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];                                            /* can't depend on C compiler to pack a struct correctly here */
  num = 0;
  err = SPBGetDeviceInfo(refnum, siSampleRateAvailable, pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0] << 8);                                 /* unsigned short is first element */
      if (num == 0) {(*range) = 1; num = 2;} else (*range) = 0;
      ptr = pp[5] + (pp[4] << 8) + (pp[3] << 16) + (pp[2] << 24); /* pointer to "fixed" table is second element */
      for (i = 0, j = 0; i < num; i++, j += 4)                          
        rates[i] = (*(unsigned short *)(j + (*(int *)ptr)));      /* ignore fraction -- this is dubious code */
    }
  return(num);
}

static int input_device_sample_sizes(long refnum, int *sizes)
{
  unsigned short num;
  int i, j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];
  num = 0;
  err = SPBGetDeviceInfo(refnum, siSampleSizeAvailable, pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0] << 8); 
      ptr = pp[5] + (pp[4] << 8) + (pp[3] << 16) + (pp[2] << 24);
      for (i = 0, j = 0; i < num; i++, j += 2) 
	sizes[i] = (*(unsigned short *)(j + (*(int *)ptr)));
    }
  return(num);
}

static int input_device_get_gains(long refnum, float *gains)
{
  OSErr err;
  long ptr[2];
  err = SPBGetDeviceInfo(refnum, siStereoInputGain, ptr);
  if (err == noErr)
    {
      gains[0] = (float)ptr[0] / 65536.0;
      gains[1] = (float)ptr[1] / 65536.0;
    }
  else return(-1);
  return(0);
}

static int input_device_set_gains(long refnum, float *gains)
{
  OSErr err;
  long val[2];
  val[0] = gains[0] * 65536;
  val[1] = gains[1] * 65536;
  err = SPBSetDeviceInfo(refnum, siStereoInputGain, val);
  return((err == noErr) ? 0 : -1); 
}

char *mus_audio_moniker(void)
{
  NumVersion nv;
  if (version_name == NULL) version_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  nv = SndSoundManagerVersion();
  mus_snprintf(version_name, LABEL_BUFFER_SIZE, "Mac audio: %d.%d.%d.%d\n", nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev);
  return(version_name);
}

static char *quality_names[5] = {"indescribable", "bad", "not bad", "ok", "good"};

static void describe_audio_state_1(void) 
{
  long response;
  NumVersion nv;
  OSErr err;
  int vals[64];
  float gains[2];
  int have_IO_mgr = 0, have_input_device = 0;
  unsigned char *devname = NULL;
  int i, j, devs, rates, range, sizes, connected;
  long refnum;
  Handle h;
  nv = SndSoundManagerVersion();
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Sound Manager: %d.%d.%d.%d\n", 
	  nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev);
  pprint(audio_strbuf);
  err = Gestalt(gestaltSoundAttr, &response);
  have_IO_mgr = (response & gestaltSoundIOMgrPresent);
  have_input_device = (response & gestaltHasSoundInputDevice);
  if (have_IO_mgr)
    {
      nv = SPBVersion();
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Sound Input Manager: %d.%d.%d.%d\n", 
	      nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev); 
      pprint(audio_strbuf);
     }
  if (!have_IO_mgr) pprint("Sound IO Manager absent!\n");
  if (!(response & gestaltBuiltInSoundInput)) pprint("no built-in input device!\n");
  if (!have_input_device) pprint("no input devices available!\n");
  if (!(response & gestaltSndPlayDoubleBuffer)) pprint("double buffering not supported!\n");
  if (response & gestalt16BitAudioSupport) pprint("has 16-bit audio ");
  if (response & gestalt16BitSoundIO) pprint("has 16-bit sound ");
  if (response & gestaltStereoInput) pprint("has stereo input\n");
  if (response & gestaltPlayAndRecord) pprint("can play and record simultaneously\n");
  if (response & gestaltMultiChannels) pprint("has multichannel support\n");
  GetSysBeepVolume(&response);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "beep vol: %.3f %.3f\n", 
	  ((float)(response >> 16)) / 255.0, 
	  ((float)(response & 0xffff)) / 255.0); 
  pprint(audio_strbuf);
  GetDefaultOutputVolume(&response);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "output vol: %.3f %.3f\n", 
	  ((float)(response >> 16)) / 255.0, 
	  ((float)(response & 0xffff)) / 255.0); 
  pprint(audio_strbuf);
  if ((have_IO_mgr) && 
      (have_input_device))
    { 
      devs = available_input_devices();
      if (devs > 0)
        {
          devname = (unsigned char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "input device%s:\n", (devs > 1) ? "s" : ""); 
	  pprint(audio_strbuf);
          for (i = 1; i <= devs; i++)
            {
              for (i = 1; i <= devs; i++)
                {
                  err = SPBGetIndexedDevice(i, devname, &h);
                  if (err == noErr)
                    {
                      err = SPBOpenDevice(devname, siWritePermission, &refnum);
                      if (err == noErr)
                        {
                          range = input_device_get_source(refnum);
                          connected = input_device_is_connected(refnum);
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s: %s%s",
                                  (*devname) ? devname : (unsigned char *)"un-named",
                                  ((input_device_get_async(refnum) == 1) ? "(async) " : ""),
                                  ((connected == siDeviceIsConnected) ? "" : 
                                   ((connected == siDeviceNotConnected) ? 
                                    "(not connected )" : "(might not be connected)")));
                        pprint(audio_strbuf);
                        if (range == 0) pprint("\n");
                        else
                                {
                                mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (source: %d)\n", range);
                                pprint(audio_strbuf);
                                }
                                mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    %d chans available, %d active\n",
                                  input_device_channels(refnum),
                                  input_device_get_channels(refnum));
                          pprint(audio_strbuf);

                          /* input_device_get_sources(refnum, NULL); */

                          range = 0;
                          rates = input_device_sample_rates(refnum, &range, vals);
                          if (rates > 1)
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    srates available:"); 
                              pprint(audio_strbuf);
                              if (range)
                                {
				  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%d to %d", vals[0], vals[1]); 
				  pprint(audio_strbuf);
				}
                              else
                                for (j = 0; j < rates; j++) 
				  {
				    mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %d", vals[j]); 
				    pprint(audio_strbuf);
				  }
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", current srate: %d\n",
                                      input_device_get_srate(refnum));
                              pprint(audio_strbuf);
                            }
                          else 
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    srate: %d\n", input_device_get_srate(refnum)); 
                              pprint(audio_strbuf);
                            }
                          err = input_device_get_quality(refnum);
                          if (err != -1) 
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    quality: %s\n",
				      quality_names[1 + input_device_get_quality(refnum)]);
                              pprint(audio_strbuf);
                            }
                          input_device_get_gains(refnum, gains);
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    gain: %.3f (%.3f %.3f)\n    sample: %s %d bits",
                                  input_device_get_gain(refnum), 
				  gains[0], gains[1],
                                  ((input_device_get_signed(refnum)) ? "signed" : "unsigned"),
                                  input_device_get_sample_size(refnum));
                          pprint(audio_strbuf);
                          sizes = input_device_sample_sizes(refnum, vals);
                          if (sizes > 0)
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (%d", vals[0]); 
			      pprint(audio_strbuf);
                              for (j = 1; j < sizes; j++) 
                                {
				  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", %d", vals[j]); 
				  pprint(audio_strbuf);
				}
                              pprint(" bit samples available)");
                            }
                          pprint("\n");
                          SPBCloseDevice(refnum);
                        }
                    }
                }
            }
          FREE(devname);
        }
    }
}

#define BUFFER_FILLED 1
#define BUFFER_EMPTY 2

#define SOUND_UNREADY 0
#define SOUND_INITIALIZED 1
#define SOUND_RUNNING 2

#define INPUT_LINE 1
#define OUTPUT_LINE 2

static int buffer_size = 1024;
static SndDoubleBufferPtr *db = NULL;
static SndDoubleBufferHeader dh;
static SndChannelPtr chan;
static int *db_state = NULL;
static int sound_state = 0;
static int current_chans = 1;
static int current_datum_size = 2;
static int current_srate = 22050;
static int current_buf = 0;
static long in_ref = -1;
static SPB spb;

#define DATA_EMPTY 0
#define DATA_READY 1
#define DATA_WRITTEN 2
static int data_status = DATA_EMPTY;
static int data_bytes = 0;
static char *data = NULL;

static pascal void nextbuffer(SndChannelPtr cp, SndDoubleBufferPtr db)
{
  db_state[current_buf] = BUFFER_EMPTY;
}

#define RETURN_ERROR_EXIT(Error_Type, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_type_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (false)

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  int dev;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (!db) db = (SndDoubleBufferPtr *)CALLOC(2, sizeof(SndDoubleBufferPtr));
  if (!db_state) db_state = (int *)CALLOC(2, sizeof(int));
  chan = nil;
  err = SndNewChannel(&chan, sampledSynth, 0, nil);
  if (err != noErr) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
		      mus_format("can't get new output channel for %d (%s): error %d", /* geez-louise!! Is anything dumber than a goddamn Mac? */
				 dev, 
				 mus_audio_device_name(dev), err));
  dh.dbhNumChannels = chans;
  current_chans = chans;
  if (format == MUS_UBYTE) 
    {
      dh.dbhSampleSize = 8; 
      current_datum_size = 1;
    }
  else 
    {
      dh.dbhSampleSize = 16;
      current_datum_size = 2;
    }
  dh.dbhCompressionID = 0; 
  dh.dbhPacketSize = 0; 
  dh.dbhSampleRate = (srate << 16);
  dh.dbhDoubleBack = NewSndDoubleBackProc(nextbuffer);
  if (size <= 0) buffer_size = 1024; else buffer_size = size;
  db[0] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size, sizeof(char));
  if ((db[0] == nil) || (MemError() != 0)) 
    {
      SndDisposeChannel(chan, 0); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate output buffer, size %d, for %d (%s)",
				   buffer_size, dev, 
				   mus_audio_device_name(dev)));
    }
  dh.dbhBufferPtr[0] = db[0];   
  db[0]->dbNumFrames = 0;
  db[0]->dbFlags = 0;
  db_state[0] = BUFFER_EMPTY;
  db[1] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size, sizeof(char));
  if ((db[1] == nil) || (MemError() != 0)) 
    {
      FREE(db[0]); 
      SndDisposeChannel(chan, 0); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate output buffer, size %d, for %d (%s)",
				   buffer_size, dev, 
				   mus_audio_device_name(dev)));
    }
  dh.dbhBufferPtr[1] = db[1];   
  db[1]->dbNumFrames = 0;
  db[1]->dbFlags = 0;
  db_state[1] = BUFFER_EMPTY;
  sound_state = SOUND_INITIALIZED;
  current_buf = 0;
  return(OUTPUT_LINE);
}

static OSErr fill_buffer(int dbi, char *inbuf, int instart, int bytes)
{
  int i, j;
  OSErr err;
  err = noErr;
  for (i = instart, j = 0; j < bytes; j++, i++) 
    db[dbi]->dbSoundData[j] = inbuf[i];
  db_state[dbi] = BUFFER_FILLED;
  db[dbi]->dbFlags = (db[dbi]->dbFlags | dbBufferReady);
  db[dbi]->dbNumFrames = (bytes / (current_chans * current_datum_size));
  if ((sound_state == SOUND_INITIALIZED) && (dbi == 1))
    {
      sound_state = SOUND_RUNNING;
      err = SndPlayDoubleBuffer(chan, &dh);
    }
  return(err);
}

static OSErr wait_for_empty_buffer(int buf)
{
  SCStatus Stats;
  OSErr err;
  err = noErr;
  while (db_state[buf] != BUFFER_EMPTY)
    {
      err = SndChannelStatus(chan, sizeof(Stats), &Stats);
      if ((err != noErr) || 
	  (!(Stats.scChannelBusy))) 
	break;
    }
  return(err);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  OSErr err;
  int lim, leftover, start;
  if (line != OUTPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		      mus_format("write error: line %d != %d",
				 line, OUTPUT_LINE));
  leftover = bytes;
  start = 0;
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      err = wait_for_empty_buffer(current_buf);
      if (err != noErr) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error during wait on line %d: error %d",
				     line, err));
      err = fill_buffer(current_buf, buf, start, lim);
      if (err != noErr) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error during fill on line %d: error %d",
				     line, err));
      start += lim;
      current_buf++;
      if (current_buf > 1) current_buf = 0;
    }
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line) 
{
  OSErr err;
  int i;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i = 0; i < 128; i++) 
	    db[current_buf]->dbSoundData[i] = 0;
          db[current_buf]->dbFlags = (db[current_buf]->dbFlags | dbBufferReady | dbLastBuffer);
          db[current_buf]->dbNumFrames = (128 / (current_chans * current_datum_size));
          wait_for_empty_buffer(current_buf);
          FREE(db[0]); 
	  db[0] = NULL;
          FREE(db[1]); 
	  db[1] = NULL;
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          err = SndDisposeChannel(chan, 0);
	  /* this is the line that forced me to use FREE/CALLOC throughout! */
          if (err != noErr) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			      mus_format("can't close output: error %d",
					 err));
        }
    }
  else
    {
      if (line == INPUT_LINE)
        {
          if (in_ref != -1)
            {
              data_status = DATA_EMPTY;
              SPBStopRecording(in_ref);
              if (spb.bufferPtr) FREE(spb.bufferPtr);
              SPBCloseDevice(in_ref);
              in_ref = -1;
            }
        }
      else 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			  mus_format("can't close unrecognized line: %d",
				     line));
    }
  return(MUS_NO_ERROR);
}

static void read_callback(SPB *spb)
{
  int i, lim;
  if (data_status != DATA_EMPTY)
    {
      if (data_bytes > spb->bufferLength) 
	lim = spb->bufferLength; 
      else lim = data_bytes;
      for (i = 0; i < lim; i++) 
	data[i] = spb->bufferPtr[i]; 
      spb->bufferLength = data_bytes;
      SPBRecord(spb, true);
      data_status = DATA_WRITTEN;
    }
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  short source;
  int dev;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  data_status = DATA_EMPTY;
  if (size <= 0) size = 1024;
  if (in_ref != -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
		      mus_format("previous input not closed? line: %d",
				 in_ref));
  err = SPBOpenDevice((unsigned char *)"", siWritePermission, &in_ref);
  if (err != noErr) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
		      mus_format("can't open input %d (%s): error %d",
				 dev, mus_audio_device_name(dev), err));
  spb.inRefNum = in_ref;
  spb.count = size;
  source = 3; /* the microphone ?? (2: CD, 4: modem, 0: none) -- nowhere is this documented! */
  input_device_set_source(in_ref, source);
  input_device_set_srate(in_ref, srate);
  input_device_set_channels(in_ref, (short)chans);
  input_device_set_sample_size(in_ref, (format == MUS_BSHORT) ? 2 : 1);
  input_device_set_signed(in_ref, (format == MUS_BSHORT) ? 1 : 0);
  spb.milliseconds = (int)((float)(size * 1000) / (float)(((format == MUS_BSHORT) ? 2 : 1) * srate));
  spb.bufferLength = size;
  spb.bufferPtr = (char *)CALLOC(size, sizeof(char));
  spb.completionRoutine = NewSICompletionProc(read_callback);
  err = SPBRecord(&spb, true);
  return(INPUT_LINE);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  OSErr err;
  unsigned long total_samps, num_samps, total_msecs, num_msecs;
  short level, status;
  if (line != INPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ,
		      mus_format("can't read from unrecognized line: %d",
				 line));
  data_status = DATA_READY;
  data_bytes = bytes;
  data = buf;
  while (data_status == DATA_READY)
    {
      err = SPBGetRecordingStatus(in_ref, &status, &level, &total_samps, &num_samps, &total_msecs, &num_msecs);
      if ((err != noErr) || (status <= 0)) break; /* not necessarily an error */
    }
  return(0);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  long response;
  int dev, our_err = MUS_NO_ERROR;
  float in_val[2];
  int our_open = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_CHANNEL:
      val[0] = 2;
      break;
    case MUS_AUDIO_PORT:
      val[0] = 2; 
      if (chan > 1) val[1] = MUS_AUDIO_MICROPHONE; 
      if (chan > 2) val[2] = MUS_AUDIO_DAC_OUT;
      break;
    case MUS_AUDIO_FORMAT:
      val[0] = 2; 
      if (chan > 1) val[1] = MUS_BSHORT;
      if (chan > 2) val[2] = MUS_UBYTE;
      break;
    default:
      switch (dev)
        {
        case MUS_AUDIO_DEFAULT:
        case MUS_AUDIO_DAC_OUT:
        case MUS_AUDIO_SPEAKERS:
        case MUS_AUDIO_LINE_OUT:
          switch (field)
            {
            case MUS_AUDIO_AMP: 
              GetDefaultOutputVolume(&response);
              if (chan == 0)
                val[0] = ((float)(response >> 16)) / 255.0;
              else val[0] = ((float)(response & 0xffff)) / 255.0;
              break;
            case MUS_AUDIO_CHANNEL: 
	      val[0] = 2;
	      break;
            case MUS_AUDIO_SRATE: 
	      val[0] = current_srate;
	      break;
            }
          break;
        case MUS_AUDIO_MICROPHONE:
        case MUS_AUDIO_LINE_IN:
          if (in_ref == -1)
            {
              err = SPBOpenDevice((const unsigned char *)"", siWritePermission, &in_ref);
              if (err != noErr) 
		RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
				  mus_format("can't open %d (%s): error %d",
					     dev, 
					     mus_audio_device_name(dev), err));
              our_open = 1;
            }
          switch (field)
            {
            case MUS_AUDIO_AMP:
              err = input_device_get_gains(in_ref, in_val);
              if (chan == 0) val[0] = in_val[0]; else val[0] = in_val[1];
              break;
            case MUS_AUDIO_CHANNEL:
              val[0] = input_device_get_channels(in_ref);
              break;
            case MUS_AUDIO_SRATE:
              val[0] = input_device_get_srate(in_ref);
              break;
            default:
	      our_err = MUS_ERROR;
	      break;
            }
          if (our_open)
            {
              SPBCloseDevice(in_ref);
              in_ref = -1;
            }
          break;
        default: 
	  our_err = MUS_ERROR;
	  break;
        }
    }
  if (our_err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ,
		      mus_format("can't read %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  float out_val[2];
  long curval, newval;
  int amp, our_open, dev, our_err = MUS_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          amp = (int)(255 * val[0]);
          GetDefaultOutputVolume(&curval);
          if (chan == 0)
            newval = ((curval & 0xffff0000) | (amp & 0xffff));
          else newval = (((amp << 16) & 0xffff0000) | (curval & 0xffff));
          SetDefaultOutputVolume(newval);
          break;
        case MUS_AUDIO_CHANNEL: 
        case MUS_AUDIO_SRATE: break;
        default: 
	  our_err = MUS_ERROR;
	  break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
      if (in_ref == -1)
        {
          err = SPBOpenDevice((const unsigned char *)"", siWritePermission, &in_ref);
          if (err != noErr) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
			      mus_format("can't open %d (%s): error %d",
					 dev, 
					 mus_audio_device_name(dev), err));
          our_open = 1;
        }
      switch (field)
        {
        case MUS_AUDIO_AMP:
          input_device_get_gains(in_ref, out_val);
          if (chan == 0) out_val[0] = val[0]; else out_val[1] = val[0];
          err = input_device_set_gains(in_ref, out_val);
          break;
        case MUS_AUDIO_CHANNEL:
          err = input_device_set_channels(in_ref, (int)val[0]);
          break;
        case MUS_AUDIO_SRATE:
          err = input_device_set_srate(in_ref, (int)val[0]);
          break;
        default:
	  our_err = MUS_ERROR;
	  break;
        }
      if (our_open)
        {
          SPBCloseDevice(in_ref);
          in_ref = -1;
        }
      break;
    default: 
      our_err = MUS_ERROR;
      break;
    }
  if (our_err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		      mus_format("can't set %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));
  return(MUS_NO_ERROR);
}

int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}

#endif


