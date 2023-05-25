#include <stdio.h>
#include <stdlib.h>

#define SHELL_BUF_SIZE 1024
void shell(char *cmd)
{
  /* cmd as a system call */
  FILE *fd = NULL;
  char *buf = NULL;
  if ((fd = popen(cmd,"r")) != NULL)
    {
      buf = (char *)calloc(SHELL_BUF_SIZE,sizeof(char));
      while ((fgets(buf,SHELL_BUF_SIZE,fd)) != NULL)
	fprintf(stderr,buf);
      free(buf);
      pclose(fd);
    }
}
/*
   cc shell.c -c
   ld -shared -o shell.so shell.o
*/