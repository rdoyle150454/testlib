#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/utsname.h>

int main(void) {

   struct utsname buffer;

   errno = 0;
   if (uname(&buffer) != 0) {
      switch (errno) {
         case EFAULT:
            fprintf(stderr, "call to uname() with an invalid pointer\n");
            break;
         default:
            fprintf(stderr, "error calling uname()\n");
      }
      exit(EXIT_FAILURE);
   }

   printf("system name = %s\n", buffer.sysname);
   printf("node name   = %s\n", buffer.nodename);
   printf("release     = %s\n", buffer.release);
   printf("version     = %s\n", buffer.version);
   printf("machine     = %s\n", buffer.machine);

   #ifdef _GNU_SOURCE
      printf("domain name = %s\n", buffer.domainname);
   #endif

   return EXIT_SUCCESS;
}
