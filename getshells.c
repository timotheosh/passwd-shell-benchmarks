#define _GNU_SOURCE
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stddef.h>
#include <sys/cdefs.h>
// For mmap()
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

static __always_inline char *itoa(unsigned short value, char *result) {
  int i = 0;
  while (value) {
    result[9 - i++] = (value % 10) + '0';
    value /= 10;
  }
  result[10] = '\0';
  return result + (10 - i);
}

struct shell {
  char *name;
  int count;
};

struct shell shells[128];

int main() {

  // FILE *fp = fopen("passwd", "r");
  int fd = open("passwd", O_RDONLY);
  struct stat s;
  fstat(fd, &s);
  size_t fileSize = s.st_size;
  char *buffer = mmap(0, fileSize, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

  // size_t lineSize = 0;
  // size_t bufSize = 1024;

  size_t ts = 0;

  while (1) {
    char *newLine = memchr(buffer, '\n', 100);
    ptrdiff_t totalLineSize = newLine - buffer;
	if (totalLineSize <= 0)
		break;
	char *colon = memrchr(buffer, ':', totalLineSize - 6) + 1;
	buffer = newLine+1;
    
	ptrdiff_t shellSize = newLine - colon;
   
    // new id generater by @crumbtoo
    int id = (colon[shellSize - 3] ^ shellSize + colon[shellSize-4]) & 0xabcdff; // 0x63 had collissions... so I changed the constant
	// printf("%s %d %d %c %c\n", strndup(colon, shellSize), shellSize, id, colon[shellSize-3], colon[1]);
	// positions once I figure out the math
    shells[id].count++;
    if (0 == shells[id].name) {
      	shells[id].name = colon;
		colon[shellSize] = '\t';
		colon[shellSize+1] = '\0';
		ts++;
    }
  }

  for (int i = 0; i < 100; i++) {
    struct shell s = shells[i];
    if (s.count > 0) {
      // fputs_unlocked(s.name, stdout);
      // fputs_unlocked(itoa(s.count, shells), stdout);
      // putchar_unlocked('\n');
      fputs(s.name, stdout);
      puts(itoa(s.count, (void*)shells));
      if (!--ts)
        break;
    }
  }

  // free(shells);
  munmap(buffer, s.st_size);
  close(fd);
  // sbrk(-4096);
  // fclose(fp);
}
