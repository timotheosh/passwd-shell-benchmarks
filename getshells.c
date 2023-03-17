// For memrchr()
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdint.h>
// For mmap()
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
// For assert()
#include <assert.h>

#define INT_TYPE int64_t

static __always_inline char *itoa(INT_TYPE value, char *result) {
  int i = 0;
  while (value) {
    result[9 - i++] = (value % 10) + '0';
    value /= 10;
  }
  result[10] = '\n';
  result[11] = '\0';
  return result + (10 - i);
}

// no padding in struct cuz 8bytes fo pointer, 8bytes for integer
struct shell {
  char *name;
  INT_TYPE count;
};

// since it is a global, it is initialized to zero
static struct shell shells[128];

int main() {

  // mmap a file to memory for reading
  #ifdef FUNC_TEST
  int fd = open("shells-list", O_RDONLY);
  #else
  int fd = open("passwd", O_RDONLY);
  #endif
  struct stat s;
  fstat(fd, &s);
  char *buffer = mmap(0, s.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

  size_t totalShells = 0;

  while (1) {
    char *newLine = memchr(buffer, '\n', 100);
    const ptrdiff_t totalLineSize = newLine - buffer;
  	if (totalLineSize <= 0)
	  	break;
	  char *colon = memrchr(buffer, ':', totalLineSize - 6) + 1;
	  buffer = newLine+1;
    
	  const ptrdiff_t shellSize = newLine - colon;
  
    // @crumbtoo suggested replacing modulus with & operation
    int id = (colon[shellSize - 3] ^ (shellSize + colon[shellSize-4])) & 0xabcdff;
  
    if (0 == shells[id].name) {
      	shells[id].name = colon;
		    colon[shellSize] = '\t';
		    colon[shellSize+1] = '\0';
		    totalShells++;
        #ifdef FUNC_TEST
        assert(shells[id].count == 0);
        printf("DEBUG: %s%d\n", colon, id);
        #endif
    }
    shells[id].count++;
  }

// Output
  for (int i = 0; i < 100; i++) {
    struct shell s = shells[i];
    if (s.count > 0) {
      fputs_unlocked(s.name, stdout);
      #ifdef FUNC_TEST
      fputs_unlocked(itoa(s.count, (void*)shells), stdout);
      printf("\t%s%d\n\n",s.name,s.count);
      #else
      fputs_unlocked(itoa(s.count, (void*)shells), stdout);
      #endif
      if (!--totalShells)
        break;
    }
  }

  munmap(buffer, s.st_size);
  close(fd);
}
