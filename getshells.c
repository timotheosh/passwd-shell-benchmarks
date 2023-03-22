// For memrchr()
#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
// For mmap()
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
// For assert()
#include <assert.h>
// Threading
#include <pthread.h>

#define INT_TYPE int64_t

struct chunk {
  const char *start;
  struct shell *output;
  size_t size;
};

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
static struct shell shellsBucket[128 * 4];

static void process(struct chunk *chunk) {
  const char *buffer = chunk->start;
  struct shell *shells = chunk->output;
  size_t size = chunk->size;
  const char *end = buffer + size;
  while (buffer != end) {
    char *newLine = (char *)memchr(buffer, '\n', 100);
    const ptrdiff_t totalLineSize = newLine - buffer;
    if (totalLineSize <= 0)
      break;
    char *colon = (char *)memrchr(buffer, ':', totalLineSize - 6) + 1;
    buffer = newLine + 1;

    const ptrdiff_t shellSize = newLine - colon;

    // @crumbtoo suggested replacing modulus with & operation
    int id =
        (colon[shellSize - 3] ^ (shellSize + colon[shellSize - 4])) & 0xabcdff;

    // shellLock.lock();
    if (0 == shells[id].name) {
      shells[id].name = colon;
      colon[shellSize] = '\t';
      colon[shellSize + 1] = '\0';
      // totalShells++;
#ifdef FUNC_TEST
      assert(shells[id].count == 0);
      printf("DEBUG: %s%d\n", colon, id);
#endif
    }
    shells[id].count++;
    // shellLock.unlock();
  }
}

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

  const size_t chunkSize = s.st_size / 4;

// {{{
  const char *chunk2Start =
      1 + (char *)memchr(buffer + 1 * chunkSize, '\n', 100);
  const char *chunk3Start =
      1 + (char *)memchr(buffer + 2 * chunkSize, '\n', 100);
  const char *chunk4Start =
      1 + (char *)memchr(buffer + 3 * chunkSize, '\n', 100);
  const size_t chunk1Size = chunk2Start - buffer;
  const size_t chunk2Size = chunk3Start - chunk2Start;
  const size_t chunk3Size = chunk4Start - chunk3Start;
  const size_t chunk4Size = buffer + s.st_size - chunk4Start;
// }}}

  const struct chunk c1 = {buffer, shellsBucket, chunk1Size};
  const struct chunk c2 = {chunk2Start, &shellsBucket[128], chunk2Size};
  const struct chunk c3 = {chunk3Start, &shellsBucket[256], chunk3Size};
  const struct chunk c4 = {chunk4Start, &shellsBucket[384], chunk4Size};

#define TC(i)                                                                  \
  pthread_t thread##i;                                                         \
  pthread_create(&thread##i, NULL, process, (void *)&c##i);
#define TJ(i) pthread_join(thread##i, NULL);
  TC(1);
  TC(2);
  TC(3);
  TC(4);
  TJ(1);
  TJ(2);
  TJ(3);
  TJ(4);
#undef TC
#undef TJ
  // Output
  setvbuf(stdout, NULL, _IOFBF, BUFSIZ);
  for (int i = 0; i < 100; i++) {
    struct shell s = shellsBucket[i];
    if (s.count > 0) {
      fputs_unlocked(s.name, stdout);
      fputs_unlocked(itoa(s.count + shellsBucket[i + 128].count +
                              shellsBucket[i + 256].count +
                              shellsBucket[i + 384].count,
                          (void *)shellsBucket),
                     stdout);
#ifdef FUNC_TEST
      printf("\t%s%d\n\n", s.name, s.count);
#endif
      // if (!--totalShells)
      // break;
    }
  }

  fflush(stdout);
  munmap(buffer, s.st_size);
  close(fd);
}
