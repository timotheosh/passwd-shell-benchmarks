#include <stdio.h>
#include <string.h>

#define MAX_SHELLS 64
#define SHELL_LEN 64
#define LINE_LEN 256
#define IO_BUFFER_SIZE 65536

int main(void)
{
    FILE *fp = fopen("passwd", "r");
    if (fp == NULL) {
        perror("passwd");
        return 1;
    }

    static char io_buffer[IO_BUFFER_SIZE];

    if (setvbuf(fp, io_buffer, _IOFBF, sizeof(io_buffer)) != 0) {
        perror("setvbuf");
        fclose(fp);
        return 1;
    }

    char line[LINE_LEN];
    char shells[MAX_SHELLS][SHELL_LEN] = {{0}};
    unsigned int shell_counts[MAX_SHELLS] = {0};
    size_t shell_count = 0;

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *shell = strrchr(line, ':');

        if (shell == NULL) {
            continue;
        }

        shell++;

        /*
         * The newline, when present, must be somewhere after the final colon.
         * Searching only the shell field avoids scanning the whole line again.
         */
        char *newline = strchr(shell, '\n');
        if (newline != NULL) {
            *newline = '\0';
        }

        size_t i;

        for (i = 0; i < shell_count; i++) {
            if (strcmp(shells[i], shell) == 0) {
                shell_counts[i]++;
                break;
            }
        }

        if (i == shell_count) {
            if (shell_count >= MAX_SHELLS) {
                fprintf(stderr, "Too many distinct shells\n");
                fclose(fp);
                return 1;
            }

            size_t length = strlen(shell);

            if (length >= SHELL_LEN) {
                fprintf(stderr, "Shell name too long: %s\n", shell);
                fclose(fp);
                return 1;
            }

            memcpy(shells[shell_count], shell, length + 1);
            shell_counts[shell_count] = 1;
            shell_count++;
        }
    }

    if (ferror(fp)) {
        perror("Error reading passwd");
        fclose(fp);
        return 1;
    }

    fclose(fp);

    for (size_t i = 0; i < shell_count; i++) {
        printf("%-18s:\t%u\n", shells[i], shell_counts[i]);
    }

    return 0;
}
