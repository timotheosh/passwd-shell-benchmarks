#include <stdio.h>
#include <string.h>

// demo program to read a unix password file and show
// how many instances of each login shell are found
// optimized 20260527 - jjs

#define MAX_SHELLS 64
#define SHELL_LEN 64

int main(void) {
    FILE *fp1 = fopen("passwd", "r");
    if (!fp1) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    char shells[MAX_SHELLS][SHELL_LEN] = {0};
    int shellcnt[MAX_SHELLS] = {0};
    int numshells = 0;

    while (fgets(line, sizeof(line), fp1)) {
        // Strip trailing newline character immediately
        line[strcspn(line, "\n")] = '\0';

        // Find the last colon directly
        char *shell_ptr = strrchr(line, ':');
        if (!shell_ptr) continue; 
        shell_ptr++; // Move past the ':'

        // Search if shell already exists in our table
        int found = 0;
        for (int k = 0; k < numshells; k++) {
            if (strcmp(shells[k], shell_ptr) == 0) {
                shellcnt[k]++;
                found = 1;
                break; // Stop searching once found
            }
        }

        // If it's a new shell, add it safely
        if (!found && numshells < MAX_SHELLS) {
            strncpy(shells[numshells], shell_ptr, SHELL_LEN - 1);
            shellcnt[numshells] = 1;
            numshells++;
        }
    }
    fclose(fp1);

    // Display shell tally
    for (int i = 0; i < numshells; i++) {
        printf("%-18s:\t%d\n", shells[i], shellcnt[i]);
    }

    return 0;
}

