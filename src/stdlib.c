#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

int copy(char *src, char *dest)
{
	int status;
	pid_t pid = fork();
	/* Child proc */
	if (pid == 0) {
		char *const args[] = {"/bin/cp", src, dest, NULL};
		execv("/bin/cp", args);

		/* Child should not reach this point */
		fprintf(stderr, "error: %s\n", strerror(errno));
		exit(1);
	}
	/* Parent proc */
	else {
		wait(&status);
		return WEXITSTATUS(status);
	}
	return -1;
}

#ifdef DEBUG
int main(int argc, char **argv)
{
	copy("./test.txt", "./test2.txt");
	return 0;
}
#endif
