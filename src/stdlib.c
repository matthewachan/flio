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
		/* Syscall interrupt */ 
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

int move(char *src, char *dest)
{
	int status;
	pid_t pid = fork();
	/* Child proc */
	if (pid == 0) {
		char *const args[] = {"/bin/cp", src, dest, NULL};
		/* Syscall interrupt */ 
		execv("/bin/mv", args);

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

ssize_t bwrite(FILE *f, const char *buf)
{
	int fd = fileno(f);
	return write(fd, buf, strlen(buf));
}

char *bread(FILE *f, size_t count)
{
	char *buf = malloc(sizeof(char) * count);
	fgets(buf, count + 1, f);
	return buf;
}

char *readLine(FILE *f)
{
	return bread(f, 200);	
}

int appendString(const char *f, const char *buf)
{
	
	FILE *file = fopen(f, "a");
	int ret = bwrite(file, buf);
	fclose(file);
	return ret;
}

char *concat(const char *s1, const char *s2)
{
	char *c = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(c, s1);
	strcat(c, s2);
	return c;
}

#ifdef DEBUG
int main(int argc, char **argv)
{
	/* copy("./test.txt", "./test2.txt"); */
	/* move("./test.txt", "./test3.txt"); */
	/* FILE *f = fopen("test3.txt", "r+"); */
	/* bwrite(fd, "hi there"); */
	/* char *s = bread(fd, 2); */
	/* char *s; */
	/* size_t len; */
	/* getline(&s, &len, fd); */
	
	/* printf("%s", readLine(f)); */
	return 0;
}
#endif
