#include <stdio.h>
#include <unistd.h>

extern int _llp_builtin__stdout;

int _llp_builtin__write_str(int fd, char* str);
