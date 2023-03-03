#include "libllpoly.h"


int _llp_builtin__stdout = 1;

int _llp_builtin__write_str(int fd, char *str)
{
    int i = 0;
    while (str[i] != '\0')
    {
        write(fd, &str[i], 1);
        i++;
    }
    return 0;
}
