(c-declare "#include <string.h>")
(c-declare "#include <stdlib.h>")
(c-declare "#include <unistd.h>")
(c-declare "#include <fcntl.h>")
(c-declare "#include <errno.h>")
(c-declare "#include <sys/types.h>")
(c-declare "
#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
// # include <afunix.h>

/*
 * MinGW does not have sockaddr_un (yet)
 */

# ifndef UNIX_PATH_MAX
#  define UNIX_PATH_MAX 108
struct sockaddr_un {
  ADDRESS_FAMILY sun_family;
  char sun_path[UNIX_PATH_MAX];
};
# endif
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#endif
")
