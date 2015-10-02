/* Sigh. According to the docs for <arpa/inet.h>, htonl() and friends may be
   implemented as macros only, so I have to re-export functions here to do the
   conversions. */

#include <HsIoStreamsHaProxyConfig.h>

#if defined(HAVE_WINSOCK2_H) && !defined(__CYGWIN__)
# include <winsock2.h>
# include <inttypes.h>

#else

#ifdef HAVE_ARPA_INET_H
#  include <arpa/inet.h>
#endif

#endif

uint32_t iostreams_htonl(uint32_t hostlong) {
    return htonl(hostlong);
}

uint16_t iostreams_htons(uint16_t hostshort) {
    return htons(hostshort);
}

uint32_t iostreams_ntohl(uint32_t netlong) {
    return ntohl(netlong);
}

uint16_t iostreams_ntohs(uint16_t netshort) {
    return ntohs(netshort);
}
