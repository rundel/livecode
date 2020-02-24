#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#if defined(WIN32) || defined(_WIN32)

// From https://stackoverflow.com/questions/11588765/using-rcpp-with-windows-specific-includes
#undef Realloc
#undef Free

#include <winsock2.h>
#include <windows.h>
#include <iphlpapi.h>
#include <Ws2tcpip.h>

#define WORKING_BUFFER_SIZE 15000

std::string wide_to_str(wchar_t* pws) {
  std::wstring ws(pws);

  return std::string(ws.begin(), ws.end());
}

std::string sock_addr_to_str(SOCKET_ADDRESS addr) {
  char buf[INET_ADDRSTRLEN];

  memset(buf, 0, INET_ADDRSTRLEN);
  getnameinfo(addr.lpSockaddr, addr.iSockaddrLength, buf, sizeof(buf), NULL, 0, NI_NUMERICHOST);

  return std::string(buf);
}


Rcpp::CharacterVector get_ipv4_impl() {
  Rcpp::CharacterVector ifaces, addrs, descs;

  unsigned long int buf_len = 0;
  GetAdaptersAddresses(AF_INET, 0, NULL, NULL, &buf_len);
  if (buf_len == 0)
    Rcpp::stop("Retriving network adaptors failed!");

  std::vector<char> mem(buf_len);
  PIP_ADAPTER_ADDRESSES adapts = (PIP_ADAPTER_ADDRESSES) mem.data();

  unsigned long int res = GetAdaptersAddresses(AF_INET, 0, NULL, adapts, &buf_len);
  if (res != NO_ERROR)
    Rcpp::stop("Retriving network adaptors failed!");

  while (adapts != NULL) {
    ifaces.push_back( wide_to_str(adapts->FriendlyName) );
    addrs.push_back( sock_addr_to_str(adapts->FirstUnicastAddress->Address) );
    descs.push_back( wide_to_str(adapts->Description));

    adapts = adapts->Next;
  };

  addrs.attr("names") = ifaces;
  addrs.attr("descs") = descs;
  return addrs;
}

#else

#include <stdio.h>
#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <string.h>
#include <arpa/inet.h>

Rcpp::CharacterVector get_ipv4_impl() {
  Rcpp::CharacterVector ifaces;
  Rcpp::CharacterVector addrs;

  char addr_buffer[INET_ADDRSTRLEN];

  ifaddrs *ifAddrStruct = NULL;
  getifaddrs(&ifAddrStruct);

  for (ifaddrs *ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
    if (!ifa->ifa_addr) {
      continue;
    }

    if (ifa->ifa_addr->sa_family == AF_INET) {
      inet_ntop(AF_INET, &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr,
                addr_buffer, INET_ADDRSTRLEN);
      ifaces.push_back(ifa->ifa_name);
      addrs.push_back(addr_buffer);
    }
  }

  if (ifAddrStruct!=NULL)
    freeifaddrs(ifAddrStruct);

  addrs.attr("names") = ifaces;

  return addrs;
}

#endif


// [[Rcpp::export]]
Rcpp::CharacterVector get_ipv4() {
  return get_ipv4_impl();
}
/*** R
get_ipv4()
*/
