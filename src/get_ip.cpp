#include <Rcpp.h>
#include <stdio.h>
#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <string.h>
#include <arpa/inet.h>

//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector get_ipv4() {
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

/*** R
get_ipv4()
*/
