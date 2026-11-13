#include <Rcpp.h>
#include <random>
#include <string>

// A random hexadecimal string of `bytes` bytes from the operating system's
// random source, not R's seeded generator: it guards a local page.
// [[Rcpp::export]]
std::string random_token(int bytes = 16) {
  static const char hex[] = "0123456789abcdef";
  std::random_device source;
  std::string out;
  out.reserve(bytes * 2);
  for (int i = 0; i < bytes; i++) {
    unsigned int value = source() & 0xFF;
    out.push_back(hex[value >> 4]);
    out.push_back(hex[value & 0x0F]);
  }
  return out;
}
