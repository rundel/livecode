# From https://en.wikipedia.org/wiki/Reserved_IP_addresses

reserved_blocks = c(
  "0.0.0.0/8",          # Current network (only valid as source address)
  "10.0.0.0/8",         # Private network
  "100.64.0.0/10",      # Shared address space (carrier-grade NAT)
  "127.0.0.0/8",        # Loopback
  "169.254.0.0/16",     # Link-local
  "172.16.0.0/12",      # Private network
  "192.0.0.0/24",       # IETF protocol assignments
  "192.0.2.0/24",       # Documentation (TEST-NET-1)
  "192.88.99.0/24",     # Formerly 6to4 relay anycast
  "192.168.0.0/16",     # Private network
  "198.18.0.0/15",      # Benchmark testing
  "198.51.100.0/24",    # Documentation (TEST-NET-2)
  "203.0.113.0/24",     # Documentation (TEST-NET-3)
  "224.0.0.0/4",        # Multicast
  "240.0.0.0/4",        # Reserved for future use
  "255.255.255.255/32"  # Limited broadcast
)

loopback_blocks = c(
  "127.0.0.0/8"
)

# Blocks that are only reachable from the same local (or carrier / overlay)
# network, which is what the private warning is about.
private_blocks = c(
  "10.0.0.0/8",
  "100.64.0.0/10",
  "172.16.0.0/12",
  "192.168.0.0/16"
)

is_ipv4 = function(ip) {
  ip = as.character(ip)
  ok = !is.na(ip) & grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", ip)
  ok[ok] = vapply(
    strsplit(ip[ok], ".", fixed = TRUE),
    function(octets) all(as.integer(octets) <= 255L),
    logical(1)
  )
  ok
}

is_ipv6 = function(ip) {
  ip = as.character(ip)
  !is.na(ip) & grepl("^\\[?[0-9A-Fa-f]*:[0-9A-Fa-f:.]*(%[^]]+)?\\]?$", ip)
}

# Doubles represent every 32-bit value exactly, R integers do not.
ipv4_to_num = function(ip) {
  res = rep(NA_real_, length(ip))
  ok = is_ipv4(ip)
  res[ok] = vapply(
    strsplit(ip[ok], ".", fixed = TRUE),
    function(octets) sum(as.numeric(octets) * 256^(3:0)),
    numeric(1)
  )
  res
}

ip_in_block = function(ip, block) {
  parts = strsplit(block, "/", fixed = TRUE)[[1]]
  prefix = as.integer(parts[2])
  size = 2^(32 - prefix)
  base = ipv4_to_num(parts[1])
  x = ipv4_to_num(ip)
  !is.na(x) & floor(x / size) == floor(base / size)
}

ip_in_any = function(ip, blocks) {
  res = rep(FALSE, length(ip))
  for (block in blocks) {
    res = res | ip_in_block(ip, block)
  }
  res
}

is_ip_loopback = function(ip) {
  ip_in_any(ip, loopback_blocks)
}

is_ip_private = function(ip) {
  ip_in_any(ip, private_blocks)
}

is_ip_reserved = function(ip) {
  ip_in_any(ip, reserved_blocks)
}

ip_type = function(ip) {
  type = rep("public", length(ip))

  type[is_ip_reserved(ip)] = "reserved"
  type[is_ip_private(ip)] = "private"
  type[is_ip_loopback(ip)] = "loopback"
  type[!is_ipv4(ip)] = NA

  # ordered based on preference heuristic
  factor(type, levels = c("public", "private", "reserved", "loopback"))
}

virtual_iface_regex = paste0(
  "^(docker|br-|bridge|veth|virbr|vboxnet|vmnet|utun|tun|tap|wg|tailscale|zt)"
)

virtual_iface_regex_windows = paste0(
  "vethernet|hyper-v|\\btap\\b|tap-|virtual|wsl|tailscale|wireguard|zerotier"
)

iface_type = function(iface) {
  iface = stringi::stri_enc_toutf8(iface)
  iface = tolower(iface)
  type = rep("other", length(iface))

  if (.Platform$OS.type != "windows") {
    # Based on "Predictable Network Interface Device Names" plus legacy ethN
    type[grepl("^(en|eth)", iface)] = "ethernet"
    type[grepl("^(wl|ww)", iface)] = "wireless"
    type[grepl(virtual_iface_regex, iface)] = "virtual"
    type[grepl("^lo", iface)] = "loopback"
  } else {
    # No clear naming convention, so match on the name and description
    type[grepl("ethernet", iface)] = "ethernet"
    type[grepl("wi(-)?fi|wireless|wlan", iface)] = "wireless"
    type[grepl(virtual_iface_regex_windows, iface)] = "virtual"
    type[grepl("loopback", iface)] = "loopback"
  }

  # ordered based on preference heuristic
  factor(type, levels = c("ethernet", "wireless", "other", "virtual", "loopback"))
}


#' List available network interfaces
#'
#' This function attempts to order the interfaces using a relative priority
#' based on the ip type, interface type, and then interface name. The goal is
#' to favor the interface that is most "accessible". Known virtual adapters
#' (Docker, VPNs, virtual machine bridges, and similar) are ranked below
#' physical ones.
#'
#' @return A tibble with columns `interface`, `iface_type`, `ip`, and `ip_type`.
#'
#' @export
network_interfaces = function() {
  ips = get_ipv4()
  rank_interfaces(names(ips), unname(ips))
}

rank_interfaces = function(interface, ip) {
  df = tibble::tibble(
    interface = interface,
    iface_type = iface_type(interface),
    ip = ip,
    ip_type = ip_type(ip)
  )

  df[order(df[["ip_type"]], df[["iface_type"]], df[["interface"]]), ]
}
