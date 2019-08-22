# From https://en.wikipedia.org/wiki/Reserved_IP_addresses

reserved_blocks = c(
  "0.0.0.0/8",          # Software	        Current network[1] (only valid as source address).
  "10.0.0.0/8",         # Private network	  Used for local communications within a private network.[2]
  "100.64.0.0/10",      # Private network	  Shared address space[3] for communications between a service provider and its subscribers when using a carrier-grade NAT.
  "127.0.0.0/8",        # Host	            Used for loopback addresses to the local host.[1]
  "169.254.0.0/16",     # Subnet	          Used for link-local addresses[4] between two hosts on a single link when no IP address is otherwise specified, such as would have normally been retrieved from a DHCP server.
  "172.16.0.0/12",      # Private network	  Used for local communications within a private network.[2]
  "192.0.0.0/24",       # Private network	  IETF Protocol Assignments.[1]
  "192.0.2.0/24",       # Documentation	    Assigned as TEST-NET-1, documentation and examples.[5]
  "192.88.99.0/24",     # Internet	        Reserved.[6] Formerly used for IPv6 to IPv4 relay[7] (included IPv6 address block 2002::/16).
  "192.168.0.0/16",     # Private network	  Used for local communications within a private network.[2]
  "198.18.0.0/15",      # Private network	  Used for benchmark testing of inter-network communications between two separate subnets.[8]
  "198.51.100.0/24",    # Documentation	    Assigned as TEST-NET-2, documentation and examples.[5]
  "203.0.113.0/24",     # Documentation	    Assigned as TEST-NET-3, documentation and examples.[5]
  "224.0.0.0/4",        # Internet	        In use for IP multicast.[9] (Former Class D network).
  "240.0.0.0/4",        # Internet	        Reserved for future use.[10] (Former Class E network).
  "255.255.255.255/32"  # Subnet            Reserved for the "limited broadcast" destination address.[1][11]
)

loopback_blocks = c(
  "127.0.0.0/8"
)

private_blocks = c(
  "10.0.0.0/8",
  "100.64.0.0/10",
  "172.16.0.0/12",
  "192.0.0.0/24",
  "192.168.0.0/16",
  "198.18.0.0/15"
)


is_ip_loopback = function(ip) {
  iptools::ip_in_any(ip, loopback_blocks)
}

is_ip_private = function(ip) {
  iptools::ip_in_any(ip, private_blocks)
}

is_ip_reserved = function(ip) {
  iptools::ip_in_any(ip, reserved_blocks)
}

ip_type = function(ip) {
  type = rep("public", length(ip))

  type[is_ip_reserved(ip)] = "reserved"
  type[is_ip_private(ip)] = "private"
  type[is_ip_loopback(ip)] = "loopback"

  type
}

get_interfaces = function() {
  ips = get_ipv4()
  types = ip_type(ips)

  tibble::tibble(
    interface = names(ips),
    ip = ips,
    type = types
  )
}

print_interfaces = function(ifaces = get_interfaces()) {

  usethis:::hd_line("Network interfaces:")
  purrr::pwalk(
    ifaces,
    function(interface, ip, type) {
      usethis:::cat_line(
        "* ", interface, ": ",
        usethis::ui_value(ip),
        " (", crayon::blurred(type) ,")"
      )
    }
  )

}


