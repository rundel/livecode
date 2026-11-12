test_that("is_ipv4 validates dotted quads", {
  expect_equal(
    is_ipv4(c("192.168.1.1", "0.0.0.0", "255.255.255.255", "256.1.1.1", "1.2.3", "a.b.c.d", NA, "::1")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("is_ipv6 detects IPv6 literals", {
  expect_equal(
    is_ipv6(c("::1", "fe80::1%en0", "2001:db8::1", "[::1]", "192.168.1.1", "localhost")),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("CIDR membership uses the prefix length", {
  expect_true(ip_in_block("172.31.255.255", "172.16.0.0/12"))
  expect_false(ip_in_block("172.32.0.0", "172.16.0.0/12"))
  expect_true(ip_in_block("255.255.255.255", "255.255.255.255/32"))
  expect_false(ip_in_block("255.255.255.254", "255.255.255.255/32"))
  expect_true(ip_in_block("100.127.0.1", "100.64.0.0/10"))
  expect_false(ip_in_block("100.128.0.1", "100.64.0.0/10"))
  expect_false(ip_in_block("not an ip", "0.0.0.0/8"))
})

test_that("ip_type classifies addresses", {
  ips = c("8.8.8.8", "192.168.1.2", "10.0.0.1", "127.0.0.1", "169.254.1.1",
          "198.18.0.1", "192.0.0.5", "100.100.1.1", "999.1.1.1")
  expect_equal(
    as.character(ip_type(ips)),
    c("public", "private", "private", "loopback", "reserved",
      "reserved", "reserved", "private", NA)
  )
})

test_that("benchmark and IETF blocks are not private", {
  expect_false(is_ip_private("198.18.0.1"))
  expect_false(is_ip_private("192.0.0.5"))
})

test_that("interfaces rank physical over virtual adapters", {
  skip_on_os("windows")
  df = rank_interfaces(
    c("docker0", "wg0", "utun3", "eth0", "lo", "veth12ab", "tailscale0", "wlan0"),
    c("172.17.0.1", "10.8.0.2", "100.64.0.3", "192.168.1.20", "127.0.0.1",
      "172.18.0.5", "100.100.1.1", "192.168.1.21")
  )
  expect_equal(df[["interface"]][1:2], c("eth0", "wlan0"))
  expect_equal(df[["interface"]][nrow(df)], "lo")
  expect_equal(
    as.character(iface_type(c("eth0", "enp3s0", "docker0", "br-1a2b", "virbr0", "vboxnet0", "vmnet8", "tun0", "tap0", "zt0", "bridge100"))),
    c("ethernet", "ethernet", rep("virtual", 9))
  )
})

test_that("network_interfaces returns IPv4 addresses", {
  df = network_interfaces()
  expect_s3_class(df, "tbl_df")
  expect_true(all(is_ipv4(df[["ip"]])))
})
