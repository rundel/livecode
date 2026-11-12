# livecode 0.1.0.9000

## Breaking changes

* ngrok tunnel and Bitly short link support have been removed, along with `bitly_get_groups()`, `bitly_get_token()`, `bitly_reset_token()`, `bitly_set_token()`, `bitly_shorten()`, and `bitly_test_token()`. `serve_file()` and `lc_server_iface$new()` lose the `bitly`, `tunnel`, `ngrok_domain`, and `ngrok_bin` arguments, and the server object loses `$start_ngrok()`, `$stop_ngrok()`, `$public_url`, and `$tunnel_active`.
* `serve_file()` no longer accepts unused arguments through `...`. `file`, `ip`, and `port` now default to `NULL`.
* `file_cache()` and `FileCache$new()` no longer take a `file_id` argument.
* The default update `interval` is now 1 second everywhere.
* IPv6 addresses are rejected with a clear error instead of producing broken URLs.

## Bug fixes

* The update loop starts with the server and keeps running after errors, such as a briefly missing file or no open editor, reporting each error once instead of silently stopping the broadcast.
* A client connecting between a file change and the next update no longer prevents already connected clients from receiving that change.
* Line highlighting no longer merges ranges that share a prefix (for example 1-10 became 10), and a selection ending at the start of a line no longer highlights that line.
* The welcome message is shown to every client when they connect, not only to those connected at the first update. Messages sent while nobody is connected are held until someone connects.
* Changes are detected by any difference in modification time or size, so a file restored from an older copy is broadcast.
* `$stop()` no longer sleeps unconditionally and no longer reports success for a server that is not running.
* Printing a server or message object works again.
* The page title is the escaped file name rather than the full path.
* Interface ranking prefers `eth*` and other physical adapters over Docker, VPN, and virtual machine adapters.
* On Windows, adapters without a unicast address are skipped instead of being dereferenced.

## Internal changes

* The package installs again: iptools (archived on CRAN) is replaced by base R IPv4 helpers, and private address blocks no longer include the benchmark and IETF ranges.
* Messages use cli; usethis, crayon, purrr, httr, processx, and withr are no longer dependencies. R6, which was used without being declared, is now in Imports, and fs is no longer used.
* The server wraps `httpuv::startServer()` instead of inheriting from httpuv's internal `WebServer` class.
* Running servers are stopped when the package is unloaded.
* Added a testthat test suite and a current GitHub Actions check workflow.
