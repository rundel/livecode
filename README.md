
# livecode <img src='man/figures/logo.png' align="right" height="140" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/rundel/livecode/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rundel/livecode/actions/workflows/R-CMD-check.yaml)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<!-- badges: end -->

<br/>

livecode is an R package that lets you broadcast a local R (or any other text) document over the web and provide live updates as it is edited.

<br/>

![](man/figures/livecode.png)

## Installation

You can install the development version of `livecode` from GitHub with:

``` r
# install.packages("pak")
pak::pak("rundel/livecode")
```

The package includes compiled code, so installing from source needs a working compiler toolchain (Rtools on Windows, the Xcode command line tools on macOS).

## Usage

``` r
# From RStudio with an open R script
server = livecode::serve_file()
#> ✔ Started sharing 'example.R' at <http://192.168.1.128:30000>.
#> ! The current ip address ("192.168.1.128") for the server is private, only users on the same local network are likely to be able to connect.

# Or share a specific file from any R session
server = livecode::serve_file("example.R")

# Once started, send messages to your users.
server$send_msg("Hello World!", type = "success")
server$send_msg("Oh no!\n\n Something bad has happened.", type = "error")

# Once finished, shut the server down.
server$stop()
#> ✔ Stopped server at <http://192.168.1.128:30000>.
```

The server listens on the address of your preferred network interface, which you can inspect (and override with the `ip` argument) using `livecode::network_interfaces()`. Only IPv4 addresses are supported.

The page highlights the file according to its extension (R, Quarto and R Markdown, Python, SQL, and other common languages, with anything else shown as plain text), numbers the lines as they are in the file, and highlights and scrolls to the lines selected in RStudio. If the connection drops, for example while the server restarts, the page shows that it is reconnecting and catches up on its own.

## Managing servers

Every server started in the current session is tracked, so you can find or stop them even if you did not keep the returned object.

``` r
# List the servers that are currently running
livecode::list_servers()

# Stop all of them
livecode::stop_all()
```

Running servers are also stopped when the package is unloaded.
