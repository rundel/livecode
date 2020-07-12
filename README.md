
# livecode <img src='man/figures/logo.png' align="right" height="140" />

<!-- badges: start -->

[![R build
status](https://github.com/rundel/livecode/workflows/R-CMD-check/badge.svg)](https://github.com/rundel/livecode/actions?query=workflow%3AR-CMD-check)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<!-- badges: end -->

<br/>

livecode is an R package that enables you to broadcast a local R (or any
other text) document over the web and provide live updates as it is
edited.

<br/>

![](man/figures/livecode.png)

## Installation

You can install the development version of `livecode` from this GitHub
repository:

``` r
remotes::install_github("rundel/livecode")
```

## Usage

``` r
# From RStudio with an open R script
server = livecode::serve_file()
#> ✔ Started sharing 'example.R' at 'http://192.168.1.128:30000'.
#> ✖ The current ip address ('192.168.1.128') for the server is private, only users on the same local network are likely to be able to connect.

# Once started, send messages to your users.
server$send_msg("Hello World!", type = "success")
server$send_msg("Oh no!\n\n Something bad has happened.", type = "error")

# Once finished, shut the server down.
server$stop()
#> ✔ Stopped server at 'http://192.168.1.128:30000'.
```

## Using bitly

`livecode` has built in functionality for generating a bitlink
automatically for your livecoding session. To do this you will need to
provide `livecode` with a bitly API access token. To obtain one of these
tokens you will need to create an account with bitly (the free tier is
sufficient) and then select <kbd>Profile Settings</kbd> \> <kbd>Generic
Access Token</kbd> and then enter your password when prompted. This
results in a long hexadecimal string that you should copy to your
clipboard.

`livecode` looks for this token in an environmental variable called
`BITLY_PAT`. To properly configure this environmental variable we can
use the `usethis` package. In R run the following,

``` r
usethis::edit_r_environ()
```

which will open your `.Renviron` file for you and you will just need to
add a single line with the format

    BITLY_PAT=0123456789abcdef0123456789abcdef01234567

replacing `0123456789abcdef0123456789abcdef01234567` with the
hexadecimal string you copied from bitly. After saving `.Renviron` you
will need to restart your R session and can then test that your token is
function correctly by running,

``` r
livecode::bitly_test_token()
#> ✔ Your bitly token is functioning correctly.
```
