# livecode 0.1.0.9000

## Breaking changes

* ngrok tunnel and Bitly short link support have been removed, along with `bitly_get_groups()`, `bitly_get_token()`, `bitly_reset_token()`, `bitly_set_token()`, `bitly_shorten()`, and `bitly_test_token()`. `serve_file()` and `lc_server_iface$new()` lose the `bitly`, `tunnel`, `ngrok_domain`, and `ngrok_bin` arguments, and the server object loses `$start_ngrok()`, `$stop_ngrok()`, `$public_url`, and `$tunnel_active`.
* `serve_file()` no longer accepts unused arguments through `...`. `file`, `ip`, and `port` now default to `NULL`.
* `file_cache()` and `FileCache$new()` no longer take a `file_id` argument.
* The default update `interval` is now 1 second everywhere.
* IPv6 addresses are rejected with a clear error instead of producing broken URLs.
* `file_stream_server()` loses its `template` argument; the unused highlight.js template is gone.
* Messages now close after 10 seconds by default; pass `timeout = FALSE` to `send_msg()` to keep one open until it is clicked.

## New features

* Positron is supported alongside RStudio: `serve_file()` with no file shares the active document, the editor selection is highlighted, and the page updates whenever the file is saved. Positron answers requests from R only while a console command runs, so livecode reads it each time code is run in the console: the highlight shows the selection as of the last code run rather than following the cursor, and `auto_save` saves the file then if it is the active editor and has unsaved changes. For continuous updates while typing, turn on Positron's auto save (`files.autoSave` set to `afterDelay` with a short `files.autoSaveDelay`). livecode saves without formatting when Positron's kernel allows it and otherwise names the `editor.formatOnSave` and `files.trimTrailingWhitespace` settings it cannot read. Renames are not detected in Positron.
* The `livecode.editor` option (`"rstudio"`, `"positron"`, or `"none"`) overrides the IDE detection.
* In RStudio, renaming the served document or saving it under another name is reported once; the original file keeps being served and is no longer auto-saved.
* The page highlights files by extension instead of always as R, using a bundled Prism 1.29.0 (replacing the 2019 copy) with grammars for R, Quarto and R Markdown, Python, SQL, shell, Markdown, YAML, JSON, and other common languages. Other files are shown as plain text.
* The page reconnects with backoff (1 second, doubling up to 30) after the server restarts or the network drops, and shows a small "reconnecting" indicator meanwhile.
* The page scrolls the instructor's selection into view, unless the viewer scrolled by hand in the last few seconds.
* Sessions can also be broadcast over tailcat with `serve_file(tailcat = TRUE)` or `$start_tailcat()`, and followed from anywhere with `follow("tc...")`, which serves the same page locally. The broadcast runs in a separate R process, only ever sends, drops followers that fall behind, and accepts at most `max_clients` (150) at once. Followers validate and rebuild everything they receive, show messages as plain text, never run or save what they receive, serve the page on 127.0.0.1 only, refuse websockets from other origins, and reconnect with backoff. `tailcat_keys()` creates keys for an address posted ahead of time; use fresh keys for each class meeting. The server gains `$stop_tailcat()` and `$tailcat_address`, and `$send_msg()` a `plain_text` argument for what followers see. This needs the rtailcat package (`pak::pak("rundel/rtailcat")`, built with Go), which is suggested rather than required.
* `stop_all()` also stops followers.

## Bug fixes

* `serve_file("file.R")` in an IDE waits until the editor shows the file instead of sleeping for half a second, and errors if the editor never shows it rather than serving whatever document is active. A missing, empty, or untitled document gives a clear error.
* The served file is compared with the editor's document after resolving symlinks, so the selection is tracked for files reached through a symlinked directory such as `/tmp` on macOS.
* Failing editor calls (auto-save, reading the editor) are reported once and never stop the update loop.
* The trailing-whitespace warning reads RStudio's preference through rstudioapi and also checks the project's `.Rproj` setting.
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
* Line numbers are the file's real line numbers: leading blank lines and indentation are no longer stripped.
* Clearing the file clears the page, and the highlight clears when the served file is not the active document.
* Source text is shown exactly, so `&amp;` or `&lt;-` in a file no longer display as `&` or `<-`.
* Moving the cursor no longer re-highlights the whole file, and a malformed message can no longer block a content update.
* The page has a viewport and language set, works on phones, and turns off the progress indicator and toast animations when the viewer prefers reduced motion.

## Internal changes

* The package installs again: iptools (archived on CRAN) is replaced by base R IPv4 helpers, and private address blocks no longer include the benchmark and IETF ranges.
* Messages use cli; usethis, crayon, purrr, httr, processx, and withr are no longer dependencies. R6, which was used without being declared, is now in Imports, and fs is no longer used.
* The server wraps `httpuv::startServer()` instead of inheriting from httpuv's internal `WebServer` class.
* Running servers are stopped when the package is unloaded.
* Added a testthat test suite and a current GitHub Actions check workflow.
* Unused Prism grammars, plugins, themes and Makefile, Noty source maps and unminified source, and the hex logo PNG are no longer installed.
