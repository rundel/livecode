# Wire protocol of the tailcat broadcast, version 1: newline-delimited JSON
# over one TCP stream, written by the broadcast and only read by followers.
# The first line is a hello, every later line a frame. A broadcast that is
# stopped ends with a bye line; a stream that ends without one was cut off.
# Everything here is pure so it can be tested without a network.

tc_protocol_version = 1L
tc_default_port = 4000L
tc_max_line_bytes = 5 * 1024^2
tc_max_messages = 20L
tc_max_message_chars = 10000L
tc_max_selection_chars = 10000L
tc_ping_interval = 5
# Several ping intervals, so a quiet but live link is never mistaken for a
# dead one.
tc_read_timeout = 20
tc_retry_min = 1
tc_retry_max = 30
tc_write_timeout = 5
# How often a follower asks tailcat which path its connection takes.
tc_path_interval = 10
tc_path_timeout = 5
# Update intervals a follower's page will draw its progress circle for.
tc_min_interval = 0.1
tc_max_interval = 3600

tc_message_types = function() {
  c("alert", "success", "error", "warning", "info")
}

# Every language the bundled Prism can highlight, plus plain text.
tc_languages = function() {
  c(prism_components(), "qmd", "none")
}

tc_encode = function(x) {
  paste0(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA), "\n")
}

tc_hello = function(file, lang, interval) {
  list(
    livecode = tc_protocol_version,
    file = basename(file),
    lang = lang,
    interval = interval
  )
}

tc_bye = function() {
  list(bye = TRUE)
}

tc_is_bye = function(x) {
  isTRUE(x[["bye"]])
}

# The path a follower's connection takes, from a tailcat discovery ping:
# direct between the two computers, or through one of tailcat's relays.
tc_route = function(ping) {
  endpoint = ping[["endpoint"]]
  if (is_string(endpoint) && nzchar(endpoint))
    return(list(kind = "direct", relay = ""))

  relay = ping[["relay_region_code"]]
  if (!is_string(relay) || !grepl("^[A-Za-z0-9-]{1,16}$", relay))
    relay = ""
  list(kind = "relay", relay = relay)
}

tc_route_unknown = function() {
  list(kind = "", relay = "")
}

tc_message = function(m) {
  list(text = m$get_plain_text(), type = m$get_type())
}

# The parent's frames carry noty_msg objects; the broadcast sends only each
# message's plain text and type, so a follower never receives HTML.
tc_spool_frame = function(frame) {
  out = list()
  for (field in c("content", "selection")) {
    if (!is.null(frame[[field]]))
      out[[field]] = frame[[field]]
  }
  if (length(frame[["messages"]]) > 0)
    out[["messages"]] = lapply(frame[["messages"]], tc_message)
  if (!is.null(frame[["sticky"]]))
    out[["sticky"]] = tc_message(frame[["sticky"]])
  out
}

is_string = function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

# Splits a byte stream into lines. A line longer than max_bytes, or one that
# is not valid UTF-8, is an error: the caller closes the connection.
tc_line_splitter = function(max_bytes = tc_max_line_bytes) {
  st = new.env(parent = emptyenv())
  st$buffer = raw(0)
  function(bytes) {
    # Only the new bytes are searched, so a long line arriving in many
    # reads costs time in proportion to its length.
    ends = length(st$buffer) + which(bytes == as.raw(10L))
    st$buffer = c(st$buffer, bytes)

    lines = character()
    prev = 0L
    for (end in ends) {
      if (end - prev - 1L > max_bytes)
        tc_abort_line("A line from the broadcast is longer than the limit.")
      lines = c(lines, tc_line_text(st$buffer[seq.int(prev + 1L, length.out = end - prev - 1L)]))
      prev = end
    }
    if (prev > 0L)
      st$buffer = st$buffer[-seq_len(prev)]

    if (length(st$buffer) > max_bytes)
      tc_abort_line("A line from the broadcast is longer than the limit.")
    lines
  }
}

tc_line_text = function(line) {
  if (any(line == as.raw(0L)))
    tc_abort_line("A line from the broadcast contains a NUL byte.")
  text = rawToChar(line)
  Encoding(text) = "UTF-8"
  if (!validUTF8(text))
    tc_abort_line("A line from the broadcast is not valid UTF-8.")
  text
}

tc_abort_line = function(msg) {
  cli::cli_abort(msg, class = "livecode_tailcat_protocol")
}

tc_parse = function(line) {
  x = tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) tc_abort_line("A line from the broadcast is not valid JSON.")
  )
  if (!is.list(x) || (length(x) > 0 && is.null(names(x))))
    tc_abort_line("A line from the broadcast is not a JSON object.")
  x
}

# The hello line, rebuilt from known fields only.
tc_validate_hello = function(x) {
  version = x[["livecode"]]
  if (!is.numeric(version) || length(version) != 1 || is.na(version))
    tc_abort_line("The broadcast did not start with a livecode hello.")
  if (version != tc_protocol_version) {
    cli::cli_abort(
      "The broadcast uses livecode protocol {version}, this version of livecode speaks {tc_protocol_version}.",
      class = "livecode_tailcat_version"
    )
  }

  file = x[["file"]]
  file = if (is_string(file)) tc_clean_name(file) else ""

  lang = x[["lang"]]
  if (!is_string(lang) || !lang %in% tc_languages())
    lang = "none"

  interval = x[["interval"]]
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) ||
      interval < tc_min_interval || interval > tc_max_interval)
    interval = NULL

  list(file = file, lang = lang, interval = interval)
}

tc_clean_name = function(file) {
  file = gsub("[[:cntrl:]]", "", file)
  file = basename(gsub("\\", "/", file, fixed = TRUE))
  if (identical(file, ".") || identical(file, ".."))
    file = ""
  substr(file, 1, 255)
}

# A frame, rebuilt from known and valid fields only. Anything else is
# dropped, so nothing the broadcast sends reaches the page unchecked.
tc_validate_frame = function(x) {
  out = list()

  content = x[["content"]]
  if (is_string(content))
    out[["content"]] = content

  selection = x[["selection"]]
  if (is_string(selection) && nchar(selection) <= tc_max_selection_chars &&
      grepl("^[0-9,-]*$", selection))
    out[["selection"]] = selection

  messages = x[["messages"]]
  if (is.list(messages) && is.null(names(messages))) {
    msgs = lapply(utils::head(messages, tc_max_messages), tc_validate_message)
    msgs = Filter(Negate(is.null), msgs)
    if (length(msgs) > 0)
      out[["messages"]] = msgs
  }

  out
}

# Rebuilt as the options of a page notification. The follower page shows
# the text as plain text.
tc_validate_message = function(m) {
  if (!is.list(m) || !is_string(m[["text"]]))
    return(NULL)
  type = m[["type"]]
  if (!is_string(type) || !type %in% tc_message_types())
    type = "info"
  list(
    text = substr(m[["text"]], 1, tc_max_message_chars),
    type = type,
    theme = "bootstrap-v4",
    layout = "topRight",
    timeout = 10000,
    progressBar = FALSE
  )
}
