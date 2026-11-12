# Converts an rstudioapi selection list into a Prism data-line string such
# as "1-10,12". A selection ending at column 1 of a later row does not
# include that row, since nothing on it is selected.
extract_line_nums = function(sel) {
  if (length(sel) == 0)
    return("")

  ranges = vapply(sel, function(s) {
    start = as.integer(s[["range"]][["start"]])
    end = as.integer(s[["range"]][["end"]])

    first = min(start[[1]], end[[1]])
    last = max(start[[1]], end[[1]])
    last_col = if (end[[1]] >= start[[1]]) end[[2]] else start[[2]]

    if (last > first && last_col <= 1)
      last = last - 1L

    if (first == last)
      as.character(first)
    else
      paste0(first, "-", last)
  }, character(1))

  paste(unique(ranges), collapse = ",")
}
