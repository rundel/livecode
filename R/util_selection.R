extract_line_nums = function(sel) {
  starts = purrr::map_dbl(sel, list("range", "start", 1))
  ends   = purrr::map_dbl(sel, list("range", "end", 1))

  ranges = paste0(starts, "-", ends, collapse=",")
  gsub("(\\d+)-(\\1)", "\\1", ranges)
}
