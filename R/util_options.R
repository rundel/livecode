#
# map_value = function(value, map = list()) {
#   if (length(value) == 0) {
#     value
#   } else if (value %in% names(map)) {
#     map[[value]]
#   } else {
#     value
#   }
# }
#
# extract_dcf_value = function(file, key, default = NULL) {
#   lines = readLines(file)
#   key_regex = paste0("^",key, ": ")
#   sub = grepl(key_regex, lines)
#   value = sub(key_regex, "", lines[sub])
#
#   if (length(value) == 0)
#     value = default
#   else if (length(value) > 1) {
#     usethis::ui_warn( c(
#       "Found multiple lines matching key: {usethis::ui_value(key)},",
#       "using the last value."
#     ) )
#     value = value[length(value)]
#   }
#
#   value
# }
#
# get_project_option = function(x, default = NULL,
#                               map = list("Yes" = TRUE, "No" = FALSE)) {
#   if (using_project()) {
#     rproj = fs::dir_ls(rstudioapi::getActiveProject(), glob = "*.Rproj")
#     value = extract_dcf_value(rproj, x, default = default)
#     map_value(value, map)
#   } else {
#     default
#   }
# }
#
# get_user_option = function(x, default = NULL) {
#   if (!is_rstudio()) {
#     default
#   } else {
#     .rs.readUiPref(x) %||% default
#   }
# }
#
#check_proj_trailing_ws = function() {
#  get_project_option("StripTrailingWhitespace", default = FALSE)
#}
#
#check_user_trailing_ws = function() {
#  get_user_option("strip_trailing_whitespace", default = FALSE)
#}



get_option = function(x, default = NULL) {
  if (!is_rstudio()) {
    default
  } else {
    .rs.readUiPref(x) %||% default
  }
}

check_strip_trailing_ws = function() {
  get_option("strip_trailing_whitespace", default = FALSE)
}

