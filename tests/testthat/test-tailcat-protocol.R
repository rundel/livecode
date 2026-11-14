test_that("lines are one JSON object each and end with a newline", {
  line = tc_encode(list(content = "a\nb\n", selection = "1-2"))
  expect_true(endsWith(line, "\n"))
  expect_equal(sum(strsplit(line, "")[[1]] == "\n"), 1)
  expect_equal(jsonlite::fromJSON(line)$content, "a\nb\n")
})

test_that("the hello carries the version, a basename, and the language", {
  hello = tc_hello("/home/me/secret/lecture.R", "r", 1)
  expect_equal(hello, list(livecode = 1L, file = "lecture.R", lang = "r", interval = 1))
})

test_that("the line splitter handles lines split across reads", {
  split = tc_line_splitter()
  expect_equal(split(charToRaw("{\"a\":")), character())
  expect_equal(split(charToRaw("1}\n{\"b\"")), "{\"a\":1}")
  expect_equal(split(charToRaw(":2}\n{\"c\":3}\n")), c("{\"b\":2}", "{\"c\":3}"))
  expect_equal(split(raw(0)), character())
})

test_that("the line splitter keeps multibyte characters split across reads", {
  split = tc_line_splitter()
  bytes = charToRaw(enc2utf8("{\"content\":\"\u00e9\u4e2d\"}\n"))
  out = c(split(bytes[1:13]), split(bytes[14:length(bytes)]))
  expect_equal(jsonlite::fromJSON(out)$content, "\u00e9\u4e2d")
})

test_that("lines over the 5 MB cap are refused, complete or not", {
  expect_equal(tc_max_line_bytes, 5 * 1024^2)

  split = tc_line_splitter(max_bytes = 10)
  expect_equal(split(charToRaw("0123456789\n")), "0123456789")
  expect_error(split(charToRaw("0123456789A\n")), class = "livecode_tailcat_protocol")

  split = tc_line_splitter(max_bytes = 10)
  split(charToRaw("01234"))
  expect_error(split(charToRaw("567890")), class = "livecode_tailcat_protocol")

  split = tc_line_splitter()
  expect_error(split(as.raw(rep(120L, tc_max_line_bytes + 1))), class = "livecode_tailcat_protocol")
})

test_that("NUL bytes, invalid UTF-8, and invalid JSON are refused", {
  split = tc_line_splitter()
  expect_error(split(as.raw(c(123, 0, 125, 10))), class = "livecode_tailcat_protocol")
  split = tc_line_splitter()
  expect_error(split(as.raw(c(0xff, 0xfe, 10))), class = "livecode_tailcat_protocol")

  expect_error(tc_parse("{not json"), class = "livecode_tailcat_protocol")
  expect_error(tc_parse("[1, 2]"), class = "livecode_tailcat_protocol")
  expect_error(tc_parse("\"text\""), class = "livecode_tailcat_protocol")
  expect_equal(tc_parse("{\"a\": 1}"), list(a = 1L))
})

test_that("the hello is validated and rebuilt", {
  expect_equal(
    tc_validate_hello(list(livecode = 1, file = "lecture.R", lang = "r", interval = 1, extra = "x")),
    list(file = "lecture.R", lang = "r", interval = 1)
  )
  for (bad in list(0, -1, 1e6, "1", NA_real_, c(1, 2)))
    expect_null(tc_validate_hello(list(livecode = 1, interval = bad))$interval)
  expect_error(tc_validate_hello(list(content = "x")), class = "livecode_tailcat_protocol")
  expect_error(tc_validate_hello(list(livecode = "1")), class = "livecode_tailcat_protocol")
  expect_error(tc_validate_hello(list(livecode = 2)), class = "livecode_tailcat_version")

  hello = function(...) tc_validate_hello(list(livecode = 1, ...))
  expect_equal(hello(file = "../../etc/passwd")$file, "passwd")
  expect_equal(hello(file = "C:\\Users\\me\\a.R")$file, "a.R")
  expect_equal(hello(file = "a\u0007\nb.R")$file, "ab.R")
  expect_equal(hello(file = "..")$file, "")
  expect_equal(hello(file = 1)$file, "")
  expect_equal(nchar(hello(file = strrep("a", 1000))$file), 255)

  expect_equal(hello(lang = "python")$lang, "python")
  expect_equal(hello(lang = "qmd")$lang, "qmd")
  expect_equal(hello(lang = "evil\" onload=\"x")$lang, "none")
  expect_equal(hello(lang = c("r", "python"))$lang, "none")
  expect_equal(hello()$lang, "none")
})

test_that("bundled languages are the only ones accepted", {
  expect_setequal(tc_languages(), c(prism_components(), "qmd", "none"))
  files = c("a.R", "a.py", "a.qmd", "a.sql", "Dockerfile", "a.txt")
  expect_true(all(vapply(files, file_code_language, character(1)) %in% tc_languages()))
})

test_that("frames are validated and rebuilt, unknown fields dropped", {
  frame = tc_validate_frame(list(
    content = "x = 1\n", selection = "1-2,5", ping = TRUE,
    interval = 1, lang = "r", onload = "alert(1)", eval = "system('x')"
  ))
  expect_equal(frame, list(content = "x = 1\n", selection = "1-2,5"))

  expect_equal(tc_validate_frame(list(content = c("a", "b"))), list())
  expect_equal(tc_validate_frame(list(content = list("a"))), list())
  expect_equal(tc_validate_frame(list(content = 1)), list())
  expect_equal(tc_validate_frame(list(content = "")), list(content = ""))

  expect_equal(tc_validate_frame(list(selection = "")), list(selection = ""))
  expect_equal(tc_validate_frame(list(selection = "1-2;alert(1)")), list())
  expect_equal(tc_validate_frame(list(selection = "<b>1</b>")), list())
  expect_equal(tc_validate_frame(list(selection = strrep("1", 20000))), list())
  expect_equal(tc_validate_frame(list(selection = 3)), list())
})

test_that("frames keep only known fields", {
  frame = tc_validate_frame(list(
    content = "x", selection = "1",
    messages = list(list(text = "<b>hi</b>", type = "success")),
    sticky = list(text = "Welcome"),
    eval = "system('touch x')"
  ))
  expect_equal(frame, list(content = "x", selection = "1"))
})

test_that("spool frames carry the content and selection only", {
  frame = list(interval = 1, lang = "r", content = "x", selection = "2")
  expect_equal(tc_spool_frame(frame), list(content = "x", selection = "2"))
  expect_equal(tc_spool_frame(list(interval = 1)), list())
})

test_that("the broadcast state gives late joiners the full current state", {
  state = tc_state()
  hello = tc_hello("lecture.R", "r", 1)

  line = tc_apply_frames(state, list(
    list(content = "a"),
    list(selection = "1")
  ))
  sent = jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(sent$content, "a")
  expect_equal(sent$selection, "1")
  expect_setequal(names(sent), c("content", "selection"))
  expect_null(tc_apply_frames(state, list(list(), list())))

  tc_apply_frames(state, list(list(content = "b")))
  lines = strsplit(tc_greeting(hello, state), "\n")[[1]]
  expect_length(lines, 2)
  expect_equal(jsonlite::fromJSON(lines[1])$livecode, 1)
  current = jsonlite::fromJSON(lines[2], simplifyVector = FALSE)
  expect_equal(current$content, "b")
  expect_equal(current$selection, "1")
})

test_that("a bye line is told apart from a frame", {
  expect_true(tc_is_bye(tc_parse(tc_encode(tc_bye()))))
  expect_false(tc_is_bye(list(bye = "yes")))
  expect_false(tc_is_bye(list(content = "bye")))
  expect_equal(tc_validate_frame(tc_bye()), list())
})
