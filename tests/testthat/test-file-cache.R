test_that("FileCache reads and refreshes on change", {
  f = local_test_file("a = 1")
  fc = file_cache(f)
  expect_equal(fc$content, "a = 1\n")
  expect_false(fc$need_update())

  touch_write(f, "a = 2")
  expect_true(fc$need_update())
  expect_equal(fc$content, "a = 2\n")
  expect_false(fc$need_update())
})

test_that("FileCache notices a file restored with an older mtime", {
  f = local_test_file("a = 1")
  fc = file_cache(f)
  old = file.mtime(f)
  writeLines("b = 22", f)
  Sys.setFileTime(f, old - 3600)
  expect_true(fc$need_update())
  expect_equal(fc$content, "b = 22\n")
})

test_that("FileCache keeps the last content when the file disappears", {
  f = local_test_file("a = 1")
  fc = file_cache(f)
  unlink(f)
  expect_false(fc$need_update())
  expect_equal(fc$content, "a = 1\n")
})

test_that("FileCache errors on a missing file", {
  expect_error(file_cache(tempfile()), "Unable to locate file")
})
