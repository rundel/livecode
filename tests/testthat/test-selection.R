sel = function(...) {
  lapply(list(...), function(r) {
    list(range = list(start = c(row = r[1], column = r[2]), end = c(row = r[3], column = r[4])), text = "")
  })
}

test_that("ranges are not collapsed by number prefixes", {
  expect_equal(extract_line_nums(sel(c(1, 1, 10, 5))), "1-10")
  expect_equal(extract_line_nums(sel(c(11, 1, 12, 3))), "11-12")
})

test_that("a single line selection or cursor gives one number", {
  expect_equal(extract_line_nums(sel(c(5, 1, 5, 1))), "5")
  expect_equal(extract_line_nums(sel(c(5, 2, 5, 9))), "5")
})

test_that("multiple ranges are joined", {
  expect_equal(extract_line_nums(sel(c(1, 1, 3, 2), c(7, 1, 7, 4), c(20, 1, 22, 2))), "1-3,7,20-22")
})

test_that("empty selections give an empty string", {
  expect_equal(extract_line_nums(list()), "")
})

test_that("a selection ending at column 1 of the next row excludes that row", {
  expect_equal(extract_line_nums(sel(c(3, 1, 6, 1))), "3-5")
  expect_equal(extract_line_nums(sel(c(3, 4, 4, 1))), "3")
})

test_that("large line numbers are not printed in scientific notation", {
  expect_equal(extract_line_nums(sel(c(100000, 1, 100000, 2))), "100000")
})
