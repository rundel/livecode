test_that("noty_msg prints and serializes", {
  m = noty_msg$new("Hello {world}", type = "success")
  expect_no_error(suppressMessages(print(m)))
  msg = m$get_msg()
  expect_equal(msg$text, "Hello {world}")
  expect_equal(msg$type, "success")
})

test_that("invalid messages are rejected", {
  expect_error(as_noty_msg(1), "Invalid message type")
})
