test_that("tailcat features say how to install rtailcat when it is missing", {
  local_mocked_bindings(pkg_available = function(pkg) pkg != "rtailcat")

  err = expect_error(follow("tcanything", open_browser = FALSE), class = "livecode_missing_package")
  msg = conditionMessage(err)
  expect_match(msg, "rtailcat", fixed = TRUE)
  expect_match(msg, "pak::pak(\"rundel/rtailcat\")", fixed = TRUE)
  expect_match(msg, "Go toolchain", fixed = TRUE)

  expect_error(tailcat_keys(tempfile()), class = "livecode_missing_package")
})

test_that("serve_file with tailcat fails before starting a server when packages are missing", {
  local_mocked_bindings(pkg_available = function(pkg) !pkg %in% c("rtailcat", "callr"))
  f = local_test_file("x = 1")

  err = expect_error(
    serve_file(f, ip = "127.0.0.1", open_browser = FALSE, auto_save = FALSE, tailcat = TRUE),
    class = "livecode_missing_package"
  )
  expect_match(conditionMessage(err), "callr", fixed = TRUE)
  expect_length(list_servers(), 0)

  srv = local_server(f)
  expect_error(srv$start_tailcat(), class = "livecode_missing_package")
  expect_null(srv$tailcat_address)
  expect_message(srv$stop_tailcat(), "No tailcat broadcast")
})

test_that("tailcat keys and arguments are checked", {
  local_mocked_bindings(pkg_available = function(pkg) TRUE)
  f = local_test_file("x = 1")

  expect_error(
    serve_file(f, ip = "127.0.0.1", open_browser = FALSE, auto_save = FALSE, tailcat = "yes"),
    "must be"
  )
  expect_length(list_servers(), 0)

  incomplete = structure(list(private_key = "a", public_key = "b"), class = "tailcat_keys")
  expect_error(tc_key_list(incomplete), "set of keys")
  keys = structure(
    list(private_key = "a", public_key = "b", preshared_key = "c", address = "d"),
    class = "livecode_tailcat_keys"
  )
  expect_equal(tc_key_list(keys), list(private_key = "a", public_key = "b", preshared_key = "c"))

  srv = local_server(f)
  expect_error(srv$start_tailcat(port = 0), "port")
  expect_error(srv$start_tailcat(max_clients = 0), "max_clients")

  expect_error(follow(c("a", "b"), open_browser = FALSE), "single tailcat address")
  expect_error(follow("", open_browser = FALSE), "single tailcat address")
})

test_that("saved keys never print their secrets or address", {
  path = withr::local_tempfile(fileext = ".rds")
  keys = structure(
    list(
      address = "tcSECRETADDRESS", public_key = "nodekey:public",
      private_key = "privkey:SECRET", preshared_key = "psk:SECRET", created = Sys.time()
    ),
    class = "livecode_tailcat_keys"
  )
  saveRDS(keys, path)

  read = tailcat_keys(path)
  expect_equal(read$address, "tcSECRETADDRESS")
  out = c(utils::capture.output(print(read)), utils::capture.output(utils::str(read)), format(read))
  expect_false(any(grepl("SECRET", out)))
  expect_true(any(grepl("nodekey:public", out, fixed = TRUE)))

  writeLines("not keys", path)
  expect_error(tailcat_keys(path), "does not hold")
})
