library(testthat)

test_that("tell_Y force_cleanup produces an informative warning without stray console output", {
  tmp_path <- tempfile("asktell_test")
  dir.create(tmp_path)
  on.exit(unlink(tmp_path, recursive = TRUE), add = TRUE)

  # simulate a stale, unconsumed Y file left over from a previous conflicting call
  saveRDS(42, file = file.path(tmp_path, "Y.done_99"))

  expect_warning(
    tell_Y(1, id = 99, tmp_path = tmp_path, force_cleanup = TRUE, trace = function(...) {}),
    "Cleanup this data"
  )

  saveRDS(42, file = file.path(tmp_path, "Y.done_97"))
  w <- tryCatch({
    tell_Y(1, id = 97, tmp_path = tmp_path, force_cleanup = TRUE, trace = function(...) {})
    NULL
  }, warning = function(w) w)
  expect_match(conditionMessage(w), "42", fixed = TRUE)
})

test_that("tell_Y without force_cleanup stops on an existing id", {
  tmp_path <- tempfile("asktell_test2")
  dir.create(tmp_path)
  on.exit(unlink(tmp_path, recursive = TRUE), add = TRUE)

  saveRDS(42, file = file.path(tmp_path, "Y.done_1"))

  expect_error(
    tell_Y(1, id = 1, tmp_path = tmp_path, trace = function(...) {}),
    "already in use"
  )
})

test_that("ask_Y/ask_dY/tell_Y/tell_dY reject NULL values", {
  expect_error(ask_Y(NULL), "must not be NULL")
  expect_error(ask_dY(NULL), "must not be NULL")
  expect_error(tell_Y(NULL), "must not be NULL")
  expect_error(tell_dY(NULL), "must not be NULL")
})
