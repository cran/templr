library(testthat)

Brent.R <- system.file("Brent.R", package = "templr")
f <- function(x) sin(x) - 0.7

test_that("run.algorithm with overwrite=FALSE stops when work_dir already has saved data", {
  work_dir <- tempfile("run_overwrite")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  run.algorithm(Brent.R, f, list(x = list(min = 0, max = pi / 2)), work_dir = work_dir)
  expect_true(length(list.files(work_dir, pattern = "\\.Rds$")) > 0)

  expect_error(
    run.algorithm(Brent.R, f, list(x = list(min = 0, max = pi / 2)), work_dir = work_dir, overwrite = FALSE),
    "already contains saved data"
  )
})

test_that("run.algorithm with overwrite=TRUE (default) re-runs into an existing work_dir", {
  work_dir <- tempfile("run_overwrite_ok")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  run.algorithm(Brent.R, f, list(x = list(min = 0, max = pi / 2)), work_dir = work_dir)
  expect_error(
    run.algorithm(Brent.R, f, list(x = list(min = 0, max = pi / 2)), work_dir = work_dir),
    NA
  )
})

test_that("run.algorithm validates objective_function and input", {
  expect_error(run.algorithm(Brent.R, "not a function", list(x = list(min = 0, max = 1)), work_dir = tempdir()), "function")
  expect_error(run.algorithm(Brent.R, sin, NULL, work_dir = tempdir()), "input")
})

test_that("parse.algorithm validates file argument", {
  expect_error(parse.algorithm(""), "non-empty")
  expect_error(parse.algorithm(NA_character_), "non-empty")
  expect_error(parse.algorithm("does/not/exist.R"), "Cannot find Algorithm file")
})

test_that("read.algorithm validates file argument", {
  expect_error(read.algorithm(""), "non-empty")
  expect_error(read.algorithm("does/not/exist.R"), "Cannot find Algorithm file")
})
