# tests/testthat/test-import.R

test_that("import from CRAN works", {
  skip_on_cran()
  skip_if_offline()

  # Test importing a small, stable package from CRAN (jsonlite is a good example)
  temp_lib <- tempfile("test_lib_cran")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)
  
  # Import from CRAN
  result <- import("jsonlite", lib.loc = temp_lib)
  
  expect_true(result$jsonlite)

  # if jsonlite was intalled in the temp library
  if (! ("jsonlite" %in% rownames(installed.packages()))) {
  expect_true("jsonlite" %in% installed.packages(lib.loc = temp_lib)[, "Package"])
  
  # Verify the package can be loaded
  expect_true(require("jsonlite", lib.loc = temp_lib, character.only = TRUE, quietly = TRUE))
  detach("package:jsonlite", unload = TRUE)
  }
})

test_that("import from GitHub HEAD works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("remotes")
  
  # Test importing from GitHub HEAD
  # Using a small, stable test package (e.g., r-lib/praise)
  temp_lib <- tempfile("test_lib_gh_head")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)
  
  # Import from GitHub HEAD
  result <- import("github:rladies/praise", lib.loc = temp_lib)
  expect_true(result$praise)

  if (! ("praise" %in% rownames(installed.packages()))) {
    expect_true("praise" %in% installed.packages(lib.loc = temp_lib)[, "Package"])
    
    # Verify the package can be loaded
    expect_true(require("praise", lib.loc = temp_lib, character.only = TRUE, quietly = TRUE))
    detach("package:praise", unload = TRUE)
  }
})

test_that("import from GitHub tag works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("remotes")
  
  # Test importing from a specific GitHub tag
  # Using a small, stable test package with tags
  temp_lib <- tempfile("test_lib_gh_tag")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)
  
  # Import from GitHub tag (e.g., r-lib/praise@v1.0.0)
  result <- import("github:rladies/praise@v1.0.0", lib.loc = temp_lib)
  
  expect_true(result$praise)

  if (! ("praise" %in% rownames(installed.packages()))) {
  expect_true("praise" %in% installed.packages(lib.loc = temp_lib)[, "Package"])
  
  # Verify the package can be loaded
  expect_true(require("praise", lib.loc = temp_lib, character.only = TRUE, quietly = TRUE))
  
  # Verify it's the correct version
  pkg_desc <- packageDescription("praise", lib.loc = temp_lib)
  expect_equal(pkg_desc$Version, "1.0.0")
  
  detach("package:praise", unload = TRUE)
    }
})

test_that("import handles invalid package names gracefully", {
  skip_on_cran()
  
  temp_lib <- tempfile("test_lib_invalid")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)
  
  # Test with non-existent package
  expect_error(import("nonexistent_package_xyz123", lib.loc = temp_lib))
})

test_that("import with force=TRUE reinstalls an already loadable package", {
  skip_on_cran()
  skip_if_offline()

  temp_lib <- tempfile("test_lib_force")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)

  # Pre-install directly into temp_lib (praise has no heavy dependencies, keeping this
  # test fast), independently of whatever import()/library() may already know about it
  # elsewhere on the machine running the test.
  utils::install.packages("praise", lib = temp_lib, repos = "https://cloud.r-project.org", quiet = TRUE)
  first_install_time <- file.info(file.path(temp_lib, "praise"))$mtime
  expect_false(is.na(first_install_time))

  Sys.sleep(1)

  # Re-import with force=TRUE should reinstall into temp_lib (not just report it as
  # already loaded/skip, which is what would happen without force)
  result_forced <- import("praise", lib.loc = temp_lib, force = TRUE)
  expect_true(result_forced$praise)
  second_install_time <- file.info(file.path(temp_lib, "praise"))$mtime

  expect_true(second_install_time > first_install_time)

  if ("praise" %in% loadedNamespaces()) {
    try(detach("package:praise", unload = TRUE), silent = TRUE)
  }
})

test_that("import handles invalid GitHub repos gracefully", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("remotes")
  
  temp_lib <- tempfile("test_lib_invalid_gh")
  dir.create(temp_lib, recursive = TRUE)
  on.exit(unlink(temp_lib, recursive = TRUE), add = TRUE)
  
  # Test with non-existent GitHub repo
  expect_error(import("nonexistent-org/nonexistent-repo", lib.loc = temp_lib))
})
