# This file is part of the standard setup for testthat.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# testthat is in Suggests, not Imports, so it is not guaranteed to be
# installed wherever this package is checked. The stock version of this file
# calls library(testthat) unconditionally, which turns a missing optional
# dependency into an R CMD check ERROR on any machine that has not installed
# it. Guarding the call means the tests run whenever testthat is available
# and are skipped cleanly when it is not.
#
# Note that this makes a missing testthat silent. If a check reports no test
# output at all, confirm testthat is installed before concluding the tests
# passed.

if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(codechest)

  test_check("codechest")
} else {
  message("testthat is not installed; skipping tests. ",
          "install.packages(\"testthat\") to run them.")
}
