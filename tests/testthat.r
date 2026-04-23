# This file is part of the setup for testthat.
# It is recommended that you do not modify it.
#
# Where are these tests run?
# In R, when you run `devtools::test()` or `testthat::test_local()`.
#
# On GitHub Actions CI, the tests are run in the `test` job.
# See `.github/workflows/testpkg.yaml` for details.
#
# For more information, see:
# https://r-pkgs.org/tests.html

library(testthat)
library(microecoshiny)

test_check("microecoshiny")
