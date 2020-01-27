testequal <- function(n, expected) {
  actual <- thirt(n)
  expect_equal(actual, expected)
}

test_that("tests thirt", {
  testequal(8529, 79)
  testequal(85299258, 31)
})