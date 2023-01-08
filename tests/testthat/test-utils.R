test_that("time to seconds works", {
  expect_equal(.timeToSec("1:34.142"), 94.142)
  expect_equal(.timeToSec("58.234"), 58.234)
  expect_equal(.timeToSec("+3.123s"), 3.123)

  expect_equal(timeToSec(c("1:33.352", "1:32.123")), c(93.352, 92.123))
})
