test_that("time to seconds works", {
  expect_equal(.timeToSec("1:34.142"), 94.142)
  expect_equal(.timeToSec("58.234"), 58.234)
  expect_equal(.timeToSec("+3.123s"), 3.123)
  expect_equal(.timeToSec(NA), NA)
  expect_equal(.timeToSec(""), 0)

  expect_equal(timeToSec(c("1:33.352", "1:32.123")), c(93.352, 92.123))
  expect_equal(timeToSec(c("1:33.352", NA, "")), c(93.352, NA, 0))
})

test_that("ewma calculates moving average",{
  expect_equal(ewma(c(5,10,15,20,25,30), 0.1),
               c(5, 5.5, 6.45, 7.805, 9.5245, 11.57205))
  expect_equal(ewma(c(1,2,3,4,5), 0.5),
               c(1, 1.5, 2.25, 3.125, 4.0625))
  expect_equal(ewma(c(1,2,NA,3,4,5), 0.5),
               c(1,1.5, 1.5, 2.25, 3.125, 4.0625))
  expect_equal(ewma(c(NA, NA, 1,2,3,4,5), 0.5),
               c(NA, NA, 1, 1.5, 2.25, 3.125, 4.0625))
})
