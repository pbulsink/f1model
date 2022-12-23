test_that("Weather works", {
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_Austrian_Grand_Prix"), 'cloudy')
})
