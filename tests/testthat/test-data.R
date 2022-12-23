test_that("Weather works", {
  expect_error(getWeather("bob.com"))
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_Austrian_Grand_Prix"), 'cloudy')
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_British_Grand_Prix"), 'warm')
  #The Russian Grand Prix is listed as 'Cloudy and rainy', 'wet' should beat out 'cloudy'
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_Russian_Grand_Prix"), 'wet')
})
