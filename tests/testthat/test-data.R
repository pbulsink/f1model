test_that("Weather works", {
  expect_error(getWeather("bob.com"))
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_Austrian_Grand_Prix"),
               'cloudy')
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_British_Grand_Prix"),
               'warm')
  #The Russian Grand Prix is listed as 'Cloudy and rainy', 'wet' should beat out 'cloudy'
  expect_equal(getWeather("https://en.wikipedia.org/wiki/2021_Russian_Grand_Prix"),
               'wet')
})

test_that("Practice Download Works", {
  practice<-getRacePractices(969)
  expect_equal(practice[practice$driverCode == "WEH","constructorId"][1], 15)
  expect_equal(practice[practice$driverCode == "RAI", "driverId"][1], 8)
  expect_equal(colnames(practice), c("position", "driverNum", "driverName",
                                     "driverCode", "driverCar", "time", "gap",
                                     "laps", "practiceNum", "raceId", "f1RaceId",
                                     "year", "round", "driverId", "constructorId"))
  p0<-getRacePractices(1051)
  expect_equal(nrow(p0), 0)
  expect_equal(ncol(practice), ncol(p0))
})
