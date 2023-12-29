library(gateCounts)

test_that("checking gateCountsToVisitorCounts", {

  # Test 1: Bidirectional gates with NA values
  # Simulate gate count data using Poisson distribution
  set.seed(1234)
  randomCounts1 <- c(sort(rpois(n = 50, lambda = 10000)),
                    sort(rpois(n = 50, lambda = 400000)),
                    sort(rpois(n = 82, lambda = 800000)),
                    999999, # max value
                    sort(rpois(n = 50, lambda = 10000)),
                    sort(rpois(n = 50, lambda = 450000)),
                    sort(rpois(n = 50, lambda = 850000)))

  # Randomly introduce NA and "Gate broken" entries
  randomPositions <- sample(x = c(1:length(randomCounts1)),
                            size = 8, replace = FALSE)
  randomCounts1[randomPositions[1:4]] <- NA
  randomCounts1[randomPositions[5:8]] <- "Gate broken"

  # Create a tibble with date information
  randomCounts4tibble <- tibble::tibble(
                          dates = seq(lubridate::dmy('01-01-2022'),
                          lubridate::dmy('31-12-2022'),
                          by='1 day')[1:length(randomCounts1)] %>%
                          format('%d-%m-%Y'),
                          counts = randomCounts1)

  # Run gateCountsToVisitorCounts function
  randomCountsEx1 <- gateCounts::gateCountsToVisitorCounts(
                      rawGateCounts = randomCounts4tibble,
                      gateType = "Unidirectional",
                      gatecounterMaxValue = 999999,
                      printMessages = FALSE)

  expect_type(randomCountsEx1, "list")
  expect_s3_class(randomCountsEx1, "gateCountsToVisitorCounts")
  expect_length(randomCountsEx1, 4)
  expect_identical(randomCountsEx1$gatecounterMaxValue, 999999)
  expect_identical(randomCountsEx1$cumulativeVisitorCount, 1841995)
  expect_identical(randomCountsEx1$dailyVisitorCounts$visitorCount[5], 3)
})


# [END]
