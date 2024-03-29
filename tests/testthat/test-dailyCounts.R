library(gateCounts)


test_that("checking visitorCountSummary", {

  # Test 2: Bidirectional gates with NA values
  # Simulate gate count data using Poisson distribution
  set.seed(1234)
  randomCounts3 <- c(sort(rpois(n = 50, lambda = 100)),
                    sort(rpois(n = 50, lambda = 1000)),
                    sort(rpois(n = 82, lambda = 100000)),
                    200000, # max value
                    sort(rpois(n = 50, lambda = 100)),
                    sort(rpois(n = 50, lambda = 1000)),
                    sort(rpois(n = 50, lambda = 100000)))

  # Randomly introduce smaller counts
  randomPositions <- sample(x = c(1:length(randomCounts3)),
                           size = 4, replace = FALSE)
  randomCounts3[randomPositions] <-
    randomCounts3[randomPositions[1:4]] - 10

  # Create a tibble with date information
  randomCounts3tibble <- tibble::tibble(
                          dates = seq(lubridate::dmy('01-01-2022'),
                          lubridate::dmy('31-12-2022'),
                          by='1 day')[1:length(randomCounts3)] %>%
                          format('%d-%m-%Y'),
                          counts = randomCounts3)

  # Run gateCountsToVisitorCounts function
  randomCountsEx3 <- gateCounts::gateCountsToVisitorCounts(
               rawGateCounts = randomCounts3tibble,
                gateType = "Unidirectional",
                gatecounterMaxValue = 200000,
                printMessages = FALSE)

  # Check output and rename column for visitorCountSummary() function
  visitorCountsEx3<- randomCountsEx3$dailyVisitorCounts %>%
     dplyr::rename("counts" = "visitorCount") %>%
     dplyr::select(dates, counts)

   # Use visitor counts to get summaries
   visitorCountsEx1Summary <-
       gateCounts::visitorCountSummary(dailyVisitorCount = visitorCountsEx3)

  expect_type(visitorCountsEx1Summary, "list")
  expect_s3_class(visitorCountsEx1Summary, "visitorCountSummary")
  expect_length(visitorCountsEx1Summary, 16)
  expect_identical(visitorCountsEx1Summary$busiestMonth$month, 7)
  expect_identical(visitorCountsEx1Summary$leastBusiestMonth$month, 1)
  expect_identical(visitorCountsEx1Summary$busiestWeek$week, 27)
  expect_identical(visitorCountsEx1Summary$leastBusiestWeek$week, 13)
  expect_identical(visitorCountsEx1Summary$busiestDay$day, 2L)
  expect_identical(round(as.numeric(visitorCountsEx1Summary$dailyAverage), 0), 914)
  expect_identical(round(as.numeric(visitorCountsEx1Summary$dailyMedian), 0), 2)
  expect_identical(round(as.numeric(visitorCountsEx1Summary$monthlyAverage), 0), 27329)
  expect_identical(round(as.numeric(visitorCountsEx1Summary$monthlyMedian), 0), 715)
})


# [END]
