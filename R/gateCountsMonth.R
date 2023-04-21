#' Calculates Visitor Count Summaries From Daily Gate Counts
#'
#' A function that calculates, daily, monthly and cumulative visitor
#' counts, provided a numeric vector or a tibble containing values
#' of raw daily gate counts with a specific format (see argument details).
#' The function adjusts for several factors outlined under details (see README
#' file for examples). This function was developed to improve current
#' methodologies for calculating visitor counts from gate counts.
#'
#' @details The function requires directionality of the gates for which
#'    the daily counts are provided (called gate type). If unidirectional
#'    no adjustments are done. If bidirectional, the cumulative sum at
#'    the end will be divided by two. Testing has shown that this method
#'    of dividing at the end will help reduce issues with counts
#'    that result from division of daily counts by two and rounding up or
#'    down. Further, negative counts can result during calculation if
#'    the counter has reset or if a lower value has been entered compared
#'    to previous day. This function attempts to correct for both scenarios.
#'    In the case of counter reset, the 'gatecounterMaxValue' argument
#'    provided by the user will be used to calculate the corrected value.
#'    In the case of a lower value being entered compared to previous day,
#'    the negative value will be replaced by NA. For next day, the count is
#'    calculated by the numeric value previously reported prior to NA, that
#'    must be higher than current value. The function also ensure counts
#'    for empty cells (when the count was forgotten to be reported) are
#'    accounted for. All scenarios are explained with images on the tutorial.
#'
#' @param rawGateCounts A numeric vector or tibble, with number of rows
#'    equalling to length of days and two columns, such that the dimension
#'    is days x 2. Here days is the number of days for which raw gate counts
#'    are present. First column must contain the date and second
#'    column must contain the gate count reading for the given date.
#' @param gateType A character string with options "Unidirectional" or
#     "Bidirectional", to indicate gate type. If the gate is one-way only,
#     then enter "Unidirectional". If the gate permits visitors in and out,
#     then enter "Bidirectional". Bidirectional selection will lead to
#     count sum being divided by two. The default value is "Unidirectional".
#' @param gatecounterMaxValue A numeric value greater than 0 indicating the
#'    gate counter max value, before it is reset. The default value is
#'    999,999.
#' @param printMessages An logical indicating TRUE or FALSE as to whether
#'    progress messages should be printed. The default value is TRUE. Setting
#'    to FALSE will remove messages from being shown.
#'
#' @return Returns an S3 object of class InfCriteria with results.
#' \itemize{
#'   \item cumulativeCount - Sum of daily gate counts for the period,
#'         adjusted for issues mentioned under details. If gateType was
#'         "Bidirectional", the final resulting number would be divided
#'         by two.
#'   \item dailyCounts - Daily gate counts for the period, adjusted
#'         for issues mentioned under details. If gateType was
#'         "Bidirectional", the visitor count is divided by
#'         two and ceiling() function from base R is applied.
#'   \item monthlyCounts - Monthly gate counts for the period, adjusted
#'         for issues mentioned under details.
#'   \item gateType - Gate type for which counts are provided by the user.
#'   \item busiestMonth - Month with the highest visitor count.
#'   \item leastBusiestMonth - Month with the least visitor count.
#'   \item busiestDay - Day with the highest visitor count. There
#'         maybe multiple dates.
#'   \item leastBusiestDay - Day with the lowest visitor count. There
#'         maybe multiple dates.
#' }
#'
#' @examples
#'
#' set.seed(1234)
#' # Example 1: Unidirectional gates with daily counts
#' randomCounts1 <- c(sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100000)),
#'                        200000, # max value
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 100000)))
#'
#' randomCountsSumEx1 <- gateCountCumulative(
#'              rawGateCounts = randomCounts1,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000)
#' randomCountsSumEx1$adjustedCountSum # access cumulative count
#' # Cumulative (adjusted) sum for gate type unidirectional is 300618
#'
#'
#' # Example 2: Unidirectional gates with random NA values
#' randomCounts2 <- c(sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100000)),
#'                        200000, # max value
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 100000)))
#'
#' # randomly introduce NA and "Gate broken" entries
#' randomPositions <- sample(x = c(1:length(randomCounts2)),
#'                          size = 8, replace = FALSE)
#' randomCounts2[randomPositions[1:4]] <- NA
#' randomCounts2[randomPositions[5:8]] <- "Gate broken"
#'
#' randomCountsSumEx2 <- gateCountCumulative(
#'              rawGateCounts = randomCounts2,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000)
#' randomCountsSumEx2$adjustedCountSum # access cumulative count
#' # Cumulative (adjusted) sum for gate type unidirectional is 300777
#'
#'
#' # Example 3: Unidirectional gates with random entry errors
#' randomCounts3 <- c(sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100000)),
#'                        200000, # max value
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 100000)))
#'
#' # randomly introduce smaller counts
#' randomPositions <- sample(x = c(1:length(randomCounts3)),
#'                          size = 4, replace = FALSE)
#' randomCounts3[randomPositions] <- randomCounts3[randomPositions[1:4]] - 10
#'
#' randomCountsSumEx3 <- gateCountCumulative(
#'              rawGateCounts = randomCounts3,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000)
#' randomCountsSumEx3$adjustedCountSum # access cumulative count
#' # Cumulative (adjusted) sum for gate type unidirectional is 300673
#'
#'
#' # Example 4: Bidirectional gates with NA values
#' randomCounts4 <- c(sort(rpois(n = 50, lambda = 10000)),
#'                   sort(rpois(n = 50, lambda = 400000)),
#'                   sort(rpois(n = 82, lambda = 800000)),
#'                        999999, # max value
#'                   sort(rpois(n = 50, lambda = 10000)),
#'                   sort(rpois(n = 50, lambda = 450000)),
#'                   sort(rpois(n = 50, lambda = 850000)))
#'
#' # randomly introduce NA and "Gate broken" entries
#' randomPositions <- sample(x = c(1:length(randomCounts4)),
#'                          size = 8, replace = FALSE)
#' randomCounts4[randomPositions[1:4]] <- NA
#' randomCounts4[randomPositions[5:8]] <- "Gate broken"
#'
#' randomCountsSumEx4 <- gateCountCumulative(
#'              rawGateCounts = randomCounts4,
#'              gateType = "Bidirectional",
#'              gatecounterMaxValue = 999999)
#' randomCountsSumEx4$adjustedCountSum # access cumulative count
#' # Cumulative (adjusted) sum for gate type bidirectional is 921027
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @references
#' Müller K, Wickham H (2022). _tibble: Simple Data Frames_. R
#' package version 3.1.8, \href{https://CRAN.R-project.org/package=tibble}{Link}.
#'
#' Google. (2022, February 14). Cleaning up gate count statistics.
#' Google Groups. Retrieved September 26, 2022,
#' \href{https://groups.google.com/a/arl.org/g/arl-assess/c/JQyllZN4gaE}{Link}.
#'
#' @export
#' @import tibble
#' @import tidyverse
gateCountSummary <- function(rawGateCounts,
                                gateType = "Unidirectional",
                                gatecounterMaxValue = 999999,
                                printMessages = TRUE) {
  # Check input arguments
  if(gateType != "Bidirectional" && gateType != "Unidirectional") {
    stop("\n gateType argument can only take on values Bidirectional
         or Unidirectional")
  }

  if (gatecounterMaxValue <= 0) {
    stop("\n gatecounterMaxValue argument should be greater than zero.")
  }

  if(all(is.na(rawGateCounts) == TRUE)) {
    stop("\n rawGateCounts doesn't contain numbers for calculation. ")
  }

  if(is.vector(rawGateCounts) == FALSE &&
     tibble::is_tibble(rawGateCounts) == FALSE) {
     stop("\n rawGateCounts should be a numeric vector or tibble")
  }

  # convert gate counts to numeric values and save as tibble
  tibbleCounts <- rawGateCounts %>%
    dplyr::mutate(gateCounts = as.numeric(X2)) %>%
    dplyr::pull(gateCounts)

  tibbleCounts <- tibble(tibbleCounts)

  # Begin calculations
  # Empty vector to capture visitor counts via calculation
  collectValue <- rep(NA, times = nrow(rawGateCounts))

  # Loop for obtaining visitor counts
   for (i in c(1:(nrow(rawGateCounts) - 1))) {
      # 1. Gate type based calculation

        collectValue[i + 1] <- tibbleCounts[i + 1, ] - tibbleCounts[i, ]

        if(printMessages == TRUE) {
          # cat("\n i = ", i, " Calc", i+1, "minus", i, "is:",
          #  unlist(tibbleCounts[i + 1], ),
          #  "-", unlist(tibbleCounts[i], ),
          #  " = ", unlist(collectValue[i + 1]), "\n")
        }

        # 2. Check counts for counter max value or typo
        # If not NA and less than zero, then TRUE
        if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] < 0)) {
          # detecting if a counter max issue
          # If that is the case value/counterMax should be close to 1
          if((tibbleCounts[i, ] / gatecounterMaxValue) >= 0.8) {
            collectValue[i + 1] <- (gatecounterMaxValue - tibbleCounts[i, ]) +
                                    tibbleCounts[i + 1, ]
            if(printMessages == TRUE) {
             # cat("\n The value is negative, due to counter reset,
             #    so new count is fixed to ", unlist(collectValue[i + 1]), "\n")
            }

          } else if((tibbleCounts[i, ] / gatecounterMaxValue) < 0.8) {
            # In this case, likely a typo from user entering data
            collectValue[i + 1] <- NA
            tibbleCounts[i + 1, ] <- NA
            if(printMessages == TRUE) {
              # cat("\n The value is negative, likely due to a error with,
              #  entering, so adjusted to ", unlist(collectValue[i + 1]), "\n")}
            }
          }
        }

        # 3. Check counts for NA values
        # If an NA, then check if the i + 1 or i is NA
        if(is.na(collectValue[i + 1]) == TRUE) {

          # If tibbleCounts[i+1, ] is NA, will not be addressed

          # If i is NA, and it is not the very first entry in loop
          if((is.na(tibbleCounts[i, ]) == TRUE) && (i >= 2)) {

            # Check back on all past values to see if any numeric values
            # Otherwise no point in performing analysis
            # This would be i-c(1:(i-1))
            if(all(is.na(tibbleCounts[i-c(1:(i-1)), ])) == TRUE) {
              if(printMessages == TRUE) {
              # cat("\n No previous count with numeric value present. \n")
              }
            } else if(all(is.na(tibbleCounts[i-c(1:(i-1)), ])) == FALSE) {
              # See how many past counts have numeric values
              # Pick the most recent numeric count to subtract from
              recentCountPlace <- min(which(is.na(tibbleCounts[i-c(1:(i-1)), ]) == FALSE),
                                      na.rm = TRUE)
              collectValue[i + 1] <- (tibbleCounts[i + 1, ] -
                                      tibbleCounts[i - recentCountPlace, ])
              # After adjustment check if a negative value, in case typo
              if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] < 0)) {
                if(printMessages == TRUE) {
                  # cat("\n The value is negative, so tibbleCounts[i + 1, ] is set to NA \n")
                }
                # resetCounter <- 0
                # while(! resetCounter) {
                # for(j in 1:length(which(is.na(tibbleCounts[i-c(1:(i-1)), ]) == FALSE))) {
                  # if negative subtract from previous value
                  tibbleCounts[i + 1, ] <- collectValue[i + 1] <- NA # adjust mistake to NA
                #  collectValue[i + 1] <- (tibbleCounts[i + 1, ] -
                #                            tibbleCounts[i - recentCountPlace - j, ])
                #  if((is.na(collectValue[i + 1]) == FALSE) && (collectValue[i + 1] >= 0)) {
                #    resetCounter <- 1
                #   }
                #  }
                #}
              }
              if(printMessages == TRUE) {
                # cat("\n Adjuted value to be", unlist(collectValue[i + 1]), "\n")
                }
            } else {
              collectValue[i + 1] <- NA # i.e., if one of the first values with
              if(printMessages == TRUE) {
                # cat("\n NA option collectValue[i + 1] = ",
                #  unlist(collectValue[i + 1]), "\n")}
            }
          }
        }

        }

   }


  # Calculations based on visitor counts
  # Cumulative count
  cumulativeCount <- sum(unlist(collectValue), na.rm = TRUE)

  # Summarize daily values
  rawGateCountsEdited <- rawGateCounts %>%
    tibble::add_column(visitorCount = unlist(collectValue)) %>%
    tibble::add_column(date = lubridate::dmy(rawGateCounts$X1))

  dailyVisitorCount <- rawGateCountsEdited %>%
    tibble::add_column(day = lubridate::day(rawGateCountsEdited$date)) %>%
    tibble::add_column(month = lubridate::month(rawGateCountsEdited$date)) %>%
    tibble::add_column(monthAbb = lubridate::month(rawGateCountsEdited$date, label = TRUE)) %>%
    tibble::add_column(year = lubridate::year(rawGateCountsEdited$date)) %>%
    dplyr::select(date, day, month, monthAbb, year, visitorCount)

  if(gateType == "Bidirectional") {
    cumulativeCount <- ceiling(cumulativeCount / 2)
    dailyVisitorCount <- round(dailyVisitorCount$visitorCount / 2, 0)
    if(printMessages == TRUE) {
      cat("\n Cumulative (adjusted) sum for gate type", tolower(gateType), ".")
    }
  }

  # summarizing by monthly count
  monthlyVisitorCount <- dailyVisitorCount %>%
    dplyr::group_by(month, monthAbb, year) %>%
    dplyr::summarise(totalVisitorCount = sum(visitorCount, na.rm = TRUE))

  leastBusiestDay <- dailyVisitorCount %>%
    ungroup() %>%
    dplyr::filter(visitorCount == min(visitorCount, na.rm = TRUE))

  leastBusiestMonth <- monthlyVisitorCount %>%
    ungroup() %>%
    dplyr::filter(totalVisitorCount == min(totalVisitorCount, na.rm = TRUE))

  busiestDay <- dailyVisitorCount %>%
    ungroup() %>%
    dplyr::filter(visitorCount == max(visitorCount, na.rm = TRUE))


  busiestMonth <- monthlyVisitorCount %>%
    ungroup() %>%
    dplyr::filter(totalVisitorCount == max(totalVisitorCount, na.rm = TRUE))

  returnValues <- list(cumulativeCount = cumulativeCount,
                       dailyCounts = dailyVisitorCount,
                       monthlyCounts = monthlyVisitorCount,
                       gateType = gateType,
                       busiestMonth = busiestMonth,
                       leastBusiestMonth = leastBusiestMonth,
                       leastBusiestDay = leastBusiestDay,
                       leastBusiestMonth = leastBusiestMonth)
  class(returnValues) <- c("GateCounts")

  return(returnValues)
}

# END

