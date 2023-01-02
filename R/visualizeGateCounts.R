#' Visualize Cumulative Daily Visitor Counts and Raw Gate Counts
#'
#' A function that permit to create a line plot showing visitor
#' counts and raw gate counts by day. Important: the counts may
#' not reflect daily counts, rather adjusted visitor count as
#' of the day. See README file examples for details.
#'
#' @param rawGateCounts A numeric vector of length corresponding to days or
#     a tibble of dimensions days x 1, containing values of raw daily gate
#     counts. Here days is the number of days for which raw gate counts
#'    are present.
#' @param gateType A character string with options "Unidirectional" or
#     "Bidirectional", to indicate gate type. If the gate is one-way only,
#     then enter "Unidirectional". If the gate permits visitors in and out,
#     then enter "Bidirectional". Bidirectional selection will lead to
#     count sum being divided by two. The default value is "Unidirectional".
#' @param unadjustedDailyCounts A numeric vector of length corresponding to
#'    days, containing values of corrected daily visitor counts, not adjusted
#'    for gate directionality. This would be the 'unadjustedDailyCounts'
#'    output from gateCounts::gateCountCumulative() function in this package.
#'
#' @return Returns a dotted plot of cumulative daily visitor counts and
#'    raw gate count. Important: the counts may not reflect daily counts,
#'    rather adjusted visitor count as of the day. See README file examples
#'    for details.
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
#' @examples
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
#'
#' # Visualize counts from Example 1
#' visOne <- gateCountsVisualize(
#'              rawGateCounts = randomCounts1,
#'              gateType = "Unidirectional",
#'              unadjustedDailyCounts = randomCountsSumEx1$unadjustedDailyCounts)
#'
#' # Example 2: Bidirectional gates with NA values
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
#'
#' # Visualize counts from Example 2
#' visTwo <- gateCountsVisualize(
#'              rawGateCounts = as.numeric(randomCounts4),
#'              gateType = "Bidirectional",
#'              unadjustedDailyCounts = randomCountsSumEx4$unadjustedDailyCounts)
#'
#'
#' @export
#' @import tibble
gateCountsVisualize <- function(rawGateCounts,
                                gateType = "Unidirectional",
                                unadjustedDailyCounts) {

  # Check input arguments
  if(all(is.na(rawGateCounts) == TRUE)) {
    stop("\n rawGateCounts doesn't contain numbers for calculation. ")
  }

  if(is.vector(rawGateCounts) == FALSE &&
     tibble::is_tibble(rawGateCounts) == FALSE) {
    stop("\n rawGateCounts should be a numeric vector or tibble")
  }

  if(gateType != "Bidirectional" && gateType != "Unidirectional") {
    stop("\n gateType argument can only take on values Bidirectional
         or Unidirectional")
  }


  if(gateType == "Unidirectional") {
    par(mfrow=c(1, 2))
    plot(rawGateCounts, type = "l", lty = 5, xlab = "Day",
         ylab = "Gate counts", main = "Unadjusted Unidirectional \n Gate Counts")
    plot(unadjustedDailyCounts, type = "l", lty = 5, xlab = "Day",
         ylab = "Visitor counts",
         main = "Adjusted Visitor Counts \n for Unidirectional Gate")
  } else {
    par(mfrow=c(1, 2))
    plot(rawGateCounts, type = "l", lty = 5, xlab = "Day",
         ylab = "Gate counts", main = "Unadjusted Bidirectional \n Gate Counts")
    unadjustedDailyCounts2 <- ceiling(unadjustedDailyCounts / 2)
    plot(unadjustedDailyCounts2, type = "l", lty = 5, xlab = "Day",
         ylab = "Approx. visitor counts",
         main = "Adjusted Visitor Counts \n for Bidirectional Gate")

  }
  return(NULL)
}



# END
