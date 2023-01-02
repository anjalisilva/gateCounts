#' Visualize Cumulative Daily Visitor Counts and Raw Gate Counts
#'
#' A function that permit to create a line plot showing cumulative
#' visitor counts and raw gate counts.
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
#' @return Returns a dotted plot of cumulative daily visitor counts and raw gate count.
#'
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
#' # Cumulative (adjusted) sum for gate type unidirectional is 300618
#' visOne <- gateCountsVisualize(
#'              rawGateCounts = randomCounts1,
#'              gateType = "Unidirectional",
#'              unadjustedDailyCounts = randomCountsSumEx1$unadjustedDailyCounts)
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
    par(mfrow=c(2,2))
    plot(rawGateCounts, type = "l", lty = 5, xlab = "day",
         ylab = "Daily gate counts", main = "Raw Unidirectional Gate Counts")
    plot(unadjustedDailyCounts, type = "l", lty = 5, xlab = "day",
         ylab = "Daily visitor counts",
         main = "Visitor Counts Adjusted for Unidirectional Gate")
  } else {
    par(mfrow=c(1, 2))
    plot(rawGateCounts, type = "l", lty = 5, xlab = "day",
         ylab = "Daily gate counts", main = "Raw Bidirectional Gate Counts")
    unadjustedDailyCounts2 <- ceiling(unadjustedDailyCounts / 2)
    plot(unadjustedDailyCounts2, type = "l", lty = 5, xlab = "day",
         ylab = "Approx. daily visitor counts",
         main = "Visitor Counts Adjusted for Bidirectional Gate")

  }
  return(NULL)
}



# END
