#' Visualize Monthly Visitor Counts
#'
#' A function that permit to create a plot showing visitor
#' counts by month for one institute.
#'
#' @param outputDailyCounts The output from running gateCounts::gateCountsToVisitorCounts()
#'    function.
#'
#' @return Returns plot of daily visitor counts.
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
#'                   200000, # max value
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 100000)))
#'
#' randomCounts1tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts1)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts = randomCounts1)
#'
#' # check max value for gate counter maximum
#' max(randomCounts1tibble$counts, na.rm = TRUE) # 200000
#'
#' randomCountsSumEx1 <- gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts1tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx1$dailyCounts # access daily adjusted counts
#'
#' # Visualize counts from Example 1
#' visOne <- visitorCountsVisMonthly(
#'              outputDailyCounts = randomCountsSumEx1)
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
#' randomCounts2tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts2)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts = randomCounts2)
#'
#' # check max value for gate counter maximum
#' max(as.numeric(randomCounts2tibble$counts), na.rm = TRUE) # 200000
#'
#' randomCountsSumEx2 <- gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts2tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx2$dailyCounts # access daily adjusted counts
#'
#' # Visualize counts from Example 2
#' visTwo <- visitorCountsVisMonthly(
#'              outputDailyCounts = randomCountsSumEx2)
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
visitorCountsVisMonthly <- function(outputDailyCounts) {

  colorPaletteCustom <- c(
    '#33a02c',
    '#9e0142',
    '#fee08b',
    '#5e4fa2',
    '#66c2a5',
    '#3288bd',
    '#e6f598',
    '#a6cee3',
    '#c51b7d',
    '#fde0ef',
    '#e31a1c',
    '#cab2d6',
    '#ff7f00',
    '#b15928',
    '#dfc27d',
    '#8dd3c7',
    '#ccebc5',
    '#f1b6da')


  # Monthly count

  monthlyOuput <- outputDailyCounts$monthlyVisitorCounts %>%
    ggplot2::ggplot(aes(x = monthAbb,
                        y = totalVisitorCount,
                        fill = monthAbb)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(aes(label = factor(totalVisitorCount)),
              vjust=0) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Month",
                  color = "Month",
                  fill = "Month",
                  title = paste("Monthly visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10))


  return(monthlyOuput)
}


# END
