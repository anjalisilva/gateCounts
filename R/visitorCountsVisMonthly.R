#' Visualize Monthly Visitor Counts
#'
#' A function that permit to create a plot showing visitor
#' counts by month for one institute.
#'
#' @param dailyVisitorCount A numeric vector or a tibble, with
#'    number of rows equaling to length of days and columns equaling
#'    to two, such that the dimension is: days x 2. Here
#'    days is the number of days for which visitor counts are present.
#'    First column must contain the dates and should contain column name
#'    "dates". Dates must be in the format of date-month-year. The second
#'    column must contain the visitor count for the given date and
#'    should be called "counts". This input could be obtained from
#'    running gateCounts::gateCountsToVisitorCounts() or
#'    gateCounts::visitorCountSummary() functions.
#'
#' @return Returns plot of monthly visitor counts.
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
#' library("magrittr")
#' library("lubridate")
#' library("tibble")
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
#' # Turn gate counts to visitor counts
#' randomCountsSumEx1 <- gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts1tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx1$dailyVisitorCounts # access daily adjusted counts
#'
#' # Visualize visitor counts using visitorCountSummary function
#' # First, format the table
#' visitorCountsEx1 <-
#'   randomCountsSumEx1$dailyVisitorCounts %>%
#'   dplyr::rename("counts" = "visitorCount") %>%
#'   dplyr::select("dates", "counts")
#'
#' # Visualize visitor counts from Example 1
#' visOne <- gateCounts::visitorCountsVisMonthly(
#'              dailyVisitorCount = visitorCountsEx1)
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
#' # Randomly introduce NA and "Gate broken" entries
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
#' # Check max value for gate counter maximum
#' max(as.numeric(randomCounts2tibble$counts), na.rm = TRUE) # 200000
#'
#' # Turn gate counts to visitor counts
#' randomCountsSumEx2 <- gateCounts::gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts2tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx2$dailyVisitorCounts # access daily adjusted counts
#'
#' # Visualize visitor counts using visitorCountSummary function
#' # First, format the table
#' visitorCountsEx2 <-
#'   randomCountsSumEx2$dailyVisitorCounts %>%
#'   dplyr::rename("counts" = "visitorCount") %>%
#'   dplyr::select("dates", "counts")
#'
#' # Visualize visitor counts from Example 2
#' visTwo <- gateCounts::visitorCountsVisMonthly(
#'              dailyVisitorCount = visitorCountsEx2)
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
visitorCountsVisMonthly <- function(dailyVisitorCount) {

  # Setting colors
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

  # Add date format
  dailyVisitorCountTibble <- dailyVisitorCount %>%
    tibble::add_column(dateFormat = lubridate::dmy(dailyVisitorCount$dates))

  # Summary
  dailyVisitorCountTibble <- dailyVisitorCountTibble %>%
    tibble::add_column(day = lubridate::day(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(weekDay = lubridate::wday(dailyVisitorCountTibble$dateFormat, label = TRUE)) %>%
    tibble::add_column(week = lubridate::week(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(month = lubridate::month(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(monthAbb = lubridate::month(dailyVisitorCountTibble$dateFormat, label = TRUE)) %>%
    tibble::add_column(year = lubridate::year(dailyVisitorCountTibble$dateFormat)) %>%
    dplyr::select(dateFormat, counts, day, weekDay, week, month, monthAbb, year)


  # Monthly count
  monthlyOuput <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = monthAbb,
                        y = counts,
                        fill = monthAbb)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Month",
                  color = "Month",
                  fill = "Month",
                  title = paste("Monthly visitor counts for period of", range(dailyVisitorCountTibble$dateFormat)[1],
                                "to", range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10))


  return(monthlyOuput)
}

# END
