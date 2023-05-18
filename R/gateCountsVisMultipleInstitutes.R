#' Visualize Daily Visitor Counts of Multiple Institutes
#'
#' A function that permit to create plots showing daily visitor counts
#' for multiple insitutes.
#' Plots include line plots and barplots of month vs daily
#' counts. Plots with log-transformed counts are also provided
#' to better visualize trends with outliers.
#'
#' @param dailyVisitorCount A numeric vector or a tibble, with
#'    number of rows equaling to length of days and columns equaling
#'    to 1 + number of institutes, such that the dimension is: days x 1
#'    + number of institutes. Here, days is the number of days for
#'    which visitor counts are present.
#'    First column must contain the dates and should contain column name
#'    "dates". Dates must be in the format of date-month-year. The second
#'    column must contain the visitor count for the given date and
#'    should be called "counts1".
#'
#' @return Returns three plots summarizing daily visitor counts by
#'    week, month and day.
#' \itemize{
#'   \item dailyOuput - A bar plot of daily visitor count values
#'         by month.
#'   \item dailyOuputLine - A line plot of daily visitor count values
#'         by month.
#'   \item dailyOuputLine2 - A line plot of daily visitor count values
#'         by month, laid over in one view.
#'   \item dailyOuputLog - A bar plot of daily visitor count values
#'         by month.
#'   \item dailyOuputStackedLog - A bar plot of log-transformed daily
#'   visitor count values by month.
#'   \item dailyOuputLineLog - A line plot of log-transformed daily
#'   visitor count values by month.
#'   \item dailyOuputLine2Log - A line plot of log-transformed daily
#'   visitor count values by month, laid over in one view.
#'   \item dailyHeatmap - A heatmap of log-transformed daily
#'   visitor count values
#' }
#'
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @examples
#' set.seed(1234)
#' # Example 1: Unidirectional gates with daily counts
#' # Simulate gate count data using Poisson distribution
#' randomCounts1 <- c(sort(rpois(n = 50, lambda = 10000)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 10)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 10000)))
#'
#' # Create a tibble with date information
#' randomCounts1tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts1)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts = randomCounts1)
#'
#' set.seed(2345)
#' randomCounts2 <- c(sort(rpois(n = 50, lambda = 10)),
#'                   sort(rpois(n = 50, lambda = 50)),
#'                   sort(rpois(n = 82, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 10)),
#'                   sort(rpois(n = 50, lambda = 3)),
#'                   sort(rpois(n = 50, lambda = 10000)))
#'
#' # Create a tibble with date information
#' randomCounts2tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts2)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts2 = randomCounts2)
#'
#' # combine data
#' multipleTibble <- randomCounts1tibble %>%
#'   dplyr::left_join(randomCounts2tibble)
#'
#' # Check max value for gate counter maximum
#' max(multipleTibble$counts, na.rm = TRUE) # 10,254
#' max(multipleTibble$counts2, na.rm = TRUE) # 10,197
#'
#' # Visualize
#' visPutEx1 <-
#'    gateCountsVisDaily(dailyVisitorCount = visitorCountsEx1)
#'
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
#'
#' @export
#' @import ggplot2
#' @import tidyverse
gateCountsVisDaily <- function(dailyVisitorCount) {

  # checking
  if(is.vector(dailyVisitorCount) == FALSE &&
     tibble::is_tibble(dailyVisitorCount) == FALSE) {
    stop("\n dailyVisitorCount should be a numeric vector or tibble.")
  }

  if(ncol(dailyVisitorCount) != 2L) {
    stop("\n dailyVisitorCount should be a numeric vector or tibble with
         2 columns: dates and counts.")
  }

  if(all(colnames(dailyVisitorCount) != c("dates", "counts"))) {
    stop("\n dailyVisitorCount should be a numeric vector or tibble with
         2 columns each named 'dates' and 'counts'.")
  }

  # set color palette
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

  # convert to a tibble
  dailyVisitorCountTibble <- tibble::tibble(dailyVisitorCount)

  dailyVisitorCountTibble <- dailyVisitorCountTibble %>%
    tibble::add_column(dateFormat = lubridate::dmy(dailyVisitorCountTibble$dates))

  # date summary
  dailyVisitorCountTibble <- dailyVisitorCountTibble %>%
    tibble::add_column(day = lubridate::day(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(weekDay = lubridate::wday(dailyVisitorCountTibble$dateFormat, label = TRUE)) %>%
    tibble::add_column(week = lubridate::week(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(month = lubridate::month(dailyVisitorCountTibble$dateFormat)) %>%
    tibble::add_column(monthAbb = lubridate::month(dailyVisitorCountTibble$dateFormat, label = TRUE)) %>%
    tibble::add_column(year = lubridate::year(dailyVisitorCountTibble$dateFormat)) %>%
    dplyr::select(dateFormat, counts, day, weekDay, week, month, monthAbb, year)


  # Daily count bar plot
  dailyOuput <-  dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = counts,
                        fill = monthAbb)) +
    geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  fill = "Month",
                  title = paste("Daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::facet_wrap(vars(monthAbb))
}
