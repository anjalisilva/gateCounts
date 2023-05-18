#' Visualize Daily Visitor Counts of Multiple Institutes
#'
#' A function that permit to create plots showing daily visitor counts
#' for multiple institutes. Plots include line plots and barplots.
#' Plots with log-transformed counts are also provided
#' to better visualize trends with outliers.
#'
#' @param visitorCountMultiple A numeric vector or a tibble, with
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
#'   \item dailyOuputStackedLog - A bar plot of log-transformed daily
#'   visitor count values by month.
#'   \item dailyOuputLineLog - A line plot of log-transformed daily
#'   visitor count values by month.
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
#' randomCounts1 <- c(sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 10)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 200)))
#'
#' # Create a tibble with date information
#' randomCounts1tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts1)] %>%
#'                         format('%d-%m-%Y'),
#'                         institute1 = randomCounts1)
#'
#' set.seed(2345)
#' randomCounts2 <- c(sort(rpois(n = 50, lambda = 10)),
#'                   sort(rpois(n = 50, lambda = 5000)),
#'                   sort(rpois(n = 82, lambda = 500)),
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 3000)),
#'                   sort(rpois(n = 50, lambda = 1000)))
#'
#' # Create a tibble with date information
#' randomCounts2tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts2)] %>%
#'                         format('%d-%m-%Y'),
#'                         institute2 = randomCounts2)
#'
#' set.seed(3456)
#' randomCounts3 <- c(sort(rpois(n = 50, lambda = 20000)),
#'                   sort(rpois(n = 50, lambda = 5000)),
#'                   sort(rpois(n = 82, lambda = 500)),
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 3000)),
#'                   sort(rpois(n = 50, lambda = 1000)))
#'
#' # Create a tibble with date information
#' randomCounts3tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts3)] %>%
#'                         format('%d-%m-%Y'),
#'                         institute3 = randomCounts3)
#'
#' # combine data
#' multipleTibble <- randomCounts1tibble %>%
#'   dplyr::left_join(randomCounts2tibble) %>%
#'   dplyr::left_join(randomCounts3tibble)
#'
#' # Check max value for gate counter maximum
#' max(multipleTibble$institute1, na.rm = TRUE) # 1107
#' max(multipleTibble$institute2, na.rm = TRUE) # 5141
#' max(multipleTibble$institute3, na.rm = TRUE) # 20357
#'
#' # Visualize
#' visPutEx1 <-
#'    visitorCountsMultipleVisDaily(
#'    visitorCountMultiple = multipleTibble)
#'
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
#' @import tidyverse
#' @import reshape2
visitorCountsMultipleVisDaily <- function(visitorCountMultiple) {

  # checking
  if(is.vector(visitorCountMultiple) == FALSE &&
     tibble::is_tibble(visitorCountMultiple) == FALSE) {
    stop("\n visitorCountMultiple should be a numeric vector or tibble.")
  }

  if(ncol(visitorCountMultiple) <= 2) {
    stop("\n visitorCountMultiple should be a numeric vector or tibble with
         2 columns: dates and counts.")
  }

  if(colnames(visitorCountMultiple[, 1]) != c("dates")) {
    stop("\n visitorCountMultiple should be a numeric vector or tibble with
         first column named 'dates'.")
  }

  # set color palette
  colorPaletteCustom <- c(
    '#33a02c',
    '#ff7f00',
    '#9e0142',
    '#5e4fa2',
    '#66c2a5',
    '#3288bd',
    '#e6f598',
    '#a6cee3',
    '#c51b7d',
    '#fde0ef',
    '#e31a1c',
    '#cab2d6',
    '#b15928',
    '#dfc27d',
    '#8dd3c7',
    '#ccebc5',
    '#f1b6da')

  # convert to a tibble
  visitorCountMultipleTibble <-
    tibble::tibble(visitorCountMultiple)

  # save the number of columns provided by user
  ncolProvided <- ncol(visitorCountMultipleTibble)

  # melt data
  visitorCountMultipleTibbleMelt <-
    reshape2::melt(visitorCountMultipleTibble,
                  id = c("dates"))

  # add date format
  visitorCountMultipleTibbleMelt <- visitorCountMultipleTibbleMelt %>%
    tibble::add_column(dateFormat = lubridate::dmy(visitorCountMultipleTibbleMelt$dates))



  # date summary
  visitorCountMultipleTibbleMeltData <- visitorCountMultipleTibbleMelt %>%
    tibble::add_column(day = lubridate::day(visitorCountMultipleTibbleMelt$dateFormat)) %>%
    tibble::add_column(weekDay = lubridate::wday(visitorCountMultipleTibbleMelt$dateFormat, label = TRUE)) %>%
    tibble::add_column(week = lubridate::week(visitorCountMultipleTibbleMelt$dateFormat)) %>%
    tibble::add_column(month = lubridate::month(visitorCountMultipleTibbleMelt$dateFormat)) %>%
    tibble::add_column(monthAbb = lubridate::month(visitorCountMultipleTibbleMelt$dateFormat, label = TRUE)) %>%
    tibble::add_column(year = lubridate::year(visitorCountMultipleTibbleMelt$dateFormat))


  # Daily count bar plot
  dailyOuput <-  visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = value,
                        fill = factor(variable))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 1) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  fill = "Institute",
                  title = paste("Daily visitor counts for period of",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::facet_wrap(vars(monthAbb))



  # Daily count line bar with facet
  dailyOuputLine <- visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = value,
                        group =  factor(variable))) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(color =  factor(variable))) +
    ggplot2::geom_point(size = 0.5, aes(color =  factor(variable))) +
    ggplot2::scale_color_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Institute",
                  group = "Institute",
                  title = paste("Daily visitor counts for period of",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::facet_wrap(vars(monthAbb))

  ###
  # Log-transforming to better visualize trends
  # Daily count bar plot with facet
  dailyOuputStackedLog <- visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = log(value + 1),
                        fill = factor(variable))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 1) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  fill = "Institute",
                  title = paste("Daily visitor counts for period of",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::facet_wrap(vars(monthAbb))


  # Line plot with facet
  dailyOuputLineLog <- visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = log(value + 1),
                        group =  factor(variable))) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(color =  factor(variable))) +
    ggplot2::geom_point(size = 0.5, aes(color =  factor(variable))) +
    ggplot2::scale_color_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Institute",
                  group = "Institute",
                  title = paste("Daily visitor counts for period of",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::facet_wrap(vars(monthAbb))



  # Heatmap
  dailyHeatmap <- visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = factor(variable),
                        y = dateFormat,
                        fill = log(value + 1))) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Date",
                  x = "Institute",
                  fill = "Log-transformed \n visitor count",
                  title = paste("Log-transformed daily visitor counts for period of",
                                range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to", range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::scale_y_date(date_labels = "%d-%m-%Y", breaks = scales::breaks_pretty(10))


  # monthly counts
  monthlyOuput <- visitorCountMultipleTibbleMeltData %>%
    ggplot2::ggplot(aes(x = monthAbb,
                        y = value,
                        fill = factor(variable))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 1) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Month",
                  color = "Institute",
                  fill = "Institute",
                  title = paste("Monthly visitor counts for period of", range(visitorCountMultipleTibbleMeltData$dateFormat)[1],
                                "to", range(visitorCountMultipleTibbleMeltData$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10))



  return(list(dailyOuput = dailyOuput,
              dailyOuputLine = dailyOuputLine,
              dailyOuputStackedLog = dailyOuputStackedLog,
              dailyOuputLineLog = dailyOuputLineLog,
              dailyHeatmap = dailyHeatmap,
              monthlyOuput = monthlyOuput))
}

# [END]
