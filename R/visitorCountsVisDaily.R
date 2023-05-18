#' Visualize Daily Visitor Counts
#'
#' A function that permit to create plots showing daily visitor counts
#' for one institute. Plots include line plots and barplots of month
#' vs daily counts. Plots with log-transformed counts are also provided
#' to better visualize trends with outliers.
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
#' @return Returns three plots summarizing daily visitor counts by
#'    week, month and day. Another three plots are produced with
#'    log-transformed counts to better visualize trends.
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
#' # Simulate gate count data using Poisson distribution
#' randomCounts1 <- c(sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 82, lambda = 100000)),
#'                   200000, # max value
#'                   sort(rpois(n = 50, lambda = 100)),
#'                   sort(rpois(n = 50, lambda = 1000)),
#'                   sort(rpois(n = 50, lambda = 100000)))
#'
#' # Create a tibble with date information
#' randomCounts1tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts1)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts = randomCounts1)
#'
#' # Check max value for gate counter maximum
#' max(randomCounts1tibble$counts, na.rm = TRUE) # 200000
#'
#' # Run gateCountsToVisitorCounts function
#' randomCountsEx1 <- gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts1tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#'
#' randomCountsEx1$dailyVisitorCounts # access daily adjusted counts
#'
#' # Visualize visitor counts using visitorCountSummary function
#' # First, format the table
#' visitorCountsEx1 <-
#'   randomCountsEx1$dailyVisitorCounts %>%
#'   dplyr::rename("counts" = "visitorCount") %>%
#'   dplyr::select("dates", "counts")
#'
#' # Visualize
#' visPutEx1 <-
#'    visitorCountsVisDaily(dailyVisitorCount = visitorCountsEx1)
#'
#'
#' # Example 2: Bidirectional gates with NA values
#' # Simulate gate count data using Poisson distribution
#' randomCounts2 <- c(sort(rpois(n = 50, lambda = 10000)),
#'                   sort(rpois(n = 50, lambda = 400000)),
#'                   sort(rpois(n = 82, lambda = 800000)),
#'                   999999, # max value
#'                   sort(rpois(n = 50, lambda = 10000)),
#'                   sort(rpois(n = 50, lambda = 450000)),
#'                   sort(rpois(n = 50, lambda = 850000)))
#'
#' # Randomly introduce NA and "Gate broken" entries
#' randomPositions <- sample(x = c(1:length(randomCounts2)),
#'                          size = 8, replace = FALSE)
#' randomCounts2[randomPositions[1:4]] <- NA
#' randomCounts2[randomPositions[5:8]] <- "Gate broken"
#'
#' # Create a tibble with date information
#' randomCounts2tibble <- tibble::tibble(
#'                         dates = seq(lubridate::dmy('01-01-2022'),
#'                         lubridate::dmy('31-12-2022'),
#'                         by='1 day')[1:length(randomCounts2)] %>%
#'                         format('%d-%m-%Y'),
#'                         counts = randomCounts2)
#'
#' # Check max value for gate counter maximum
#' max(as.numeric(randomCounts2tibble$counts), na.rm = TRUE) # 999999
#'
#' # Run gateCountsToVisitorCounts function
#' randomCountsEx2 <- gateCountsToVisitorCounts(
#'              rawGateCounts = randomCounts2tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 999999,
#'              printMessages = FALSE)
#'
#' randomCountsEx2$dailyVisitorCounts # access daily adjusted counts
#'
#' # Visualize visitor counts using visitorCountSummary function
#' # First, format the table
#' visitorCountsEx2 <-
#'   randomCountsEx2$dailyVisitorCounts %>%
#'   dplyr::rename("counts" = "visitorCount") %>%
#'   dplyr::select("dates", "counts")
#'
#' # Visualize
#' visPutEx2 <-
#'    visitorCountsVisDaily(dailyVisitorCount = visitorCountsEx2)
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
#' @import tidyverse
visitorCountsVisDaily <- function(dailyVisitorCount) {

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


  # Daily count line bar with facet
  dailyOuputLine <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = day,
                        y = counts,
                        group = monthAbb)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(color = monthAbb)) +
    geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::scale_color_manual(values = colorPaletteCustom) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Month",
                  group = "Month",
                  title = paste("Daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::facet_wrap(vars(monthAbb))


  # Daily count line bar without facet
  dailyOuputLine2 <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = day,
                        y = counts,
                        group = monthAbb)) +
    ggplot2::geom_line(linetype = "dashed",
              size = 0.5,
              aes(color = monthAbb)) +
    ggplot2::geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Month",
                  title = paste("Daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(rangeYscaleBreak) +
    # ggplot2::scale_color_brewer(palette = "Paired")
    scale_color_manual(values = colorPaletteCustom)


  ###
  # Log-transforming to better visualize trends
  # Daily count bar plot with facet
  dailyOuputStackedLog <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = log(counts + 1),
                        fill = monthAbb)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(y = "Log-transformed visitor count",
                  x = "Day",
                  fill = "Month",
                  title = paste("Log-transformed daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom)


  # Bar plot with facet
  dailyOuputLog <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = log(counts + 1),
                        fill = monthAbb)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(y = "Log-transformed visitor count", x = "Day",
                  fill = "Month",
                  title = paste("Log-transformed daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::facet_wrap(vars(monthAbb))


  # Line plot with facet
  dailyOuputLineLog <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = day,
                        y = log(counts + 1),
                        group = monthAbb)) +
    ggplot2::geom_line(linetype = "dashed",
                       size = 0.7,
                       aes(color = monthAbb)) +
    ggplot2::geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::labs(y = "Log-transformed visitor count",
                  x = "Day",
                  color = "Month",
                  title = paste("Daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(rangeYscaleBreak) +
    ggplot2::scale_color_manual(values = colorPaletteCustom) +
    ggplot2::facet_wrap(vars(monthAbb))


  # Line plot without facet
  dailyOuputLine2Log <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = day,
                        y = log(counts + 1))) +
    ggplot2::geom_line(linetype = "F1",
                       size = 0.8,
                       aes(color = monthAbb)) +
    ggplot2::geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::labs(y = "Log-transformed visitor count",
                  x = "Day",
                  color = "Month",
                  title = paste("Log-transformed daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to",
                                range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # annotation_logticks() +
    # ggplot2::scale_color_brewer(palette = "Paired")
    ggplot2::scale_color_manual(values = colorPaletteCustom)


  # Heatmap
  dailyHeatmap <- dailyVisitorCountTibble %>%
    ggplot2::ggplot(aes(x = "",
                        y = dateFormat,
                        fill = log(counts + 1))) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Date",
                  x = "Institute",
                  fill = "Log-transformed visitor count",
                  title = paste("Log-transformed daily visitor counts for period of",
                                range(dailyVisitorCountTibble$dateFormat)[1],
                                "to", range(dailyVisitorCountTibble$dateFormat)[2])) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::scale_y_date(date_labels = "%d-%m-%Y", breaks = scales::breaks_pretty(10))


  return(list(dailyOuput = dailyOuput,
           dailyOuputLine = dailyOuputLine,
           dailyOuputLine2 = dailyOuputLine2,
           dailyOuputLog = dailyOuputLog,
           dailyOuputStackedLog = dailyOuputStackedLog,
           dailyOuputLineLog = dailyOuputLineLog,
           dailyOuputLine2Log = dailyOuputLine2Log,
           dailyHeatmap = dailyHeatmap))
 }

# END
