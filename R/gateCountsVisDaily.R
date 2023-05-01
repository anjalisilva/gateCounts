#' Visualize Daily Visitor Counts
#'
#' A function that permit to create a plot showing daily visitor counts.
#'
#' @param outputDailyCounts The output from running gateCounts::gateCountSummary()
#'    function.
#' @param scaleBreakYAxisMin Numeric value indicating the minimum
#'    in the range for a scale break to be introduced on y-axis.
#'    The range requires both a minimum and maximum (next argument).
#'    Default value is NA, so no scale break introduced on y-axis.
#' @param scaleBreakYAxisMax Numeric value indicating the maximum
#'    in the range for a scale break to be introduced on y-axis.
#'    The range requires both a minimum (previous argument) and maximum.
#'    Default value is NA, so no scale break introduced on y-axis.
#'
#' @return Returns three plots summarizing daily visitor counts by
#'    week, month and day. Another three plots are produced with
#'    log-transformed counts to better visualize trends.
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
#' randomCountsSumEx1 <- gateCountSummary(
#'              rawGateCounts = randomCounts1tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx1$dailyCounts # access daily adjusted counts
#'
#' # Visualize counts from Example 1
#' visOne <- gateCountsVisDaily(
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
#' randomCountsSumEx2 <- gateCountSummary(
#'              rawGateCounts = randomCounts2tibble,
#'              gateType = "Unidirectional",
#'              gatecounterMaxValue = 200000,
#'              printMessages = FALSE)
#' randomCountsSumEx2$dailyCounts # access daily adjusted counts
#'
#' # Visualize counts from Example 2
#' visTwo <- gateCountsVisDaily(
#'              outputDailyCounts = randomCountsSumEx2)
#'
#' @author Anjali Silva, \email{anjali@alumni.uoguelph.ca}
#'
#' @export
#' @import ggplot2
#' @import ggbreak
gateCountsVisDaily <- function(outputDailyCounts,
                               scaleBreakYAxisMin = NA,
                               scaleBreakYAxisMax = NA) {

  # Daily count
  dailyOuput <- outputDailyCounts$dailyVisitorCounts %>%
  ggplot2::ggplot(aes(x = factor(day),
                      y = visitorCount)) +
    geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(y = "Visitor count", x = "Day",
                  title = paste("Daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::facet_wrap(vars(monthAbb))

  dailyOuputLine <- outputDailyCounts$dailyVisitorCounts %>%
    ggplot2::ggplot(aes(x = day,
                        y = visitorCount)) +
    geom_line(linetype = "dashed", size = 0.3) +
    geom_point(size = 0.5) +
    ggplot2::labs(y = "Visitor count", x = "Day",
                  title = paste("Daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::facet_wrap(vars(monthAbb))

  if((is.na(scaleBreakYAxisMin)) == TRUE && (is.na(scaleBreakYAxisMax) == TRUE)) {
    # trying to introduce scale break for y-axis

    maxVal <- max(outputDailyCounts$dailyVisitorCounts$visitorCount,
        na.rm = TRUE)
    meanVal <- mean(outputDailyCounts$dailyVisitorCounts$visitorCount,
        na.rm = TRUE)

    # rangeYscaleBreak <- c(meanVal, maxVal)
    rangeYscaleBreak <- c(0, 0)
  } else {
    rangeYscaleBreak <- c(scaleBreakYAxisMin,
                          scaleBreakYAxisMax)
  }

  dailyOuputLine2 <- outputDailyCounts$dailyVisitorCounts %>%
    ggplot2::ggplot(aes(x = day,
                        y = visitorCount,
                        group = monthAbb)) +
    geom_line(linetype = "dashed",
              size = 0.5,
              aes(color = monthAbb)) +
    geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Month",
                  title = paste("Daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggbreak::scale_y_break(rangeYscaleBreak) +
    # ggplot2::scale_color_brewer(palette = "Paired")
    scale_color_manual(values=c('#a6cee3',
                                '#1f78b4',
                                '#b2df8a',
                                '#33a02c',
                                '#fb9a99',
                                '#e31a1c',
                                '#fdbf6f',
                                '#ff7f00',
                                '#cab2d6',
                                '#6a3d9a',
                                '#b15928',
                                '#dfc27d'))

  ###
  # Log-transforming to better visualize trends
  # Daily count
  dailyOuputLog <- outputDailyCounts$dailyVisitorCounts %>%
    ggplot2::ggplot(aes(x = factor(day),
                        y = log(visitorCount + 1))) +
    geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(y = "Visitor count", x = "Day",
                  title = paste("Log-transformed daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::facet_wrap(vars(monthAbb))

  dailyOuputLineLog <- outputDailyCounts$dailyVisitorCounts %>%
    ggplot2::ggplot(aes(x = day,
                        y = log(visitorCount + 1))) +
    geom_line(linetype = "dashed", size = 0.3) +
    geom_point(size = 0.5) +
    ggplot2::labs(y = "Visitor count", x = "Day",
                  title = paste("Log-transformed daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::facet_wrap(vars(monthAbb))

  dailyOuputLine2Log <- outputDailyCounts$dailyVisitorCounts %>%
    ggplot2::ggplot(aes(x = day,
                        y = log(visitorCount + 1),
                        group = monthAbb)) +
    geom_line(linetype = "dashed",
              size = 0.5,
              aes(color = monthAbb)) +
    geom_point(size = 0.5, aes(color = monthAbb)) +
    ggplot2::labs(y = "Visitor count",
                  x = "Day",
                  color = "Month",
                  title = paste("Log-transformed daily visitor counts for period of", range(outputDailyCounts$dailyVisitorCounts$date)[1],
                                "to", range(outputDailyCounts$dailyVisitorCounts$date)[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggbreak::scale_y_break(rangeYscaleBreak) +
    # ggplot2::scale_color_brewer(palette = "Paired")
    scale_color_manual(values=c('#a6cee3',
                                '#1f78b4',
                                '#b2df8a',
                                '#33a02c',
                                '#fb9a99',
                                '#e31a1c',
                                '#fdbf6f',
                                '#ff7f00',
                                '#cab2d6',
                                '#6a3d9a',
                                '#b15928',
                                '#dfc27d'))



  return(list(dailyOuput = dailyOuput,
           dailyOuputLine = dailyOuputLine,
           dailyOuputLine2 = dailyOuputLine2,
           dailyOuputLog = dailyOuputLog,
           dailyOuputLineLog = dailyOuputLineLog,
           dailyOuputLine2Log = dailyOuputLine2Log))
 }

# END
