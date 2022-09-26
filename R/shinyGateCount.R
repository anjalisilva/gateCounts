#' Launch Shiny App For Package gateCounts
#'
#' A function that launches the shiny app for gateCounts
#' package. The shiny app permit to calculate cumulative
#' gate count with adjustments, where needed. Visualization
#' can be performed.
#'
#' @return No return value but open up a shiny page.
#'
#' @examples
#' \dontrun{
#' shinyGateCount()
#' }
#'
#' @author Anjali Silva, \email{a.silva@utoronto.ca}
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
#' @importFrom shiny runApp
shinyGateCount <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "gateCounts")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# END
