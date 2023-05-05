# Heatmap
dailyHeatmap <- outputDailyCounts$dailyVisitorCounts %>%
  ggplot2::ggplot(aes(x = "",
                      y = date,
                      fill = log(visitorCount + 1))) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "red") +
  ggplot2::theme_bw() +
  ggplot2::labs(y = "Date",
                x = "", # put multiple libraries here
                fill = "Log-transformed visitor count",
                title = paste("Log-transformed daily visitor counts for period of",
                              range(outputDailyCounts$dailyVisitorCounts$date)[1],
                              "to", range(outputDailyCounts$dailyVisitorCounts$date)[2]))

