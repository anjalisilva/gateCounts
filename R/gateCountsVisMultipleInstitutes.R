# Heatmap

# sample <- dailyVisitorCountTibble %>%
#   select(dateFormat, counts, counts)
#
#   dailyHeatmap <- dailyVisitorCountTibble %>%
#     ggplot2::ggplot(aes(x = "",
#                         y = dateFormat,
#                         fill = log(counts + 1))) +
#     ggplot2::geom_tile() +
#     ggplot2::scale_fill_gradient(low = "white", high = "red") +
#     ggplot2::theme_bw() +
#     ggplot2::labs(y = "Date",
#                   x = "Institute",
#                   fill = "Log-transformed visitor count",
#                   title = paste("Log-transformed daily visitor counts for period of",
#                                 range(dailyVisitorCountTibble$dateFormat)[1],
#                                 "to", range(dailyVisitorCountTibble$dateFormat)[2])) +
#     ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#     ggplot2::scale_y_date(date_labels = "%d-%m-%Y", breaks = scales::breaks_pretty(10))
#
#
# dailyHeatmap <- dailyVisitorCountTibble %>%
#   ggplot2::ggplot(aes(x = "",
#                       y = dateFormat,
#                       fill = log(counts + 1))) +
#   ggplot2::geom_tile() +
#   ggplot2::scale_fill_gradient(low = "white", high = "red") +
#   ggplot2::theme_bw() +
#   ggplot2::labs(y = "Date",
#                 x = "Institute",
#                 fill = "Log-transformed visitor count",
#                 title = paste("Log-transformed daily visitor counts for period of",
#                               range(dailyVisitorCountTibble$dateFormat)[1],
#                               "to", range(dailyVisitorCountTibble$dateFormat)[2])) +
#   ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   ggplot2::scale_y_date(date_labels = "%d-%m-%Y", breaks = scales::breaks_pretty(10))
