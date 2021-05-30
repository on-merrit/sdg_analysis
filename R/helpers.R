as_year <- function(num_val) lubridate::ymd(num_val, truncated = 2L)


plot_correlation <- function(cor_matrix, cluster = TRUE) {
  # code from http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

  order <- ifelse(cluster, "hclust", "original")

  corrplot::corrplot(cor_matrix, method = "color", col = col(200),
                     type = "upper", order = order,
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col = "black", tl.srt = 45, #Text label color and rotation
                     # hide correlation coefficient on the principal diagonal
                     diag = FALSE
  )
}
