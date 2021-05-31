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


make_proportion <- function(df, var, group, order_string = NA_character_) {
  df %>%
    group_by({{group}}) %>%
    count({{var}}) %>%
    mutate(prop = n/sum(n),
           order = case_when(
             str_detect({{var}}, order_string) ~ prop,
             TRUE ~ 0
           ),
           order = sum(order))
}

add_proportion <- function(df, var, order_var,
                           order_string = NA_character_) {
  df %>%
    mutate(prop = {{var}}/sum({{var}}),
           order = case_when(
             str_detect({{order_var}}, order_string) ~ prop,
             TRUE ~ 0
           ),
           order = sum(order))
}
