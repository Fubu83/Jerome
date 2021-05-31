#' Dataframe line plot
#'
#' @param df dataframe with four columns
#'
#' @return a line plot
#' @export myseq_n_2
#'
#' @examples
#' my_data <- tibble::tribble(
#' ~x, ~y, ~z, ~n,
#' 2,4,3,3,
#' 2,4,3,4,
#' 2,4,3,5,
#' 2,4,3,6,
#' 2,4,3,7,
#' 2,4,3,8,
#' 2,4,3,9,
#' 2,4,3,10,
#' 2,4,3,12)
#'
#'
#' myseq_n_2(my_data)
#'
myseq_n_2 <- function(df){
  stopifnot(length(df) == 4)
  output <- vector(mode = "numeric", length = length(df[[4]]))
  for (i in seq_along(df[[4]])) {
    output[i] <- myseq_n(x = cbind(df[[1]][i], df[[2]][i], df[[3]][i]), n = df[[4]][i])
    df_2 <- cbind(df, output)
    n <- df_2[[4]]

    ggplot2::ggplot(data = df_2, ggplot2::aes(x = n, y = output)) +
      ggplot2::geom_line() +
      ggplot2::labs(subtitle = "My Sequence: c(3, 2.5, 2.7, 2.783, 2.755, 2.744, 2.748, 2.749, 2.748)") -> plot
  }
  return(plot)
}

