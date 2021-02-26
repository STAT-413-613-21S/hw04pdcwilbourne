#' Plots Output Values for Recursive Sequence
#'
#' Calcn2 plots the output values of a specific recursive sequence over
#' time given a tibble with information on the sequence's values as input.
#'
#' @param tibble1
#' a tibble first three digits in the sequence and the nth value to calculate
#'
#' @return a plot of the output values for different values of n
#' @export calcn2
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
#' calcn2(my_data)
#'

calcn2 <- function(tibble1) {
  tibble1$output <- 0
  for (i in seq(nrow(tibble1))) {
    tibble1$output[i] <- calcn(c(tibble1[[i,1]], tibble1[[i,2]], tibble1[[i,3]]),
                               n = tibble1[[i,4]])
  }
  output_plot <- ggplot2::ggplot(tibble1, ggplot2::aes(n, output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Recursive Output", x = "n", y = "Output")
  return(output_plot)
}
