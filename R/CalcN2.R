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
  for (i in seq(length(tibble1$n))) {
    tibble1$output[i] <- calcn(c(tibble1$x[i], tibble1$y[i], tibble1$z[i]),
                               n = tibble1$n[i])
  }
  ggplot2::ggplot(tibble1, aes(n, output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Recursive Output", x = "n", y = "Output")
  return(tibble1)
}

