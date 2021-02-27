#' Plots Output for a (Specific) Recursive Sequence
#'
#' For the recursive sequence xn = xn-1 + ((xn-3 - xn-2)/n), plots the output
#' of the sequence given as input a tibble with information on the sequence's
#' beginning values and the specific numbers in the sequence to plot.
#'
#' @param rcrsv_df
#' A tibble with four columns: the first three digits in the sequence, and the
#' nth value to calculate. The first three columns typically remain the same,
#' while the last column increases.
#'
#' @return Returns a plot of the output.
#'
#' @export rcrsv_plot
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
#' rcrsv_plot(my_data) # plots the nth outcomes of the sequence from the
#' fourth column
#'
#' my_wrong_data <- tibble::tribble(
#' ~x, ~y, ~z, ~n, ~p,
#' 2,4,3,3,5,
#' 2,4,3,4,5,
#' 2,4,3,5,5,
#' 2,4,3,6,5,
#' 2,4,3,7,5,
#' 2,4,3,8,5,
#' 2,4,3,9,5,
#' 2,4,3,10,5,
#' 2,4,3,12,5)
#'
#' \dontrun{rcrsv_plot(my_wrong_data) # returns an error}
#'

rcrsv_plot <- function(rcrsv_df) {
  # test inputs
  rcrsv_plot_checkinputs(rcrsv_df)

  # write function
  rcrsv_df$output <- 0
  # generate output column
  for (i in seq(nrow(rcrsv_df))) {
    rcrsv_df$output[i] <- rcrsv_solve(c(rcrsv_df[[i,1]], rcrsv_df[[i,2]], rcrsv_df[[i,3]]),
                               n = rcrsv_df[[i,4]])
  }
  # plot output
  output_plot <- ggplot2::ggplot(rcrsv_df, ggplot2::aes(n, output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Recursive Output", x = "n", y = "Output")
  return(output_plot)
}

rcrsv_plot_checkinputs <- function(rcrsv_df) {
  # test that rcrsv_df is a dataframe
  if(!is.list(rcrsv_df)) {
    stop("input is not a list/dataframe")
  }
  # test that rcrsv_df has four columns
  if(ncol(rcrsv_df) != 4) {
    stop("input does not have four columns")
  }
  # test that each column is numeric
  for (i in seq(length(rcrsv_df))) {
    if(!is.numeric(rcrsv_df[,i] %>% unlist())) {
      stop("column " + i + " is not numeric")
    }
  }
  # test that column four contains only positive values
  for (i in seq(length(rcrsv_df[,4]))) {
    if(rcrsv_df[i,4] %>% unlist() <= 0) {
      stop("column 4 must be greater than zero")
    }
  }
  # test that column four contains only integers
  for (i in seq(length(rcrsv_df[,4]))) {
    if(rcrsv_df[i,4] %>% unlist() %% 1 != 0) {
      stop("column 4 must be an integer")
    }
  }
}
