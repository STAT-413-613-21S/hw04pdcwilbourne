#' Calculate New Value of (Specific) Recursive Sequence
#'
#' For the recursive sequence xn = xn−1 + ((xn−3 − xn−2)/n), returns the
#' nth value in the sequence given the first three values.
#'
#' @param x a vector of the first three numbers in the sequence
#' @param n the integer value in the sequence to return
#'
#' @return the value of the nth element in the sequence
#' @export calcn
#'
#' @examples
#' calcn(c(2, 4, 3), 4) # returns 2.5
#' \dontrun{calcn(c(2, 4), 4) #returns an error}
calcn <- function(x, n) {
  #test inputs
  checkcalcnerrors(x,n)

  #write function
  results <-  vector(mode = "double")
  results[1:3] <-  x[1:3]
  for (i in seq(n)) {
    if (i>3)
      results[i] <- results[i-1] + (results[i-3] - results[i-2])/i
  }
  return(results[n])
}

checkcalcnerrors <- function(x,n) {
  # test that x and n are of correct length
  try(if(length(x) != 3) stop("x is not of length 3"))
  try(if(purrr::is_list(n)) stop("n cannot be a list"))
  # test that x and n are numeric
  try(if(!purrr::is_numeric(x)) stop("x is not numeric"))
  try(if(!purrr::is_numeric(n)) stop("n is not numeric"))
  # test that n is a positive integer
  try(if(n<=0) stop("n must be positive"))
  try(if(n%%1 != 0) stop("n must be an integer"))

  # write function
}

testcalcn <- function() {
  calcn(c(2, 4, 3), 4)
  calcn(c(11,1,130), 1000L)
  calcn(c(11,1,130), 1L)
  calcn(c(7, 3, 20), 8L)
}
