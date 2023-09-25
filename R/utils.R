#' function to format numbers to text with 1,000s comma
#'
#' @param number value to format
#'
#' @export comma
#'
comma <- function( number ) {
  format(round(as.numeric( number ), digits = 0), big.mark = ",")
}
