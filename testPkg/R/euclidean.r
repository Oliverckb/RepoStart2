#' @title euclidean function
#' 
#' @describeIn  return the greatest divisor of a and b. 
#' @references see\url{http://https://en.wikipedia.org/wiki/Euclidean_algorithm}.
#' 
#' @details nothing for more details
#' 
#' @param a  A number.
#' @param b  A number.
#' @return the greatest divisor of \code{a} and \code{b} 
#' @usage euclidean(a,b)
#' 
#' @export
#' @examples 
#' euclidean(10,20)
#' euclidean(112,4522)
euclidean <- function(a,b){
  stopifnot((is.numeric(a) && is.numeric(b)) == TRUE)
  while(b != 0){
    tem <- b
    b <- a%%b
    a <- tem
  }
  return(abs(a))
}