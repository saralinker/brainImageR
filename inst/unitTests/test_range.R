#' @title test_range
#'
#' @description
#' tests range function
#'
#'
#' @param first starting value number
#' @param last ending value
#' @param interval
#' @return vector of numbers
#'
#' @examples
#' test_range(1,10,0.5)


test_range <- function(first = 0, last = 10, by = 1) {
  a <- range(first, last, by)
  passed <- FALSE
  if(first > last){
    if (length(a) == 0){
      passed <- TRUE
    }
    else{
      passed <- FALSE
    }
  }else if ((((last -first)/by) + 1) == length(a))
    passed <- TRUE
  return(passed)
}
