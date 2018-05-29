#' @title brainrange
#'
#' @description
#' replaces seq to pass bioccheck
#'
#'
#' @param first starting value number
#' @param last ending value
#' @param by amount to move by in sequence
#' @return vector of numbers
#'
#' @examples
#' brainrange(1,10,0.5)
#'
#' @export


brainrange <- function(first = 0, last = 1, by = 1) {
    i <- first - by
    final <- vector()
    while(i < last){
        i <- i + by
        final <- c(final, i)
    }
    a <- final[final <= last]
    return(a)
}
