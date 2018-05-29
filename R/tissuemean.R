#' @title Internal-Take mean of all samples per tissue
#'
#' @description
#' mean of all samples per tissue
#'
#' @param region region of interest
#' @param LIST full set of all values
#' @return returns the mean of region for the whole list
#'
#' @examples
#' x <- 1
#'
#' @export
#'

tissuemean <- function(region, LIST){
    return(as.numeric(mean(LIST[region])))
}
