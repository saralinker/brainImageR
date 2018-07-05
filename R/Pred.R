#' @title Pred
#' @description keeps track of parameters and results from predict_time
#' @slot pred_age data.frame of results from predict_time
#' @slot model randomForest model
#' @slot minage minimum age filter from predict_time
#' @slot maxage maximum age filter from predict_time
#' @slot tissue tissue filter from predict_time
#' @importFrom methods new
#' @export Pred
#' @exportClass Pred
#'
#' @examples
#' prep <- methods::new(Class="Pred",
#' pred_age = data.frame(matrix(0,nrow=10,ncol=10)),
#' model =  list(c(rep("A",5), rep("B",5))),
#' minage = 8,
#' maxage = 2120,
#' tissue = "HIP"
#' )

Pred <- methods::setClass(Class="Pred",
                            methods::representation(pred_age = "data.frame",
                                                    model = "list",
                                                    minage = "numeric",
                                                    maxage = "numeric",
                                                    tissue = "character"
                                                    ))
