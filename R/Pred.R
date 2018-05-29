#' @title Set class Pred
#' @description
#' Set class Pred
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
