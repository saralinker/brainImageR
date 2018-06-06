#' @title Set class Comp
#' @description
#' Set class Comp
#' @importFrom methods new
#' @export Comp
#' @exportClass Comp
#'
#' @examples
#' comp <- methods::new(Class="Comp",
#' genes = c("a","b"),
#' tissueExp1 = c(10,12),
#' tissueExp2 =  c(10,13),
#' composite = matrix(0,nrow=10,ncol=10),
#' random.matrix = data.frame(matrix(0,nrow=10,ncol=10)),
#' refset = "developing"
#' )
Comp <- methods::setClass(Class="Comp",
                            methods::representation(
                            genes = "character",
                            tissueExp1 = "numeric",
                            tissueExp2 = "numeric",
                            composite = "matrix",
                            random.matrix = "data.frame",
                            refset = "character"))
