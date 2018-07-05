#' @title Comp keeps track of Spatial Enrichment calculations and parameters
#' @description
#' Comp tracks the parameters and calculations throughout spatial
#' gene set enrichment.
#' @slot genes character vector of query genes
#' @slot tissueExp1 named numeric vector of query gene count in tissues
#' @slot tissueExp2 named numeric vector of avg.
#'  background gene count in tissues
#' @slot composite composite image matrix
#'
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
