#' Generate a random overlap
#'
#' @description random overlap for background correction and bootstrapping
#'
#' @param i current iteration
#' @param genes all genes to sample from
#' @param samplesize sample size to select from gene list
#' @param refset reference map. developing (default) or adult
#' @return returns a list of the random gene overlap for each tissue
#'
#' @examples
#' ##Internal to brainImageR, called within SpatialEnrichment
#' #brainImageR:::loadworkspace()
#' ##First load in a gene set
#' data(vth)
#' #tissueExp <- RandomTissueSummary(1, vth, 20)
#'
#'
#'@export

RandomTissueSummary <- function(i, genes, samplesize,
                                refset = c("developing", "adult")){
    refset <- match.arg(refset)
    genes.1 <-  sample(x=genes,size=samplesize ,replace=FALSE)
    tissueExp2 <- TissueSummary(genes.1,refset = refset)
    return(tissueExp2)
}
