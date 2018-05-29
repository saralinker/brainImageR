#' Calculate p-value from bootstrapped sample
#'
#'  \code{PValue.onetail} Calculates the p-value from a bootstrapped sample
#'
#' @param regions regions to search
#' @param tissueExp1 presence of genes in query
#' @param random.matrix presence of genes at random
#' @return p-value of the significance of tissueExp1 given the random.matrix
#'
#' @examples
#' ##Internal to brainImageR, called within Boot
#' #brainImageR:::loadworkspace()
#' ##First put together a gene list, or load in the default vth dataset
#' data(vth)
#' ##Calculate the spatial enrichment.
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #tissueExp1 <- composite@tissueExp1
#' #random.matrix <- composite@random.matrix
#' #boot <- PValue.onetail(regions = names(tissueExp1),
#' #tissueExp1,
#' #random.matrix)
#'
#'@export

PValue.onetail <- function(regions,tissueExp1,random.matrix){
    a <- stats::na.exclude(random.matrix[regions,])
    b <- tissueExp1[regions]
    if(length(regions) ==1){
        return(sum(a > b) / length(a))
        }else{
            out <- vector()
            for(i in brainrange(1,nrow(a))){
                calc <- (b[i] / mean(as.numeric(stats::na.exclude(a[i,]))))
            if ( !is.na(calc) & calc > 1){
                out <- c(out, sum(a[i,] > b[i]) / ncol(a))
            }else{
                out <- c(out, sum(a[i,] < b[i]) / ncol(a))
            }
                }
            out <- as.list(out)
            names(out) <- regions
            return(out)
        }
}
