#' @title Internal- Overlaps regional enrichment into a single section
#'
#' @description
#'  \code{BrainMap} Merges maps from reColor
#'
#' @param dim numeric dimensions of the original image
#' @param tissueExp counts of genes per tissue, from SpatialEnrichment.
#' @param Abrev character of all regions in the given section
#' @param Files character of tiff images for each region
#' @param slice integer of current slice
#' @param refset character of reference brain map
#' @return returns a matrix weighted by the gene overlap
#'
#' @examples
#' ##Internal to brainImageR,called within CreateBrain
#' #brainImageR:::loadworkspace()
#' ##First load in a gene set
#' data(vth)
#' ##calculate the spatial enrichment
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #tissueExp <- Boot(composite)
#'
#' ##Select the slice of interest
#' #slice <- 4
#' #Files <- .cache[["EH1434"]][[slice]]
#' #dim <- .cache[["EH1436"]][[slice]]
#'
#' #Select the region of interest
#' #Abrev <- .cache[["EH1438"]]
#' #abrev <- "VZ"
#'
#'
#'# map <- BrainMap(dim = dim ,
#'# tissueExp = tissueExp,
#'#  Abrev = Abrev, Files = Files,
#'#  slice = slice,
#'# refset = "developing")
#'
#'
#'@export

BrainMap <- function(dim,tissueExp,Abrev,Files, slice,
                        refset = c("developing", "adult")){
    refset <- match.arg(refset)
    tmp <- do.call("cbind",
                    lapply(X=Abrev,
                            FUN=reColor,
                            tissueExp = tissueExp,
                            dim=dim,
                            Abrev = Abrev,
                            Files = Files,
                            slice = slice,
                            refset = refset))
    tmp2 <- matrix(apply(X=tmp,MARGIN=1,FUN=sum),nrow=dim[1],ncol=dim[2])
    return(tmp2)
}
