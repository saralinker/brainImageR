#' @title List of areas that are present in a plotted brain slice
#'
#' @description provides all brain areas within a slice for a refset.
#'
#' @param composite Object from either SpatialEnrichment or CreateBrain
#' @param slice Section of the brain to be queried
#' @return all areas present in the brain section of interest
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##Load in a gene set
#' data(vth)
#' ##calculate spatial enrichment
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #available_areanames(composite, slice = 5)
#'
#' @export
#'
available_areanames <- function(composite, slice = NULL){
    refset <- tolower(composite@refset)
    if (refset == "developing"){
        conversion <- dev_conversion
        slices <- .cache[["EH1443"]]

    }else if (refset == "adult"){
        conversion <- .cache[["EH1442"]]
        slices <- .cache[["EH1444"]]
    }else{
        stop("Please choose a refset = developing or adult.")
    }
    panel <- conversion[,paste("panel",slices[slice],sep="")]
    nm <- unique(as.character(panel[!is.na(panel)]))
    nm <- nm[order(nm)]
    return(nm)

}
