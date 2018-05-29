#' @title List of regions that are supported by given tissue
#'
#' @description Brain areas supported by tissue of interest. Opposite=tis_set()
#'
#' @param composite object returned from SpatialEnrichment or CreateBrain
#' @param tissue_abrev abreviation of the microdissected tissue of interest.
#' @return  general brain areas
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##Load in a gene set
#' data(vth)
#' ##calculate spatial enrichment
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #tis_in_region(composite, "LHAa")
#' @export
#'

tis_in_region <- function(composite, tissue_abrev){

    refset = composite@refset
    refset <- tolower(refset)

    if (refset == "developing"){
        conversion <- dev_conversion
        slices <- .cache[["EH1443"]]
    }else if (refset == "adult"){
        conversion <- .cache[["EH1442"]]
        slices <- .cache[["EH1444"]]
    }else{
        stop(paste(c("Please choose refset = developing or adult.")))
    }

    panel <- conversion[conversion$acronym == tissue_abrev,-c(1)]
    areas = as.character(panel[!is.na(panel)])
    panel <- do.call("rbind",strsplit(x = names(panel)[!is.na(panel)],
                                        split = "panel",
                                        fixed = TRUE))[,2]
    s <- match(panel,slices)
    if(length(s) == 0){
        return("Tissue not present in the Allen Brain Reference Map")
    }else{
        return(data.frame(slice = s[!is.na(s)],areas = areas[!is.na(s)]))
    }
}
