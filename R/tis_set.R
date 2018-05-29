#' @title List of tissues that support a given region in the brain plot
#'
#' @description  Tissues that support brain area. Opposite=tis_in_region()`
#'
#' @param composite object returned from SpatialEnrichment or CreateBrain
#' @param area.name abreviation of the brain area of interest.
#' @param slice section of the brain to query (1-10)
#' @return all areas supported by the tissue
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##Load in a gene set
#' data(vth)
#' ##calculate spatial enrichment
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #get the set of tissues that are present within a given region
#' #tis_set(composite, area.name = "Pu", slice = 6)
#' @export
#'

tis_set <- function(composite, area.name, slice){

refset = composite@refset
refset <- tolower(refset)

if (refset == "developing"){
    conversion <- dev_conversion
    slices <- .cache[["EH1443"]]
    colmeta <- .cache[["EH1447"]]
}else if (refset == "adult"){
    conversion <- .cache[["EH1442"]]
    slices <- .cache[["EH1444"]]
    colmeta <- .cache[["EH1448"]]
}else{
    stop(paste(c("Please choose refset = developing or adult.")))
}

panel <- conversion[,c("acronym",paste("panel",slices[slice],sep=""))]
panel <- panel[panel[,2] == area.name,]
regions <- as.character(panel[!is.na(panel[,2]), "acronym"])

c_sa <- colmeta$structure_acronym
c_sn <- colmeta[match(regions, c_sa),"structure_name"]
a <- data.frame(abrev = regions,
                structure_name = as.character(c_sn))

if(length(regions) == 0){
    return("No tissues support this region in the reference set")
}else{
    return(a)
}
}
