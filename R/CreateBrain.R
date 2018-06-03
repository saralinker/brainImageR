#'  Overlap spatial enrichment information and anatomical organization
#'
#'  \code{CreateBrain} convert spatial enrichment into anatomical coordinates.
#'
#' @param composite "Comp" object returned from SpatialEnrichment
#' @param boot significance estimates returned from Boot
#' @param slice brain section
#' @param pcut padj filter.
#' @return "Comp" object
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##First put together a gene list, or load in the default vth dataset
#' data(vth)
#' ##Calculate the spatial enrichment.
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' #tissueExp1 <- composite@tissueExp1
#' #random.matrix <- composite@random.matrix
#' ##Calculate the significance estimates
#' #boot <- Boot(composite)
#' ##Color the brain section of interest with enrichment
#' #composite <- CreateBrain(composite, boot, slice = 6, pcut = 0.05)
#' ##Plot the brain
#' #PlotBrain(composite, Breaks = 12)
#'
#'@export

CreateBrain <- function(composite, boot, slice, pcut = 0.05){
    tissueExp1 <- composite@tissueExp1
    tissueExp2 <- composite@tissueExp2
    random.matrix <- composite@random.matrix
    refset = composite@refset
    refset <- tolower(refset)

    if (refset == "developing"){
        abrev <- .cache[["EH1438"]]
        DIM <- .cache[["EH1436"]]
        outline <- .cache[["EH1440"]]
        conversion <- dev_conversion
        slices <- .cache[["EH1443"]]
        Dataset <- .cache[["EH1434"]]
        }else if (refset == "adult"){
            abrev <- .cache[["EH1439"]]
            DIM <- .cache[["EH1437"]]
            outline <- .cache[["EH1441"]]
            conversion <- .cache[["EH1442"]]
            slices <- .cache[["EH1444"]]
            Dataset <- .cache[["EH1435"]]
        }else{
            stop(paste(c("Please choose refset = developing or adult.")))
            }

    #Get info about the slice
    Abrev <- as.vector(unlist(abrev[slice]))
    Files <- Dataset[[slice]]
    dim <- as.vector(unlist(DIM[slice]))

    subboot <- c(boot[boot$pvalue < pcut & is.finite(boot$FC), "FC"])
    names(subboot) <- rownames(boot[boot$pvalue < pcut & is.finite(boot$FC), ])

    composite1 <- BrainMap(dim = dim,
                            tissueExp = subboot,
                            Abrev = Abrev,
                            Files = Files,
                            slice = slice,
                            refset = refset)
    message("Creating Spatially-enriched Brain Matrix...")
    normalized.1 <- (composite1)
    normalized <- normalized.1
    normalized[composite1 == 0.0001] <- 0.0001
    n_SD <- stats::sd(normalized.1[!is.na(normalized.1)])
    n_max <- max(normalized.1[!is.na(normalized.1)])
    Outlinefill <- n_max + n_SD
    normalized[outline[[slice]] < 1] <- Outlinefill
    comp <- methods::new(Class="Comp",
                        tissueExp1 = tissueExp1,
                        tissueExp2 =  tissueExp2,
                        composite = normalized,
                        random.matrix = random.matrix,
                        refset = refset
    )

    return(comp)
}
