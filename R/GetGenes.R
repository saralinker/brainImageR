#' @title GenGenes
#' @description
#'  \code{GetGenes} returns the genes that are expressed within a given tissue
#'
#' @param genes Query gene list.
#' @param composite Result from SpatialEnrichment
#' @param tissue_abrev The tissue of interest.
#' @return Gene overlap between query and tissue of interest
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##First put together a gene list, or load in the default vth dataset
#' data(vth)
#' ##Calculate the spatial enrichment.
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' ##Ask which genes are present in any given tissue.
#' #available_areanames(composite, slice = 4)
#' #vth_in_VZ <- GetGenes(vth, composite, tissue_abrev = "VZ")
#'
#'@export

GetGenes <- function(genes,composite, tissue_abrev = NULL){
    tissueExp1 <- composite@tissueExp1
    tissueExp2 <- composite@tissueExp2
    refset <- composite@refset
    refset <- tolower(refset)
    if (refset == "developing"){
        abatissuesBygenes <- .cache[["EH1445"]]
    }else if (refset == "adult"){
        abatissuesBygenes <- .cache[["EH1446"]]
    }else{
        stop(paste(c("Please choose refset = developing or adult.")))
    }


    normTissueExp <- tissueExp1 / tissueExp2
    normTissueExp <- normTissueExp[order(normTissueExp,decreasing=TRUE)]
    if(is.null(tissue_abrev)){
        warning("Use available_areanames() for acceptable abbreviations")
    }else{
        Columns <- grep(pattern=tissue_abrev,
                        x=colnames(abatissuesBygenes),
                        ignore.case=TRUE)
        if(length(Columns) > 1){
            Test <- apply(abatissuesBygenes[,Columns],MARGIN=1,FUN=sum)
        }else{
            Test <- abatissuesBygenes[,Columns]
        }
        put.genes <- rownames(abatissuesBygenes[Test > 0,])
        a <- table(c(put.genes,genes))
        return(names(a[a==2]))
    }
}
