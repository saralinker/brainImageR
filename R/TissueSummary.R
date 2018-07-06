#' @title Internal-Identify the number of genes expressed in each tissue
#'
#' @description .
#' Quantifies the presence of gene list within each tissue
#'
#'
#' @param genes Genes to query for tissue location
#' @param refset reference map. developing (default) or adult
#' @return returns genes counts for each tissue
#'
#' @examples
#'
#' #Internal to brainImageR, used within SpatialEnrichment
#' #First load in a gene set
#' #brainImageR:::loadworkspace()
#' data(vth)
#' #tissueExp <- TissueSummary(vth, refset = "developing")
#' @importFrom stats na.exclude
#' @export


TissueSummary <- function(genes, refset = c("developing", "adult")){
    refset <- tolower(match.arg(refset))
    if (refset == "developing"){
        abatissuesBygenes <- .cache[["EH1445"]]
        colmeta <- .cache[["EH1447"]]
    }else if (refset == "adult"){
        abatissuesBygenes <- .cache[["EH1446"]]
        colmeta <- .cache[["EH1448"]]
    }else{
        stop("Please choose refset = developing or adult.")
    }

    if(!is.null(ncol(na.exclude(abatissuesBygenes[genes,])))){
        a1 <- na.exclude(abatissuesBygenes[genes,])
        tissueswithgenes <- colSums(a1)
    }else{
        tissueswithgenes <- na.exclude(abatissuesBygenes[genes,])
    }
    a <- table(c(as.character(colmeta$structure_acronym),
                    as.character(colmeta$structure_acronym),
                    names(tissueswithgenes)))
    a <- a[a==2]
    a[a==2] <- 0
    tissueswithgenes <- c(a, tissueswithgenes)
    tissueswithgenes <- tissueswithgenes[order(names(tissueswithgenes))]

    return((tissueswithgenes))
}
