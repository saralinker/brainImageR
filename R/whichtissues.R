#' @title Identify the tissues where a set of genes are expressed
#'
#' @description Identifies which tissues express genes
#'
#' @param g gene list
#' @param refset reference map. developing (default) or adult
#' @return Tissue regions
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' genes <- c("HOXB9", "HOXB10", "VIM")
#' #whichtissues(genes, refset = "developing")
#' @export whichtissues


whichtissues <- function(g, refset = "developing"){
    refset <- tolower(refset)
    if (refset == "developing"){
        abatissues <- .cache[["EH1445"]]
    }else if (refset == "adult"){
        abatissues <- .cache[["EH1446"]]
    }else{
        stop(paste(c("Please choose refset = developing or adult.")))
    }
    if (length(g) == 1){
        a2 <- colSums(abatissues[g,])
        a2 <- a2[a2 !=0]
    }else{
        a2 <- abatissues[g,]
        a2 <- stats::na.exclude(a2)
        a2 <- a2[,colSums(a2) != 0]
    }
    return(a2)
}
