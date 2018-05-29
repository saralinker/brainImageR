#' Quick search for presence of genes in the ABA list
#'
#'  \code{InABA} checks for the gene name in the ABA dataset
#'
#' @param genes genes to search
#' @param refset reference brain map. developing (default) or adult
#' @return returns the list of genes that are also present in the ABA dataset
#' @examples
#'
#' #brainImageR:::loadworkspace()
#' ##First load in a gene set
#' data(vth)
#' ##Then query the dataset to see which genes are present
#' #vth_in <- InABA(vth)
#' #head(vth_in)
#' #length(vth_in) / length(vth)
#'
#' @export



InABA <- function(genes, refset = "developing"){
    rowmeta <- .cache[["EH1449"]]
    refset <- tolower(refset)
    all_genes <- as.character(rowmeta$gene_symbol)
    genes2 <- genes[(!is.na(rowmeta[match(genes,all_genes),"probeset_id"]))]
    return(genes2)
}
