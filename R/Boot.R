#' @title Calculate significance of gene set enrichment
#'
#' @description
#' \code{Boot} bootstraps the likelihood of the observed enrichment
#'
#'
#' @param composite Object returned from SpatialEnrichment.
#' @return spatiotemporal prediction
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
#' #boot <- boot[order(boot$FC, decreasing=TRUE),]
#' #head(boot)
#'
#' @export

Boot <- function(composite){
    dev_colmeta <- .cache[["EH1447"]]
    ad_colmeta <- .cache[["EH1448"]]
    tissueExp1 <- composite@tissueExp1
    random.matrix <- composite@random.matrix
    boot <- PValue.onetail(names(tissueExp1),tissueExp1,random.matrix)
    fc_1 <- as.numeric(tissueExp1[names(boot)])
    fc_2 <- as.numeric(apply(random.matrix,1,mean)[names(boot)])
    boot2 <- data.frame(count.sample = as.numeric(tissueExp1[names(boot)]),
                        count.random = apply(random.matrix,1,mean)[names(boot)],
                        FC = as.numeric( fc_1 / fc_2),
                        pvalue=as.numeric(boot)
                        )
    rownames(boot2) <- names(boot)
    boot2 <- (boot2[order(boot2$pvalue),])
    boot2$padj <- stats::p.adjust(boot2$pvalue)
    boot2$abrev <- rownames(boot2)
    dc_sa <- dev_colmeta$structure_acronym
    ac_sa <- ad_colmeta$structure_acronym
    a <- dev_colmeta[match(rownames(boot2), dc_sa),"structure_name"]
    b <- ad_colmeta[match(rownames(boot2), ac_sa),"structure_name"]
    if(sum (is.na(a)) > sum(is.na(b))){
        boot2$structure <- b
    }else {
        boot2$structure <- a}
    return(boot2)
}
