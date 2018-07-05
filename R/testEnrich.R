#' @title Calculate significance of gene set enrichment
#'
#' @description
#' \code{testEnrich} test the enrichment of the observed enrichment
#'
#'
#' @param composite Comp object returned from SpatialEnrichment.
#' @param method character either "bootstrap" or "fisher"
#' @return spatiotemporal prediction
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##First put together a gene list, or load in the default vth dataset
#' data(vth)
#' ##Calculate the spatial enrichment.
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' ##Calculate the significance estimates
#' #boot <- testEnrich(composite)
#' #boot <- boot[order(boot$FC, decreasing=TRUE),]
#' #head(boot)
#'
#' @export

testEnrich <- function(composite, method = c("fisher", "bootstrap")){
    method = match.arg(method)
    dev_colmeta <- .cache[["EH1447"]]
    ad_colmeta <- .cache[["EH1448"]]
    genes <- composite@genes
    tissueExp1 <- composite@tissueExp1
    random.matrix <- composite@random.matrix



    if (method == "bootstrap"){
        boot <- PValue.onetail(names(tissueExp1),tissueExp1,random.matrix)
        fc_1 <- as.numeric(tissueExp1[names(boot)])
        fc_2 <- as.numeric(apply(random.matrix,1,mean)[names(boot)])
        boot2 <- data.frame(count.sample = fc_1,
                            count.random = fc_2,
                            FC = as.numeric( fc_1 / fc_2),
                            pvalue=as.numeric(boot)
                            )
        rownames(boot2) <- names(boot)
        boot2$abrev <- rownames(boot2)
    }else if (method == "fisher"){
        l <- length(tissueExp1)
        boot2 <- data.frame(matrix(data = NA,nrow = l,ncol = 6))
        rownames(boot2) <- names(tissueExp1)
        colnames(boot2) <- c("count.sample",
                                "count.random",
                                "odds.ratio",
                                "pvalue",
                                "padj",
                                "abrev")
        boot2$count.sample <- as.numeric(tissueExp1[rownames(boot2)])
        x <- as.numeric(apply(random.matrix,1,mean)[rownames(boot2)])
        boot2$count.random <- x
        boot2$abrev <- rownames(boot2)
        allgenes <- as.character(rowmeta$gene_symbol)
        f <- function(i){
            sample_overlap <- as.numeric(tissueExp1[i])
            sample_notoverlap <- length(genes) - sample_overlap
            tissue_genes <- length(GetGenes(genes = allgenes,
                                            composite = composite,
                                            tissue_abrev = i))
            x <- length(allgenes)
            outgroup <- x - sample_overlap - sample_notoverlap - tissue_genes
            df <- data.frame(sample = c(sample_overlap, sample_notoverlap),
                                tissue = c(tissue_genes, outgroup))
            f <- stats::fisher.test(df)
            f$p.value
            return(c(as.vector(f$estimate), as.vector(f$p.value)))
        }
        tmp <- vapply(X = rownames(boot2), FUN = f,
                        FUN.VALUE = c("odds.ratio" = 0, "pvalue" = 0))
        boot2[,"odds.ratio"] <- as.vector(tmp["odds.ratio",])
        boot2[,"pvalue"] <- as.vector(tmp["pvalue",])
        boot2$FC <- boot2$count.sample / boot2$count.random

    }
    boot2 <- (boot2[order(boot2$pvalue),])
    boot2$padj <- stats::p.adjust(boot2$pvalue)
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
