#'  Calculate the presence of your gene set within each brain region
#'
#' Calculates the presence of gene set within each region
#'
#' @param genes query gene set
#' @param background background gene list, default = NULL (uses all ABA genes)
#' @param reps replicates for bootstrap, default = 100
#' @param refset  reference brain map. developing (default) or adult
#' @return "Comp" object
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##First load in a gene set
#' data(vth)
#' ##Then calculate the spatial enrichment
#' #composite <- SpatialEnrichment(vth,20,"developing")
#'@export


SpatialEnrichment <- function(genes,
                                background = NULL,
                                reps = 100,
                                refset = "developing"){
    rowmeta <- .cache[["EH1449"]]
    if(is.null(rowmeta)){
        warning(paste(c("workspace not loaded: ",
                        "Run brainImageR:::loadworkspace()",
                        " with Bioconductor >= v3.8 installed")))
    }
    stopifnot(exprs = !is.null(rowmeta))

    refset <- tolower(refset)
    refset <- tolower(refset)
    gene_match <- match(genes,rowmeta$gene_symbol)
    probes <- rowmeta[gene_match,"probeset_id"]
    probe_match <- match(probes[!is.na(probes)],rowmeta$probeset_id)
    samplesize <- length(unique(rowmeta[probe_match,"gene_symbol"]))
    surviving <- samplesize
    starting <- length(genes)
    probe_match <- match(probes[!is.na(probes)],rowmeta$probeset_id)
    genes <- unique(as.character(rowmeta[probe_match,"gene_symbol"]))

    message(paste(surviving,"of",starting,"genes are present in ref dataset"))
    tissueExp1 <- tissueExp <- TissueSummary(genes, refset)
    message(paste(surviving,"genes are expressed using",reps , "iterations"))
    if(!is.null(background)){
        message("Using user-provided set of background genes")
        starting.b <- length(background)
        probes.b <- rowmeta[match(background,rowmeta$gene_symbol),"probeset_id"]
        probe_match <- match(probes.b[!is.na(probes.b)],rowmeta$probeset_id)
        background <- unique(as.character(rowmeta[probe_match,"gene_symbol"]))
        surviving.b <- length(background)
        message(paste(surviving.b,
                    "of",
                    starting.b,
                    "background genes are in ref dataset"))
        }else{
            message("Using all genes in ABA microarray as background")
            background <- unique(as.character(rowmeta$gene_symbol))
        }
    #random.matrix <- as.data.frame(sapply(X=c(1:reps),
    #                                      FUN=RandomTissueSummary,
    #                                      genes= background,
    #                                      samplesize = surviving,
    #                                      refset = refset))

    template <- c(brainrange(1,length((names(tissueExp1)))))
    names(template) <- (names(tissueExp1))
    random.matrix <- as.data.frame(vapply(X=brainrange(1,reps),
                                            FUN.VALUE = template,
                                            FUN=RandomTissueSummary,
                                            genes= background,
                                            samplesize = surviving,
                                            refset = refset))

    tissueExp <- as.table(apply(X=random.matrix,MARGIN=1,FUN=mean))
    tissueExp2 <-  tissueExp
    tissueExp1b <- vector()
    for(i in unique(names(tissueExp1))){
        tissueExp1b[i] <- mean(tissueExp1[names(tissueExp1) == i])
    }
    tissueExp1 <- tissueExp1b
    tissueExp2b <- vector()
    for(i in unique(names(tissueExp1))){
        tissueExp2b[i] <- mean(tissueExp2[names(tissueExp1) == i])
    }
    tissueExp2 <- tissueExp2b

    rm2 <- matrix(data = NA,
                    nrow = length(unique(names(tissueExp1))),
                    ncol = ncol(random.matrix),
                    dimnames = list(unique(names(tissueExp1)),
                    colnames(random.matrix))
    )
    for(i in unique(names(tissueExp1))){
        rm2[i,] <- apply(random.matrix[names(tissueExp1) == i,], 2, mean)
    }
    random.matrix <- data.frame(rm2)
    comp <- methods::new(Class="Comp",
                            genes = genes,
                            tissueExp1 = tissueExp1,
                            tissueExp2 =  tissueExp2,
                            composite = matrix(NA,1,1),
                            random.matrix = random.matrix,
                            refset = refset
    )
    return(comp)
}

