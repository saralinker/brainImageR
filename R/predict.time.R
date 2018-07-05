#' @title Predict developmental time from gene expression data
#'
#' @description
#' Predict human developmental time from expression dataset
#'
#' @param dat Normalized expression matrix
#' @param minage min pcw of the reference set. default = 8
#' @param maxage max pcw of the reference set. default = 2120
#' @param genelist Optional: restrict analysis to gene list
#' @param tissue Optional: restrict analysis to tissue (available)
#' @param minrsq (range 0-1) model leniency. default = 0.5.
#' @return spatiotemporal predictions.
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##Load in the data
#' data(dat)
#' ##predict time
#' #time <- predict_time(dat)
#'
#' @export

predict_time <- function(dat = NULL, genelist = NULL, minage = 8,
                            maxage = 2120, tissue = NULL, minrsq = 0.6){
    aba_fun <- function(x){stats::cor(x = x, y = abapcw$pcw)}

    alldev_colMeta <- .cache[["EH1450"]]
    alldev_scale <- .cache[["EH1451"]]
    #1) Check data
    if(is.null(dat)){
        stop("data is NULL.")
    }

    ##################################
    ### SECTION 1 A genelist is provided
    ##################################
    #3) Check genelist
    # genelist is provided by the user
    if(!is.null(genelist)){#Provided by the user
        genelist  <- unique(genelist)
        }else {#no gene list given
            v <- unlist(apply(dat, 1, stats::var))
            v_min <- stats::quantile(v,probs = 0.33)
            genelist <- rownames(dat[rowSums(dat) > 2 & v > v_min,])
        }
    ###########################
    #4A) Calculate model if not using a precomputed model
    ###########################
    if(!is.null(genelist) ){
        ######
        # Remove genes that don't cooperate well with randomforest
        if(length(grep(pattern = "-", genelist, fixed = TRUE)) > 0){
            a <- grep(pattern = "-", genelist, fixed = TRUE)
            genelist <-  genelist[-a]
    }
    if(length(grep(pattern = ".", genelist, fixed = TRUE)) > 0){
        a <- grep(pattern = ".", genelist, fixed = TRUE)
        genelist <-  genelist[-a]
    }
    ######
    setgenes <- genelist
    ######
    if(is.null(minage)){minage <- min(alldev_colMeta$pcw)}
    if(is.null(maxage)){maxage <- max(alldev_colMeta$pcw)}
    if(maxage > 2120){
        maxage <- 2120
        warning("Max age surpassed. Setting to 2120 pcw")
    }
    acpcw <- alldev_colMeta$pcw
    aba <- alldev_scale[,acpcw < maxage & acpcw > minage  ]
    abapcw <- alldev_colMeta[acpcw < maxage & acpcw > minage,]
    if(!is.null(tissue)){
        aba <- aba[,abapcw$structure_acronym == tissue  ]
        abapcw <- abapcw[abapcw$structure_acronym == tissue,]
    }

    nm <- table(c(genelist,rownames(aba)))
    nm <- names(nm[nm==2])
    aba <- aba[nm,]
    m <- apply(X = aba, MARGIN = 1,FUN = mean)
    mq <- stats::quantile(stats::na.exclude(m))[2]
    s <- apply(X = aba, MARGIN = 1,FUN = stats::sd)
    sq <- stats::quantile(stats::na.exclude(s))[2]
    pickme <- m[m > mq & s > sq]
    aba <- aba[names(stats::na.exclude(pickme)),]

    aba_pcw_cor <- apply(X = aba, MARGIN = 1, FUN = aba_fun)
    loadgenes <- names(stats::na.exclude(aba_pcw_cor[abs(aba_pcw_cor) > 0.5 ]))

    if(length(loadgenes) < 1){
        warning("No association with dev time. Try a new gene list.")
        stop()
    }
    #Initialize
    qp <- 0.1
    tmp <- as.data.frame(t(rbind(aba[loadgenes,],abapcw$pcw)))
    colnames(tmp) <- c(loadgenes,"pcw")

    if(length(grep("-",colnames(tmp))) > 0){
        tmp <- tmp[,-c(grep("-",colnames(tmp)))]
    }
    if(sum(is.na(tmp)) > 0){
        tmp <- tmp[,-c(which(is.na(tmp[1,])))]
    }
    colnames(tmp)[length(tmp)] <- "pcw"
    rf <- randomForest::randomForest(log(pcw) ~ ., tmp, ntree= 500)
    gini <- as.data.frame(rf$importance)
    colnames(gini) <- c("MeanDecreaseGini")
    cutoff <- stats::quantile((gini$MeanDecreaseGini),probs=qp)
    tmpgenes <- rownames(gini)[gini$MeanDecreaseGini >= cutoff]
    #Round 2
    qp <- 0.5 #was 0.5
    tmp <- as.data.frame(t(rbind(aba[tmpgenes,],abapcw$pcw)))
    colnames(tmp) <- c(tmpgenes,"pcw")
    rf <- randomForest::randomForest(log(pcw) ~ ., tmp, ntree= 150)
    gini2 <- as.data.frame(rf$importance)
    colnames(gini2) <- c("MeanDecreaseGini")
    cutoff <- stats::quantile((gini2$MeanDecreaseGini),probs=qp)
    setgenes <- rownames(gini2)[gini2$MeanDecreaseGini >= cutoff]
    if(length(setgenes) < 2 | mean(stats::na.exclude(rf$rsq)) < minrsq){
        warning("No association with dev time. Try a new gene list.")
        stop()
    }

    }else{
        warning("genelist empty")
        stop()

    }
    ###########################
    #5) Use the model to predict time
    ###########################

    #6) Check if the dataset contains the genes in the model.
    genes <- table(c(rownames(dat), setgenes, setgenes))
    if(sum(genes == 3) == 0){
        warning("No genes in reference. Trying the transpose of the dataset...")
        dat <- t(dat)
        genes <- table(c(rownames(dat), setgenes, setgenes))
        if(sum(genes == 3) == 0){
            warning("Use official human gene symbol. Ex: REST, SOX11 ...")
            stop()
        }
    }
    missing <- names(genes[genes == 2])
    genes <- names(genes[genes == 3])
    #7) If there is not 100% overlap
    #   Or if the user recalculated the setgenes above
    #   ...calculate a new Model
    #if(length(genes) != length(setgenes)){
    tmp_genes <- stats::na.exclude(aba[stats::na.exclude(genes),])
    tmp <- as.data.frame(t(rbind(tmp_genes,abapcw$pcw)))
    colnames(tmp) <- c(rownames(tmp_genes),"pcw")
    Model <- randomForest::randomForest(log(pcw) ~., tmp )
    #}
    dat.scale <- matrix(data=0,nrow=nrow(dat),ncol=ncol(dat))
    dat <- round(dat, digits = 0)
    for(i in brainrange(1,ncol(dat))){
        dat.scale[,i] <- scale(dat[,i])[,1]
    }
    dimnames(dat.scale) <- dimnames(dat)
    dat.scale <- as.data.frame(dat.scale)
    NewData <- as.data.frame(t(dat.scale))
    pred_age <- exp(stats::predict(object=Model,newdata=NewData))
    pred <- methods::new(Class="Pred", pred_age = data.frame(pred_age),
                            model = list(Model),
                            minage = as.numeric(minage),
                            maxage = as.numeric(maxage),
                            tissue = as.character(tissue)
    )
    return(pred)
}
