#' @title Plot your temporal predictions
#' @description
#'  \code{PlotPred} Plots the temporal predictions.
#'
#' @param time Object returned from predict_time
#' @return prediction plot
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' ##Load in data
#' data(dat)
#' ##predict time
#' #time <- predict_time(dat)
#' ##plot the predictions
#' #PlotPred(time)
#' @importFrom ggplot2 aes_string geom_point ggplot scale_colour_manual
#' @importFrom ggplot2 xlab ylab theme_bw geom_smooth geom_hline labs
#' @importFrom ggplot2 geom_bar theme element_text
#' @export

PlotPred <- function(time){
    alldev_scale <- .cache[["EH1451"]]
    alldev_colMeta <- .cache[["EH1450"]]

    Model <- time@model[[1]]
    minage <- ifelse(length(time@minage)>0, time@minage, 8)
    maxage <- ifelse(length(time@maxage)>0, time@maxage, 2120)
    acpcw <- alldev_colMeta$pcw
    alldev_scale2 <- alldev_scale[,acpcw >= minage & acpcw <= maxage]
    alldev_colMeta2 <- alldev_colMeta[acpcw >= minage & acpcw <= maxage,]
    pt <- exp(stats::predict(object = Model, newdata = t(alldev_scale2)))
    t1 <- data.frame(pred_age = time@pred_age[,1],
                    actual_age = time@pred_age[,1],
                    group= "sample",
                    row.names = rownames(time@pred_age))
    newpcw <- alldev_colMeta2$pcw
    t2 <- data.frame(pred_age = pt, actual_age = newpcw, group= "aba")
    df <- rbind(t1,t2)
    YINT1 <- min(df[df$group == "sample", "pred_age"])
    YINT2 <- max(df[df$group == "sample", "pred_age"])
    p1 <- ggplot(df[df$group == "aba",],
                            aes_string(x =  "actual_age",
                                                y = "pred_age",
                                                colour = "group"))+
        geom_point(size = 1, alpha = 1)+
        scale_colour_manual(values =c("blue","grey","red"))+
        xlab("Actual age, pcw")+
        ylab("predicted age, pcw")+
        theme_bw(base_size = 10)+
        geom_smooth(method = "lm", colour = "grey")+
        geom_hline(yintercept = YINT1,
                            colour = "grey")+
        geom_hline(yintercept = YINT2,
                            colour = "grey")+
        labs(title = "Reference Data")

    df$sample <- rownames(df)
    p2 <- ggplot(df[df$group != "aba",],
                            aes_string(x = "sample","pred_age" ))+
                            geom_bar(stat = "identity")+
                            labs(title = "Query Data")+
                            ylab("predicted age, pcw")+
                            theme_bw(base_size = 10)+
                            theme(axis.text.x = element_text(
                            angle = 90,
                            hjust = 1))

    #################
    gridExtra::grid.arrange(p1, p2, nrow = 1)

}
