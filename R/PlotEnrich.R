#' PlotEnrich
#'
#'  Plot the significance results returned from testEnrich
#'
#' @param boot results from the testEnrich function
#' @param topcut upper cutoff (fold-change) for plotting region names
#' @param botcut bottom cutoff (fold-change) for plotting region names
#' @return Spatial enrichment plot
#' @examples
#' #brainImageR:::loadworkspace()
#' ##First put together a gene list, or load in the default vth dataset
#' data(vth)
#' ##Calculate the spatial enrichment
#' #composite <- SpatialEnrichment(vth, 20, "developing")
#' #tissueExp1 <- composite@tissueExp1
#' #random.matrix <- composite@random.matrix
#' ##Calculate the significance estimates
#' #boot <- testEnrich(composite)
#' #PlotEnrich(boot, topcut = 6, botcut = 0.1)
#'
#'@export

PlotEnrich <- function(boot, topcut = 2, botcut = 2){
    significant <- boot$padj < 0.05
    ggplot2::ggplot(boot, ggplot2::aes_string("count.random", "count.sample"))+
    ggplot2::geom_point(alpha = 0.5)+
    ggplot2::geom_point(data = boot,
                        ggplot2::aes_string("count.random",
                                            "count.sample",
                                            colour = "significant"
                                            ),alpha = 0.3)+
    ggplot2::scale_colour_manual(values = c("grey","blue"))+
    ggplot2::theme_bw(base_size = 12)+
    # ggrepel::geom_text_repel(
    # data = subset(boot, abs(FC) > topcut),
    # ggplot2::aes_string(label = "abrev"),
    # size = 4,
    # box.padding = 0.1,
    # point.padding = 0.4
    # )+
    # ggrepel::geom_text_repel(
    # data = subset(boot, abs(FC) < botcut),
    # ggplot2::aes_string(label = "abrev"),
    # size = 4,
    # box.padding = 0.1,
    # point.padding = 0.3
    # )+
    ggplot2::xlab("gene count, random")+
    ggplot2::ylab("gene count, query")
}
