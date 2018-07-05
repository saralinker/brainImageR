#' PlotEnrich
#'
#' A quick plot to assess the enrichments returned from testEnrich.
#' Gene overlap calculated by random chance is plotted on the
#' x-axis and the gene overlap from the query set on the y-axis. Each
#' dot represents an individual microdissected tissue. Note that
#' the signficance estimate is only dependent on the randomly generated
#' overlaps if the p-values were calculated with the bootstrap procedure.
#'
#' @param boot Comp object returned from the testEnrich function
#' @return Spatial enrichment plot
#'
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
#' #PlotEnrich(boot)
#'
#'@importFrom ggplot2 ggplot aes_string geom_point scale_colour_manual
#' theme_bw xlab ylab
#'
#'@export

PlotEnrich <- function(boot){
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
