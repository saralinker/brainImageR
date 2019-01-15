#' @title Color and Plot the SGSE image
#' @description
#' \code{PlotBrain} Plots CreateBrain. The gene set enrichment observe
#' within the microdissected tissues (results of testEnrich) are
#' combined here to show gene set enrichment across broad brain regions.
#' Enriched regions are colored in red, and regions depleted for the query
#' gene list are colored in blue.
#'
#' @details
#' PlotBrain plots your spatial gene set enrichment image.
#'
#' @param composite Comp object returned from CreateBrain.
#' @param legend Boolean whether the legend should be included. Default = TRUE
#' @return plots the SGSE brain image
#'
#' @examples
#'
#' ##First put together a gene list, or load in the default vth dataset
#' #brainImageR:::loadworkspace()
#' data(vth)
#' ##Calculate the spatial enrichment.
#' #composite <- SpatialEnrichment(vth, reps = 20, refset = "developing")
#' ##Calculate the significance of the gene set enrichment
#' #boot <- testEnrich(composite)
#' ##Color the brain section of interest
#' #composite <- CreateBrain(composite, boot, slice = 5, pcut = 0.05)
#' ##Plot the brain
#' #PlotBrain(composite)
#' @importFrom grid grid.raster grid.rect gpar grid.text grid.newpage
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices as.raster
#' @export

PlotBrain <- function(composite, legend = TRUE){
    Breaks <- 6
    map <- composite@composite
    map <- as.matrix(map)
    map[!is.finite(map)] <- 0

    redGradient <- brewer.pal(n=round(Breaks),name="Reds")
    blueGradient <- rev(brewer.pal(n=round(Breaks),name="Blues"))

    map[map > 2 & map < max(map)] <- 2

    cutNormal_b1.low <- c(0.00020, 0.20016, 0.40012, 0.60008, 0.80004)
    cutNormal_b1.hi <- c(0.20016, 0.40012, 0.60008, 0.80004,1)

    cutNormal_a1.low <- c(1.0, 1.2, 1.4, 1.6, 1.8)
    cutNormal_a1.hi <- c(1.2, 1.4, 1.6, 1.8 ,2.0)

    normalized.color <- map

    for(i in brainrange(1, length(cutNormal_b1.low))){
        placement <- map >= cutNormal_b1.low[i] & map <= cutNormal_b1.hi[i]
        normalized.color[placement] <- blueGradient[i]
    }
    for(i in brainrange(1, length(cutNormal_a1.low))){
        placement <- map >= cutNormal_a1.low[i] & map <= cutNormal_a1.hi[i]
        normalized.color[placement] <- redGradient[i]
    }

    #changed white from 1 to 0.9 because some panels' white is
    normalized.color[map == 1] <- "white"
    normalized.color[map == 0.0001] <- "#dddcdc"
    normalized.color[map == max(map)] <- "black"



    grid.newpage()

    #add legend
    #plot(1,1)
    #grid.newpage()
    if(composite@refset == "developing"){
        grid.raster(as.raster(normalized.color),
                        interpolate=TRUE)
    }else{
        grid.raster(as.raster(normalized.color),
                            interpolate=TRUE,
                            width = 0.8)
    }

    all_colors <- c(blueGradient[-c(1)],
                    "white",
                    redGradient[-c(length(redGradient))])

    all_segments <- c("0.0",
                        round(cutNormal_b1.low[-c(1)],2),
                        "1.0",
                        round(cutNormal_a1.low[-c(1)],2),
                        "2.0")

    if(legend == TRUE & composite@refset == "developing"){
        x <- 0.8
        y <- 0.6
        y1 <- y
        shift <- 0.035

        for (i in all_colors){
            grid.rect(x = x,
                            y = y1,
                            width = shift,
                            height = shift,
                            gp=gpar(col = "white", fill = i))
            y1 <- y1 + shift
        }

        y1 <- y

        for(i in all_segments){
            grid.text(label = i,
                            x = x,
                            y = y1,
                            gp = gpar(fontsize = 9),
                            hjust = -1.55)
            y1 <- y1 + shift
            }
        }else if(legend == TRUE & composite@refset == "adult"){
            x <- 0.90
            x1 <- x
            y <- 0.65
            y1 <- y
            shift <- 0.03
            for (i in all_colors){
                grid.rect(x = x1,
                            y = y1,
                            width = shift,
                            height = shift,
                            gp=gpar(col = "white", fill = i))
                y1 <- y1 + shift
            }

            x1 <- x
            for(i in rev(all_segments)){
                grid.text(label = i,
                            x = x1,
                            y = y1,
                            gp = gpar(fontsize = 9),
                            vjust = 1.7,
                            hjust = -0.5)
                y1 <- y1 - shift
            }
            }

}


