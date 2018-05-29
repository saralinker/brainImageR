#' @title Color in the brain images based on enrichment values
#'
#' @description
#' \code{reColor} quantifies the presence of a gene list within each tissue
#'
#'
#' @param i tissue region from within the specified rostral-caudal section
#' @param slice current slice
#' @param tissueExp tissueExp1 from SpatialEnrichment.
#' @param dim Original dimensions of the image
#' @param Abrev list of tissue regions
#' @param Files list of tiff images
#' @param refset  reference map. developing (default) or adult
#' @return returns genes counts for each tissue
#' @examples
#'
#' #brainImageR:::loadworkspace()
#' ##First load a gene set
#' data(vth)
#' ##calculate spatial enrichment
#' #composite <- SpatialEnrichment(vth,20,"developing")
#' #boot <- Boot(composite)
#' #subboot <- c(boot[boot$pvalue < 0.05 & is.finite(boot$FC), "FC"])
#' #names(subboot) <- rownames(boot[boot$pvalue < 0.05 & is.finite(boot$FC), ])
#' #tissueExp <- subboot
#' ##Select the slice of interest
#' #slice <- 4
#' #Files <- .cache[["EH1434"]][[slice]]
#' #dim <- .cache[["EH1436"]][[slice]]
#' ##Select the region of interest
#' #Abrev <- .cache[["EH1438"]][[4]]
#' #abrev <- "VZ"
#' #tmp <- reColor(abrev, slice, tissueExp, dim, Abrev, Files)
#'
#'
#' @export


reColor <- function(i,slice,tissueExp,dim,Abrev,Files,refset = "developing"){
    refset <- tolower(refset)
    if (refset == "developing"){
        conversion <- dev_conversion
        slices <- .cache[["EH1443"]]
    }else if (refset == "adult"){
        conversion <- .cache[["EH1442"]]
        slices <- .cache[["EH1444"]]
    }else{
        stop(paste(c("Please choose refset = developing or adult.")))
    }


    region <- matrix(data=unlist(Files[Abrev==i]),nrow=dim[1],ncol=dim[2])
    chosen_tissue <- as.character(Abrev[Abrev==i])
    conv_nm <- as.character(conversion[,paste("panel",slices[slice],sep="")])
    conv_short <- conversion[conv_nm == chosen_tissue,"acronym"]
    converted <- as.character(stats::na.exclude(conv_short))
    converted_tissexp <- tissueExp[converted]

    #make anatomy sections with no supporting expression information black
    if(length(converted_tissexp) ==0 ){
        newValue <- 0
    }else{
        #sig sections with transcription info get filled in
        converted_tissexp <- converted_tissexp[!is.na(converted_tissexp)]
        if(length(converted_tissexp) > 0 ){ #color tissue with no info 0.0001
            newValue <- mean(as.numeric(unlist(converted_tissexp)))
        }else{
            newValue <- 0.0001
        }
        }
    #This was changed from 1 to 0.9 because some whites aren't exactly 1
    region[region > 0.9] <- 0
    region[region != 0] <- newValue
    return(as.vector(region))
}
