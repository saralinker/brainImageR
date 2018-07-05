#' @title loadworkspace
#' @description loads data into .cache. BrainImageR uses reference maps
#' of the human brain, as well as reference gene expression data, and
#' gene sets to compare the user's data to the Allen Brain Atlas.
#' These datasets are quite large and are therefore stored within an
#' ExpressionHub data package. loadworkspace makes this data available to
#' the brainImageR functions.
#'
#'
#' @examples
#' #brainImageR:::loadworkspace()
#' @export

.cache <- new.env(parent=emptyenv())
loadworkspace <- function(libname, pkgname) {
    hub <- ExperimentHub::ExperimentHub()
    suppressWarnings(.cache[["EH1434"]] <- hub[["EH1434"]])
    suppressWarnings(.cache[["EH1435"]] <- hub[["EH1435"]])
    suppressWarnings(.cache[["EH1436"]] <- hub[["EH1436"]])
    suppressWarnings(.cache[["EH1437"]] <- hub[["EH1437"]])
    suppressWarnings(.cache[["EH1438"]] <- hub[["EH1438"]])
    suppressWarnings(.cache[["EH1439"]] <- hub[["EH1439"]])
    suppressWarnings(.cache[["EH1440"]] <- hub[["EH1440"]])
    suppressWarnings(.cache[["EH1441"]] <- hub[["EH1441"]])
    suppressWarnings(.cache[["EH1442"]] <- hub[["EH1442"]])
    suppressWarnings(.cache[["EH1443"]] <- hub[["EH1443"]])
    suppressWarnings(.cache[["EH1444"]] <- hub[["EH1444"]])
    suppressWarnings(.cache[["EH1445"]] <- hub[["EH1445"]])
    suppressWarnings(.cache[["EH1446"]] <- hub[["EH1446"]])
    suppressWarnings(.cache[["EH1447"]] <- hub[["EH1447"]])
    suppressWarnings(.cache[["EH1448"]] <- hub[["EH1448"]])
    suppressWarnings(.cache[["EH1449"]] <- hub[["EH1449"]])
    suppressWarnings(.cache[["EH1450"]] <- hub[["EH1450"]])
    suppressWarnings(.cache[["EH1451"]] <- hub[["EH1451"]])
}
