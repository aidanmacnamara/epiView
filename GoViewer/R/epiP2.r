#'
#'
#' a view of the epigenetics data from project two
#'
#' @format a multi layer list
#'
#' The format is complicated and see examples for a walk-through. Data
#' is a selection of an object produced by Aidan.
#'
#' @docType data
#' @usage data(epiP2)
#'
#' @examples
#'
#' data(epiP2)
#'
#' # markers available
#' names(epiP2$tmp)
#'
#' # actual data
#' dim(epiP2$tmp$H3K27ac$res)
#' epiP2$tmp$H3K27ac$res[1:3,1:10]
#'
#' # details of individual cell lines
#' head(epiP2$tmp$H3K27ac$annot)
#'
#' # not current used
#' epiP2$mds_type
#'
#' # repeat of label information (out of date - too many entries?)
#' epiP2$labels
"epiP2"

