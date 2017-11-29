#' lists the available genes for all the presented marks
#'
#' @param x a data list in Aidan's global_choice()$tmp format.
#'   may have been prefiltered for marks and cell lines
#'
#' @return a vector of available genes
#' @export

availableGenes=function(x){
  listIntersect(availableGenesByMarks(x))
}
