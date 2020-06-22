#' selects profile data of interest for a given gene
#'
#' @param counts the counts component from the surplied profiles dataset
#' @param gene the requested gene of interest. MUltiple genes are allowed
#'
#' @return
#' @export
#'
#' @examples
#' data(profiles)
#' selectPlotData(profiles$counts)

selectPlotData=function(counts,gene="MFSD14A"){
  counts[counts$state !="" & counts$gene %in% gene,]
}



