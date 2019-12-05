#' returns a descriptive text for a GO analysis
#'
#' @param contrast
#' @param GOtype
#' @param GOmethod
#' @param minNodeSize
#' @param noEnriched
#'
#' @return
#' @export
#'
#' @examples
#' goAnalysisDescripton("A vs B",
#'                      "BP",
#'                      "fisher",
#'                      100)
#'
#' goAnalysisDescripton("A vs B",
#'                      "BP",
#'                      c("fisher","KS"),
#'                      100)
#'
#' goAnalysisDescripton("A vs B",
#'                      "BP",
#'                      c("fisher","KS"),
#'                      100,500)

goAnalysisDescripton=function(contrast,
                              GOtype,
                              GOmethod,
                              minNodeSize,
                              ordered,
                              noEnriched=NULL){

  if (!is.null(noEnriched)){
    enriched=sprintf(" and %d enriched genes",noEnriched)
  } else {
    enriched=""
  }

  if (ordered){
    orderText="ordering"
  } else {
    orderText="selection"
  }

  sprintf(paste("enrichment by %s for %s '%s'",
                "with %s; minimum node size of %d%s"
                ),
          GOtype,orderText,contrast,andList(GOmethod),minNodeSize,
          enriched)
}
