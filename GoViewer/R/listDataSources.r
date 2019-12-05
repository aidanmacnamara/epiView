#' lists available data
#'
#' @param id widget id
#' @param input shiny input list
#' @param output shiney output list
#' @param ui true for ui, false for server
#' @param cx a reactive list containing data
#'
#' @export

listDataSources=function(id,input,output,ui=T,cx=NULL){

  ns=NS(id)

  if (ui){
    list(HTML("<br>available datasets"),
         tableOutput(ns("listing")))
  } else {
    output[[ns("listing")]]=renderTable({
      sapply(epiP2$tmp,
             function(w){c(genes=ncol(w$res),lines=nrow(w$res))})},
      rownames=T)
  }
}
