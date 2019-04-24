#' displays a summary of an input dataset
#'
#' Intended for development and debugging
#'
#' @param id prefix for Shiny element IDs
#' @param input Shiny input object
#' @param output Shiny output object
#' @param ui true selects the UI, false the server
#' @param x a reactive function generating the selected input data
#'
#' @return
#' @export

inputDataSummary=function(id,input,output,ui=T,x=NULL){

  ns=NS(id)
  print(ns)

  if (ui){
    verbatimTextOutput(ns("print"))
  } else {

    if (is.null(x)){
      stop("x not given to server call")
    } else {

      cat("server:",id,"\n")

      output[[ns("print")]]=renderPrint({
        print(x())
      })
    }

  }
}
