#' displays an object or text to the screen
#'
#' @param id shiny ID
#' @param input shiny input object
#' @param output shiny output object
#' @param ui TRUE selects the UI o/w server side
#' @param text reactive list with data and current states
#' @export
#'
#' @examples
displayText=function(id,input=NULL,output=NULL,ui=T,text="none given"){
  ns=NS(id)

  if (ui){
    verbatimTextOutput(ns("display"))
  } else {
    output[[ns("display")]]=renderPrint({text})
  }
}
