#' list selected contrasts
#'
#' Intended for development, this lists the contrasts selected by the contrast viewer
#'
#' @param id a Shiny UI id
#' @param input Shiny input
#' @param output Shiny output
#' @param ui selects UI code if true, o/w the server
#' @param cx a reactiveVector (c.f. pointer) to communicate data - here read the constrsts - with the remainder of the code
#'
#' @export

listSelectionsPanel=function(id,input,output,ui,cx){

  ns=NS(id)

  if (ui){

    verbatimTextOutput(ns("display"))

  } else {

    output[[ns("display")]]=renderPrint({
      cx$selections
    })

  }

}
