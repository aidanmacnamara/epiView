#' implements data transformation panel for tensor functionality
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, FALSE not currently implemented
#'
#' @return  standard Shiny value
#' @export

tensorDataTransPanel=function(id,input,output,ui=T){

  ns=NS(id)

  trans.selection=c("none","sqrt","log10","softLog10")
  names(trans.selection)=c("none","square root","log10(x+1)","softLog10")

  # remove the soft transformation initially
  trans.selection=trans.selection[-4]

  if (ui){
    # UI mode

    tabPanel("trans",
             selectInput(ns("selection"),
                         label = "transformation",
                         choices = trans.selection,
                         selected = "none"),
             plotOutput(ns("density")))

  } else {
    # server mode
    stop("server mode not currently implemented")
  }


}
