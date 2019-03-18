#' allows user to select colour legend mappings
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, FALSE not currently implemented
#'
#' @return  standard Shiny value
#' @export

colourLegendPanel=function(id,input,output,ui=T){

  ns=NS(id)
  mappings = tensorDataOptions("colour.legend.mappings")

  if (ui){
    # UI mode

    tabPanel("legend",
             selectInput(ns("preset"),
                         label = "preset suggestions",
                         choices = names(mappings),
                         selected = "none"),
             textAreaInput(inputId=ns("selection"),
                           label="mapping",
                           value="",
                           cols="100%",
                           rows=6),
             verbatimTextOutput(ns("preset.values")),
             tableOutput(ns("available-lines"))

    )

  } else {
    # server mode
    stop("server mode not currently implemented")
  }


}
