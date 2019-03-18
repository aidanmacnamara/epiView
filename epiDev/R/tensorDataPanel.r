#' implements data panel for tensor functionality
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, FALSE not currently implemented
#'
#' @return  standard Shiny value
#' @export

tensorDataPanel=function(id,input,output,ui=T){

  ns=NS(id)

  if (ui){
    # UI mode

    tabPanel("data",
             tabsetPanel(tabPanel("marks",
                                  verbatimTextOutput(ns("upload-status"),
                                                     placeholder=T),
                                  uiOutput(ns("mark-selection")),
                                  verbatimTextOutput(ns("available-data")),
                                  tableOutput(ns("available-lines"))),

             tabPanel("full summary",
                      HTML("data counts before filtering"),
                      tableOutput(ns("mark-project-summary"))
             )
             ))
  } else {
    # server mode
    stop("server mode not currently implemented")
  }
}
