#' main CP analysis inspector panel
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, FALSE not currently implemented
#'
#' @return  standard Shiny value
#' @export

tensorCpAnalysisInspectorPanel=function(id,input,output,ui=T){

  ns=NS(id)

  if (ui){
    # UI mode

    wellPanel(
      htmlOutput(ns("title")),
      selectInput(ns("direction"),
                  label = "",
                  choices = c("horizontal","vertical")),
      plotOutput(ns("mark.plot"),
                 height=200),
      tabsetPanel(
        tabPanel("histogram",
                 tabPanel("histogram",
                          plotOutput(ns("gene.histogram"),
                                     height=200))),
        tabPanel("genes",
                 HTML("migrating to this version")),
        tabPanel("GO enrichment",
                 HTML("migrating to this version"))
      )
    )

  } else {
    # server mode
    stop("server mode not currently implemented")
  }


}
