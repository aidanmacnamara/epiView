#' main CP analysis panel
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, FALSE not currently implemented
#'
#' @return  standard Shiny value
#' @export

tensorCpAnalysisPanel=function(id,input,output,ui=T){

  ns=NS(id)
  print(ns("hi"))

  if (ui){
    # UI mode
   tabPanel("CP analysis",

             fluidPage(
               fluidRow(
                 column(8,
                        plotOutput(ns("main.plot"))),
                 column(4,tensorCpAnalysisInspectorPanel(ns("detail"),
                                                         input,
                                                         output,
                                                         ui=T)

                 )
               ), # top row
               fluidRow(
                 column(3,sliderInput(ns("k"),
                                      "number of factors",
                                      min = 1, max = 20,
                                      value = 4,
                                      step=1)),
                 column(2,sliderInput(ns("label.size"),
                                      "label size",
                                      min = 1, max = 6,
                                      value = 2.5,
                                      step=0.1)),
                 column(1,uiOutput(ns("x.axis.choice"))),
                 column(1,uiOutput(ns("y.axis.choice")))
               ) # bottom row
             )
    )

  } else {
    # server mode
    stop("server mode not currently implemented")
  }


}
