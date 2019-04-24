#' top level function call to run the app
#'
#' @return
#' @export
#'
#' @examples
#' gv() runs the package

gv=function(){

  library(shiny)

  cx=reactiveValues(full.data=list(),selections=list())

  # data initialisation

  data(P2.source)

  # can't read cx$full.data here - reactive objects problems
  cx$full.data=P2.source
  print(cx)

  # don't want to include this data at this point
  # cx$selections=lapply(P2.source,goViewerSourceToInput)




  ui <- fluidPage(

    tabsetPanel(tabPanel("1 SELECT",
                         tabsetPanel(
                           tabPanel("viewer",
                                    contrastViewer("contrast",input,output,ui=T)),
                           tabPanel("Venn diagram",
                                    vennDiagramPanel("Venn",input,output,ui=T)),
                           tabPanel("data",
                                    availableDataSummary("data",input,output,ui=T))
                         )),
                tabPanel("2 ANALYSE",
                         tabPanel("GoViewer",
                                  goViewer("go",input,output,ui=T,cx=cx)))
    )






  )

  server <- function(input, output, session) {


    # selection
    availableDataSummary("data",input,output,ui=F,cx=cx)
    contrastViewer("contrast",input,output,session,ui=F,cx=cx)
    vennDiagramPanel("Venn",input,output,ui=F,cx=cx)

    # GO analysis
    goViewer("go",input,output,session,ui=F,cx=cx)

  }

  shinyApp(ui, server)

}
