#' example Shiny app
#'
#' A simple demonstration application to test the elements of the library
#'
#' @param results a results object
#'
#' @xexport
#' @import shiny

appTwo=function(results){

  require(shiny)
  # require(GoViewer)

  cx=reactiveValues(full.data=list(),selections=list())

  data(P2.source)
  cx$full.data=P2.source

  ui <- fluidPage(

    # Application title
    tabsetPanel(
      tabPanel("viewer",
               contrastViewer("contrast",input,output,ui=T)),
      tabPanel("selections",
               listSelectionsPanel("list",input,output,ui=T,cx=cx)),
      tabPanel("Venn diagram",
               vennDiagramPanel("Venn",input,output,ui=T,cx=cx)),
      tabPanel("available data",
               verbatimTextOutput("data.sources"))

    )
  )

  server <- function(input, output,session) {


    # dataSource=reactive({
    #   data("P2.source")
    #   P2.source
    # })


    output[["data.sources"]]=renderPrint({
      cx$full.data
    })

    contrastViewer("contrast",input,output,session,ui=F,cx)
    listSelectionsPanel("list",input,output,ui=F,cx)
    vennDiagramPanel("Venn",input,output,ui=F,cx)
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}


