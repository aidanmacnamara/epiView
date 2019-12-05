#' example Shiny app
#'
#' A simple demonstration application to test the elements of the library
#'
#' @export
#' @import shiny

appOne=function(){

  cx=reactiveValues(selections=list())

  data(P2)
  cx$selections=P2


  require(shiny)
 # require(GoViewer)

  ui <- fluidPage(

    # Application title
    tabsetPanel(
      tabPanel("GoViewer",
               goViewer("go",input,output,ui=T,cx=cx))
    )
  )

  server <- function(input, output,session) {

    print("server call")


    # # all the input datasets
    # x.all=reactive({
    #   data(P2)
    #   P2
    # })

    goViewer("go",input,output,session,ui=F,cx=cx)

  }

  # Run the application
  shinyApp(ui = ui, server = server)
}


