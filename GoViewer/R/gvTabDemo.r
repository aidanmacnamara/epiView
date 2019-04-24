#' demo program for single tab installation
#'
#' A simple code to illustrate the use of gvTab(). Three short sections of
#' code are required: A, B and C. See code listing for details.
#'
#' @return
#'
#' @import shiny
#' @export
#'
#' @examples
#' gvTabDemo()
gvTabDemo=function(){


  # A: set up reactive list and model data
  require(GoViewer)
  require(shiny)

  cx=reactiveValues(full.data=list(),selections=list())
  data(P2.source)
  cx$full.data=P2.source

  ui = fluidPage(
    tabsetPanel(
      # B: call gvTab for Shiny UI
      tabPanel("GoViewer",
               gvTab("gv",input,ouput,ui=T,cx=cx))
    ))

    server = function(input,output,session){
      # C: call gcTab for Shiny server response
      gvTab("gv",input,output,session,ui=F,cx=cx)
    }

    shinyApp(ui,server)

}

#gvTabDemo()
