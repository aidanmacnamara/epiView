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
gvTabDemo2=function(){


  # A: set up reactive list and model data
  require(GoViewer)
  require(shiny)

  cx=reactiveValues(bundles=list(),
                    contrast.data=list(),
                    epi.data=list(),
                    selections=list(),
                    legend=list(),
                    controls=list(),
                    results=list(),
                    status=list(),
                    colour.legends=globalOptions("colour.legend"),# provisional values
                    active.colour.legend=list(),
                    pca.selected.points=list()
                    )


  data(P2.source)

  # adding bundles interface

  bundles=list(P2=goViewerInputSourceBundle(P2.source,
                                            "project two"),
                  P2a=goViewerInputSourceBundle(P2.source[1:3],
                                            "test: P2 1-3"),
                  P2b=goViewerInputSourceBundle(P2.source[4:5],
                                            "test: P2 4-5")
                  )

  # set the contrast data default to the first entry
  cx$bundles=bundles
  cx$contrast.data=flattenBundles(bundles[1])

  # currently, we are only loading P2 epi data
  data("epiP2")
  cx$epi.data=epiP2$tmp

  ui = fluidPage(
    tabsetPanel(
      # B: call gvTab for Shiny UI
      tabPanel("GoViewer",
               gvTab2("gv",
                      input,
                      ouput,
                      ui=T,
                      cx=cx,
                      helpLink="https://www.bbc.co.uk/weather/"))
    ))

  server = function(input,output,session){
    # C: call gcTab for Shiny server response
    gvTab2("gv",input,output,session,ui=F,cx=cx)
  }

  shinyApp(ui,server)

}

#gvTabDemo()
