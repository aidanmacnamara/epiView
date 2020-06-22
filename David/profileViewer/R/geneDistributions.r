#' Title
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param rv
#'
#' @return
#' @export
#'
#' @examples
geneDistributions=function(id,input=NULL,output=NULL,ui=T,rv=NULL){

  ns=NS(id)

  if (ui){
    list(
      plotOutput(ns("plot")),
      uiOutput(ns("source.widget")),
      checkboxInput(ns("log.scale"), label = "log scale", value = FALSE),
      wellPanel(HTML("plots distributions from the input data sources. Note that if quantile
                     normalisation is selected ('normalised') all cell types are forced
                     to follow the same distribution."))
    )
  } else {

    # lists the active data sources

    dataSourceList=reactive({
      c("normalised",names(rv$raw))
    })

    # active data source picker

    output[[ns("source.widget")]]=renderUI({
      selectInput(ns("source"),"data source",
                  choices = dataSourceList(),
                  selected = 1)
    })


    output[[ns("plot")]]=renderPlot({

      sourceType = input[[ns("source")]]

      if (sourceType == "normalised"){
        plotData=rv$counts
        normalised=T
      } else {
        plotData=rv$raw[[sourceType]]
        normalised=F
      }


      plotGeneDist(plotData,
                   type="summary",
                   log=input[[ns("log.scale")]],
                   guides=rv$guides,
                   guides2=rv$guides2,
                   wrap=!normalised,
                   no.guides=!normalised)
    })
  }

}
