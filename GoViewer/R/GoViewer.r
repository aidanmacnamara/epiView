#' top level GO viewer wiget
#'
#' Used as part of a demo shiny app
#'
#' @param id prefix for all shiny names
#' @param input shiney input object
#' @param output  shiny output object
#' @param session shiny ooutput session
#' @param ui true selects UI code, false server
#' @param x.all a contains a list of goViewerInput objects like \link{P2} (assumed reactive)
#' @export

goViewer=function(id,input,output,session=NULL,ui=T,cx=NULL){
  ns=NS(id)

  if (ui){

    tabsetPanel(
     tabPanel("select contrast",
              selectInputDataObject(ns("select.input"),input,output,ui=T)),
      # tabPanel("data",
      #          inputDataSummary(ns("inputs"),input,output,ui=T)),
      tabPanel("analysis",
              mainGoAnalysisPanel(ns("analysis"),input,output,ui=T)),
     tabPanel("test")
    )

  } else {


    cat("go server\n")


    # this now returns direct inputs and selections
    x.all=reactive({
      renameListNames(c(cx$selections,
                        lapply(cx$contrast.data,
                               goViewerSourceToInput)))
    })

    # the base data frane
    x=reactive({
      x.all()[[input[[paste0(ns("select.input"),"-choice")]]]]
    })

    selectInputDataObject(ns("select.input"),input,output,ui=F,x.all)
    inputDataSummary(ns("inputs"),input,output,ui=F,x)
    mainGoAnalysisPanel(ns("analysis"),input,output,session,ui=F,x=x,cx=cx)

  }
}
