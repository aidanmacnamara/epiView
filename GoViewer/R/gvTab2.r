#' Title
#'
#' @param id
#' @param input
#' @param output
#' @param session
#' @param ui
#' @param cx
#'
#' @return
#' @export

gvTab2=function(id,input=NULL,output=NULL,session=NULL,ui=T,cx=NULL,helpLink="https://www.bbc.co.uk/weather/"){


  require(shiny)

  if (ui){

    tabsetPanel(
      tabPanel("data sources",
               selectBundles("bundles",ui=T)),
      tabPanel("comparisons",
               contrastViewer("contrast",input,output,ui=T)),
      tabPanel("MVA",
               pcaPanel("MVA",input,output,ui=T)),
      tabPanel("organiser",
               availableDataSummary("data",input,output,ui=T)),
      tabPanel("Venn diagram",
               vennDiagramPanel("Venn",input,output,ui=T)),
      tabPanel("GO enrichment",
               mainGoAnalysisPanel("analysis",input,output,ui=T)),
      tabPanel("help",
               helpPage(topic="v1",links=helpLink)),
      tabPanel("development panel",
               devPanel("dev",input,output,ui=T))
    )

  } else {

    if (is.null(cx)){
      stop("argument cx expected for server (ui=F) call")
    }

    # selection
    selectBundles("bundles",input,output,ui=F,cx=cx)
    availableDataSummary("data",input,output,session,ui=F,cx=cx)
    contrastViewer("contrast",input,output,session,ui=F,cx=cx)
    vennDiagramPanel("Venn",input,output,ui=F,cx=cx)

    # multivariate analyses
    pcaPanel("MVA",input,output,ui=F,cx=cx)

    # GO analysis
    mainGoAnalysisPanel("analysis",input,output,session,ui=F,cx=cx)

    # development
    devPanel("dev",input,output,session,ui=F,cx=cx)
  }

}
