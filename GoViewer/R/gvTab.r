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

gvTab=function(id,input=NULL,output=NULL,session=NULL,ui=T,cx=NULL){


  require(shiny)

  if (ui){
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
                                  goViewer("go",input,output,ui=T))))
  } else {

    if (is.null(cx)){
      stop("argument cx expected for server (ui=F) call")
    }

    # selection
    availableDataSummary("data",input,output,ui=F,cx=cx)
    contrastViewer("contrast",input,output,session,ui=F,cx=cx)
    vennDiagramPanel("Venn",input,output,ui=F,cx=cx)

    # GO analysis
    goViewer("go",input,output,session,ui=F,cx=cx)
  }

}
