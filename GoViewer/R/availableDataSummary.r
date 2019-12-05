#' summary of the available data and selections
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param cx
#'
#' @import DT
#'
#' @return
#' @export
#'
#' @examples
availableDataSummary=function(id,input,output,session,ui=T,cx=NULL){


  # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option



  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # UI

    fluidPage(

      tabsetPanel(
        tabPanel("manage data and analysis",
                 HTML("<br>A top level summary of all the data available and any data selections, if made.<br><br>"),
                 DT::DTOutput(ns("contrast.data")),
                 HTML("<br>"),
                 tabsetPanel(
                   tabPanel("selections",
                            analysisObjectSummary(ns("selections.list"),input)),
                   tabPanel("analysis results",
                            analysisObjectSummary(ns("results.list"),input))
                   )
        ),

        tabPanel("review analyses",
                 storedAnalyses(ns("analysis"),input,output))
        )
    )

  } else {

    # ====================================================================================================
    # server side

    # list of available raw datasets

    output[[ns("contrast.data")]]=DT::renderDataTable({
      data.frame(full_data=unlist(lapply(cx$contrast.data,"[[","name")))
    },
    rownames = F,
    options = list(sDom  = '<"top">rt<"bottom">'),
    class="compact")


    storedAnalyses(ns("analysis"),input,output,session,ui=F,cx=cx)

    analysisObjectSummary(ns("selections.list"),input,output,ui=F,cx=cx,
                          entryName="selections",
                          title="<br>The following gene selections are available for analysis.
                                      To delete a selection, click
                                      to select and wait for a delete button.<br><br>",
                          rowDisplay=function(w){
                            if (w$type == "split"){
                              info = sprintf("gene selection: %d from %d",sum(w$data),length(w$data))
                            } else {
                              info = "ordered response"
                            }
                            c(name=w$name,
                              info=info)
                          })

    # development
    devPanel(ns("dev"),input,output,session,ui=F,cx=cx)

    analysisObjectSummary(ns("results.list"),input,output,ui=F,cx=cx,
                          entryName="results",
                          title="<br>the following analyses have been saved for recall<br><br>",
                          rowDisplay=function(w){
                            c(name=w$name,
                              info=w$description,
                              created=w$created)
                          })


    # ,
    # rowDisplay=function(w){
    #   c(name=w$name,
    #     info=sprintf("analysis created at %s",w$created))
    #}
  }
}
