#' displays a listing of selections or analysis objects
#'
#' @param id
#' @param input
#' @param output
#' @param session
#' @param ui
#' @param cx
#' @param entryName
#' @param title
#' @param rowDisplay
#'
#' @return
#' @export
#'
#' @examples
#'
analysisObjectSummary=function(id,input,output=NULL,session=NULL,ui=T,cx=cx,
                               entryName="selections",
                               title="<font color='red'>analysisObjectSummary title argument missing</font><br>",
                               rowDisplay=function(w){
                                 c(msg="analysisObjectSummary rowDisplay argument missing",time=date())
                               }){

  ns=NS(id)

  if (ui){

    list(uiOutput(ns("selection.summary")),
         uiOutput(ns("delete.request")))

  } else {


    output[[ns("selections")]]=DT::renderDataTable({
      out=plyr::ldply(cx[[entryName]],rowDisplay)
      out$.id=NULL
      out},
      rownames = F,
      options = list(sDom  = '<"top">rt<"bottom">'),
      class="compact")

    output[[ns("selection.summary")]]=renderUI({

      if (length(cx[[entryName]]) > 0){

        list(HTML(title),
             wellPanel(DT::DTOutput(ns("selections"))),
             HTML("<br>"))
      }
    })

    output[[ns("delete.request")]]=renderUI({

      deleteRows=input[[ns("selections_rows_selected")]]
      nDeleteRows=length(deleteRows)

      if (nDeleteRows > 0){

        actionButton(ns("delete.action"),
                     label=HTML(sprintf("delete the %d highlighted selection(s)",
                                        nDeleteRows)))
      }


    })

    observeEvent(input[[ns("delete.action")]],{
      deleteRows=input[[ns("selections_rows_selected")]]
      cx[[entryName]]=cx[[entryName]][-deleteRows]
    })



  }


}
