#' creates a download page for the Venn diagram panel
#'
#' @param id shiny ID
#' @param input shiny input object
#' @param output  shiny output object
#' @param ui true if use in UI mode
#' @param setSelections a reactive object created by \link{vennDiagramPanel}
#'
#' @return
#' @export

vennDiagramDownload=function(id,input,output,ui=T,setSelections=NULL){

  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # user interface

    uiOutput(ns("download.data.field"))

  } else {

    # ====================================================================================================
    # server size

    if (is.null(setSelections)){
      stop("server-side use requires value for 'setSelections'")
    }

    # ....................................................................................................
    # information display / download UI component

    output[[ns("download.data.field")]]=renderUI({
      selection=setSelections()

      if (length(selection)){
        list(HTML(sprintf("<br>reporting intersections between %d selections: %s<br><br>",
                          length(selection),
                          andList(names(selection)))),
             downloadButton(ns("download.data"),"download"))
      } else {
        NULL
      }

    })

    # ....................................................................................................
    # data nandler for download

    output[[ns("download.data")]]=downloadHandler(

      # file name with the current date-time
      filename=function(){
        sprintf("gene selections %s.txt",date())
      },
      content=function(file){
        selection=setSelections()
        write.table(outputVennTable(selection,
                                    c("-","in set")),
                    file=file,
                    quote=F,
                    sep="\t",
                    row.names=FALSE)
      }
    )
  }
}
