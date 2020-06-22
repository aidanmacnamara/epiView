#' a generic data download widget
#'
#' Am easy to use wrapper for the Shiny download functionality with some
#' arbitrary defaults.
#'
#' @param id the Shiny ID
#' @param input shiny input list
#' @param output Shiny output list
#' @param ui if TRUE (default) returns the UK
#' @param data the data to be downloaded if requested
#' @param label a label to be displayed by the download icon
#'
#' @return

genericDownloadButton=function(id,input=NULL,output=NULL,ui=T,
                               data=NULL,label="download"){

  ns=NS(id)

  if (ui==T){
      uiOutput(ns("download.button"))
  } else {

    # the button will still appear if there is no data to be downloaded
    output[[ns("download.button")]]=renderUI({
      downloadButton(ns("download.data"),label)
    })


    output[[ns("download.data")]]=downloadHandler(



      # file name with the current date-time
      filename=function(){
        sprintf("download %s.txt",date())
      },
      content=function(file){

        # check to see if we have some data
        if (is.null(data)){
          stop("error - no data for download")
        }
        write.table(data,
                    file=file,
                    quote=F,
                    sep="\t",
                    row.names=FALSE)
      }
    )

  }
}
