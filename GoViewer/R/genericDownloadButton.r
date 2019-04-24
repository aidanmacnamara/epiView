#' Title
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param data
#'
#' @return
#' @export

genericDownloadButton=function(id,input=NULL,output=NULL,ui=T,data=NULL){

  ns=NS(id)

  if (ui==T){

    if (length(data)>0){
      uiOutput(ns("download.button"))
    } else {
      HTML("<font color='red'>no data specified</font>")
    }


  } else {

    if (is.null(data)){
      stop("error - no data for download")
    }

    output[[ns("download.button")]]=renderUI({
      downloadButton(ns("download.data"),"download")
    })


    output[[ns("download.data")]]=downloadHandler(

      # file name with the current date-time
      filename=function(){
        sprintf("GO downloaded %s.txt",date())
      },
      content=function(file){
        A=data.frame(x=1:10)

        write.table(A,
                    file=file,
                    quote=F,
                    sep="\t",
                    row.names=FALSE)
      }
    )

  }
}
