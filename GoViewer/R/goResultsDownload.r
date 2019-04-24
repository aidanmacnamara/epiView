#' downloads a copy of the GO output
#'
#' Implements a download button to store GO analysis outputs. Objects can be reactive
#' or no-reactive being excuted in the later case to make the code more portable.
#' OUtput is currently restricted to tab-deliminated .txt
#'
#' @param id root for the Shiny widget ID
#' @param input Shiny input object
#' @param output SHiny output object
#' @param ui if T returns UI, otherwise server side support
#' @param goObject a GO object from TopGO (optionally a reactive object)
#' @param goReultsObject a GO results object from TopGO (optinally a reactive object)
#' @param nameFormat the name of the output file. May not contain the
#'    character % other than fillowed by an s - %s - where it is expanded
#'    to the current data using \[base]{sprintf}.
#' @param goStatus a reactive list. If present, goStatus$ready=FALSE, the download button is surpressed and replaced by a message
#'    instructing the user to first carry out a calculation.
#'
#' @import topGO
#' @export

goResultsDownload=function(id,input=NULL,output=NULL,ui=T,
                           goObject=NULL,goResultsObject=NULL,
                           fileName="GO downloaded %s.txt",
                           message=T,
                           goStatus=NULL){

  require(topGO)

  ns=NS(id)

  if (ui==T){
    uiOutput(ns("download.button"))
  } else {

    goStatusReady=reactive({
      is.null(goStatus) || goStatus$ready
    })

    if (message){
      messageText=HTML("<p>if not already completed, execution will be triggered as soon as you start to download the data</p>")
    } else {
      messageText=NULL
    }

    output[[ns("download.button")]]=renderUI({
      if (goStatusReady()){
        list(downloadButton(ns("download.data"),"download"),
             messageText)
      } else {
        HTML("<p>no data available - run analysis first. Execution will start when you view the results or start a download</p>")
      }
    })

    output[[ns("download.data")]]=downloadHandler(

      # file name with the current date-time
      filename=function(){
        sprintf(fileName,date())
      },
      content=function(file){

        # evaluate reactive lists prior to analysis

        if (is.reactive(goObject)){
          tempObject=goObject()
        }  else {
          tempObject=goObject
        }

        if (is.reactive(goResultsObject)){
          tempResults=goResultsObject()
        } else {
          tempResults=goResultsObject
        }

        write.table(do.call("GenTable",
                            c(list(tempObject),
                              tempResults,
                              list(numChar=500,
                                   topNodes=200))),
                    file=file,
                    quote=F,
                    sep="\t",
                    row.names=FALSE)
      }
    )

  }
}
