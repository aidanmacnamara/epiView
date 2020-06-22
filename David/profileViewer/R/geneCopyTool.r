#' widget to copy gene list or details
#'
#' Allows the user to download either a list (e.g. for enrichment) or
#' full information of a list of genes.
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param genes a reactive function listing the selected genes
#' @param rv a reactive object holding the current data
#'
#' @return
#' @import rclipboard
#' @export
#'
#' @examples
geneCopyTool=function(id,input=NULL,output=NULL,ui=T,genes=NULL,rv=NULL){

  ns=NS(id)

  library(rclipboard)

  if (ui){

    wellPanel(
      rclipboardSetup(),
      uiOutput(ns("copy.button")),
      genericDownloadButton(ns("details"),label="details")
    )

  } else {

    output[[ns("copy.button")]]=renderUI({

      genes0=genes()

      # the clipboard button appears not to like dots in the Shiny IDs
      rclipButton(ns("copy"),
                  label=sprintf("copy %d selected genes to paste",
                                length(genes0)),
                  clipText=paste(genes0,collapse="\n"))
    })


    geneSummary=reactive({
        summaryForSelectedGenes(genes=genes(),
                                       rv=rv)
    })

    genericDownloadButton(ns("details"),input,output,ui=F,data=geneSummary())
  }

}
