#' generate a downloadable report
#'
#' This funcion provides a UI and serverside pair generate a written report from a .rmd markdown file. Currently only
#' PDF output is upported but additional formats could be provided later. Note that the UI side is called with the id argument set only:
#' the button itself (with any associated text) is created by the server. This ensures that the id's for the download action
#' are automatically matched. The report generation process can be slow, so a progress bar is provided. The markdown conversion appears broadly
#' stable but not yet 100%.
#'
#' @param id Shiny widget ID
#' @param input Shiny input opbject
#' @param output Shiny output opbject
#' @param ui if true, the UI output is created, otherwise server
#' @param buttonLabel a lable for the button (set from the SERVER side)
#' @param params an additional list of parameters to pass to the report generator. If NULL (default) a list with a single entry, now,
#'    passed for the current date
#' @param rmdScript the R markdown script to run. See \url{https://shiny.rstudio.com/articles/generating-reports.html} for a discussion
#'
#' @export
#'
reportGenerator=function(id,input=NULL,output=NULL,
                         ui=T,
                         buttonLabel="download report",
                         params=NULL,
                         rmdScript="report.rmd"
){

  ns = NS(id)

  if (ui){

    uiOutput(ns("button"))

  } else {

    output[[ns("button")]]=renderUI({
      downloadButton(ns("action"),buttonLabel)
    })

    output[[ns("action")]] = downloadHandler(

      filename = "report.docx",


      content = function(file){

        tmpReport = file.path(tempdir(),rmdScript)
        file.copy(rmdScript,tmpReport,overwrite = T)

        if (is.null(params)){
          params = list(now=date())
        }


        withProgress(message="generating report",
                     value=0.80,
                     {
                       rmarkdown::render(tmpReport,
                                         output_format = "word_document",
                                         output_file = file,
                                         params = params,
                                         envir = new.env(parent=globalenv()),
                                         clean=T)
                     })

      }
    )
  }
}
