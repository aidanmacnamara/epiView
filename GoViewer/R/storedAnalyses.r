#' reproduces stored GO analysis to the screen
#'
#' Displays previous results stored in cx$results. If this list is empty, a no analysis
#' message is displayed. Currently only displays plots but will extend to include tables
#'
#' @param id a shiny widget ID
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param ui TRUE selects user interface components, o/w server
#' @param cx a shiny reactive list containing the stored analyses in $results
#'
#' @return
#' @export

storedAnalyses=function(id,input=NULL,output=NULL,session=NULL,ui=T,cx=NULL){

  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # Shiny UI - created dynamically by the server

    uiOutput(ns("qq.display"))


  } else {


    # ====================================================================================================
    # server components

    # ....................................................................................................
    # UI interface components

    output[[ns("qq.display")]]=renderUI({

      if (length(cx$results) > 0){

        list(fluidRow(
          column(4,selectInput(ns("stored.analysis"),
                               label="select previous analysis output",
                               choices = invertList(names(cx$results)))),
          column(2,list(goResultsDownload(ns("download"),message=F),
                        reportGenerator(ns("report")))),
          column(6,htmlOutput(ns("title")))
        ),
        goQQviewer(ns("qq"),input,output))

      } else {
        HTML("<br>no previous analyses selected")
      }

    })

    # ....................................................................................................
    # results output

    # ... accessing the results entry

    iResult=reactive({
      as.numeric(input[[ns("stored.analysis")]])
    })


    selectedResults=reactive({
      cx$results[[iResult()]]
    })

    goResultsObject=reactive({
      selectedResults()$goResultsObject
    })

    goMethods=reactive({
      selectedResults()$goMethods
    })

    goTitle=reactive({
      selectedResults()$description
    })

    # ... display results

    output[[ns("title")]]=renderText({
      paste("<br>",goTitle())
    })

    goQQviewer(ns("qq"),input,output,session,ui=F,
               goObject=NULL,
               goResultsObject=goResultsObject,
               goMethods=goMethods)

    # ... download tab

    goResultsDownload(ns("download"),input,output,
                      ui=F,
                      goObject=selectedResults()$goObject,
                      goResultsObject=goResultsObject)

    # ... report generation

    reportGenerator(ns("report"),input,output,ui=F,
                    rmdScript = "goReport.rmd",
                    params=list(now=date(),
                                goObject=selectedResults()$goObject,
                                goResultsObject=goResultsObject())
                    )
  }
}
