#' test panel for development only
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

devPanel=function(id,input,output,session=NULL,ui=T,cx=cx){
  ns=NS(id)

  if (ui){

    tabsetPanel(
      tabPanel("data",
               saveTool(ns("save"),input,output)),
      tabPanel("cx summary",
               verbatimTextOutput(ns("text"))
      ),
      tabPanel("QQ plot",
               uiOutput(ns("qq.display")))
    )



  } else {

    output[[ns("text")]]=renderPrint({
      for (a in names(cx)){
        print(a)
        print(cx[[a]])
        cat("\n")
      }
    })


    output[[ns("qq.display")]]=renderUI({


      if (length(cx$results) > 0){

        list(selectInput(ns("stored.analysis"),
                         label="select previous analysis output",
                         choices = invertList(names(cx$results))),
             goQQviewer(ns("qq"),input,output))

      } else {
        HTML("<br>no previous analyses selected")
      }

    })

    iResult=reactive({
      as.numeric(input[[ns("stored.analysis")]])
    })

    resultsObject=reactive({
      cx$results[[iResult()]]
    })

    goResultsObject=reactive({
      resultsObject()$goResultsObject
    })

    goMethods=reactive({
      resultsObject()$goMethods
    })

    goTitle=reactive({
      resultsObject()$description
    })




    goQQviewer(ns("qq"),input,output,session,ui=F,
               goObject=NULL,
               goResultsObject=goResultsObject,
               goMethods=goMethods,
               title=goTitle)

    # can not pass to title ???

    saveTool(ns("save"),input,output,ui=F,cx=cx)




  }
}
