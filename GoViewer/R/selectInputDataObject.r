#' selects the input contrast
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param all.inputs a reactive data object containing all the avaliable contrasts
#'
#' @return
#' @export

selectInputDataObject=function(id,input,output,ui,all.inputs=NULL){

  ns=NS(id)

  if (ui){

    list(
        HTML("<p>"),
        uiOutput(ns("choice")),
        HTML("<br><p><i>summary of currently selected dataset:</i></p>"),
        verbatimTextOutput(ns("print")))

  } else {

    # if (is.null(all.inputs)){
    #   stop("all.inputs required to server instance")
    # }

    # only server side function is to power the UI selection

#    print(invertList(lapply(all.inputs(),"[[","name")))


    output[[ns("choice")]]=renderUI({

#      input[[ns("refresh")]]

      selectInput(ns("choice"),
                  label="contrast",
                  choices = invertList(lapply(all.inputs(),"[[","name")))

      # selectInput(ns("choice"),
      #             label="contrast",
      #             choices = list(a=1,b=2))


    })




    output[[ns("print")]]=renderPrint({

        all.inputs()[[input[[ns("choice")]]]]

    })
  }
}
