directionReportPanel=function(id,input=NULL,output=NULL,ui=T,info=NULL){
  ns=NS(id)

  if (ui){

    list(
      HTML("<h1></h1>"),
      uiOutput(ns("report"))
    )

  } else {

    output[[ns("report")]]=renderUI({

      last.update=info$last.update
      type=last.update$type

      if (is.null(type)){

        wellPanel(
          HTML("No directions found. Use the <font color='blue'>explore directions</font> option on the PCA controls to select a direction and
               press <font color='blue'>save</font> to store. This panel will then show full details of the last saved direction.")
        )

      } else {

        if (type == "custom"){

          x=last.update$x

          print(last.update$direction)
          print(head(last.update$pca.components))

          list(HTML("<h3>reporting custom user selected direction</h3>",
                    "Weightings and PCA information for ",
                    "<font color='blue'>",nrow(x),
                    "</font> genes absed upon a direction selected at <font color='blue'>",
                    last.update$time,"</font>",
                    " have been copied to an input box. From here you can copy and paste ",
                    "to a notepad or Excel.<h1></h1>"),
               wellPanel(textAreaInput(ns("not.used"),
                                       label="gene weightings",
                                       rows=10,
                                       value=dataframe2tabs(cbind(last.update$x,
                                                                  last.update$pca.components)))),
               HTML("<h1></h1>",
                    "These results were based upon direction of",
                   vector2string(signif(last.update$direction,5)),
                   "defined on",
                   vector2string(colnames(last.update$pca.components))))



        } else if (type == "standard"){

          HTML("<h3>standard principal component direction</h3>")

        } else {
          stop("direction selection type unknown: ",type)

        }
      }
    })



    output[[ns("summary")]]=renderText({
      date()
    })

  }
}
