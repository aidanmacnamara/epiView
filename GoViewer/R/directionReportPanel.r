directionReportPanel=function(id,input=NULL,output=NULL,ui=T,info=NULL){
  ns=NS(id)

  if (ui){

    list(
      HTML("<h1></h1>"),
      HTML("This pannel reports infomation about the last selected axes or directions in a PCA plot."),
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

          list(HTML("<h3>custom user selected direction</h3>",
                    "weightings and PCA information have been copied to an input box. From here you can copy and paste ",
                    "to a notepad or Excel."),
               textAreaInput(ns("not.used"),
                             label="gene weightings",
                             value=dataframe2tabs(last.update$x)))

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
