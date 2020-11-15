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
      x=last.update$x

      name=last.update$name
      if (is.null(name) | nchar(name) ==0){name="none-given"}

      if (is.null(type)){

        wellPanel(
          HTML("No directions found. Use the <font color='blue'>explore directions</font> option on the PCA controls to select a direction and
               press <font color='blue'>save</font> to store. This panel will then show full details of the last saved direction.")
        )

      } else {

        # a helper function to display the weight information
        geneArea=function(data){
          wellPanel(textAreaInput(ns("not.used"),
                                  label="gene weightings",
                                  rows=10,
                                  value=dataframe2tabs(data)))
        }

        if (type == "custom"){
          print(last.update$direction)

          list(HTML("<h3>reporting custom user selected direction</h3>",
                    "Weightings and PCA information for ",
                    "<font color='blue'>",nrow(x),
                    "</font> genes. Direction selected at <font color='blue'>",
                    last.update$time,"</font>",
                    " have been copied to an input box. From here you can copy and paste ",
                    "to notepad or Excel.<h1></h1>"),
               geneArea(cbind(last.update$x,last.update$pca.components)),
               HTML("<h1></h1>",
                    "These results were based upon direction of <font color='blue'>",
                   vector2string(signif(last.update$direction,5)),
                   "</font> defined on <font color='blue'>",
                   vector2string(colnames(last.update$pca.components)),
                   "</font> saved as <font color='blue'>",name,"</font>."))



        } else if (type == "standard"){

          pcDir=last.update$pcDir
          colnames(x)[2]=paste0("PC",pcDir)

          list(HTML("<h3>standard principal component direction</h3>",
               "Weightings (loadings) for the principal component <font color='blue'>",pcDir,"</font>",
               "for",
               "<font color='blue'>",nrow(x),
               "</font> genes. Selection saved  at <font color='blue'>",
               last.update$time,"</font>, saved as <font color='blue'>",name,"</font>",
               "has been copied to an input box. From here you can copy and paste ",
               "to notepad or Excel.<h1></h1>"),
               geneArea(x))

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
