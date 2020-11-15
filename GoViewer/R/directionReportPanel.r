#' direction informmation panel
#'
#' Creates a special panel for the display of information about selected directions in the PCA space. Currently supports
#' a selected principal component or a composite direction taken from the projection onto two or more PCs. Displays information
#' about the selection and a text input area from which weightings (loadings) can be downloaded. Information is displayed for
#' the last set of directional information saved.
#'
#' @param id Shiny ID for this widget
#' @param input a Shiny input object
#' @param output a Shiny output object
#' @param ui if TRUE, the UI is returned, otherwise the server is implemented
#' @param info a reactive values list from \link{pcaPanel} containing the information to be displayed.
#'
#' @return
#' @export

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

      # the type of output depends on the type of selection last saved

      if (is.null(type)){

        # ... if no directions were saved, print a message to inform the user

        wellPanel(
          HTML("No directions found. Use the <font color='blue'>explore directions</font> option on the PCA controls to select a direction and
               press <font color='blue'>save</font> to store. This panel will then show full details of the last saved direction.")
        )

      } else {

        # ...otherwise decide on what should be displayed from the type of directions to report

        x=last.update$x # we will now always have these
        name=last.update$name
        if (is.null(name) | nchar(name) ==0){name="none-given"}

        # a helper function to display the weight information
        geneArea=function(data){
          wellPanel(textAreaInput(ns("not.used"),
                                  label="gene weightings",
                                  rows=10,
                                  value=dataframe2tabs(data)))
        }

        if (type == "custom"){

          # >>> reports a compound direction along with the vector in PCA space and the loadings of the
          #     corresponding eigenvectors used.

          list(HTML("<h3>reporting custom user selected direction</h3>",
                    "Weightings and directional information defined on",
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

          # >>> reports loadings for a single [simple] PC component

          pcDir=last.update$pcDir
          colnames(x)[2]=paste0("PC",pcDir)

          list(HTML("<h3>standard principal component direction</h3>",
               "Weightings (loadings) for the principal component <font color='blue'>",pcDir,"</font>",
               "defined on",
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


  }
}
