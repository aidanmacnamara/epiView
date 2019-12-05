#' produce a observed vs expected quantile plot for GO enrichment
#'
#' Produces a QQ plot for a GO enrichment p-values
#'
#' @param id a Shiny UI id
#' @param input a Shiny input object
#' @param output a Shiny output object
#' @param session a Shiny output object
#' @param ui true generates the UI, false implements the server
#' @param goObject a gene ontology object (assumed reactive)
#' @param goResultsObject a gene ontology results object (assumed reactive)
#' @param goMethods a vector of one or more GO enrichment methods
#' @param title an optional title
#' @param title.nchar the number of characters per line in the title
#' @param goStatus a reactive list. If present, goStatus$ready=FALSE will inactivate this panel and cause it to return a NULL (blank) widget.
#'
#' @export
#' @import stringr
#'
#' @examples
goQQviewer=function(id,input,output,session=NULL,ui=T,
                    goObject=NULL,goResultsObject=NULL,
                    goMethods=NULL,goSavedObject=NULL,
                    title=NULL,
                    title.nchar=80,
                    goStatus=NULL){

  ns=NS(id)


    if (ui){

      list(uiOutput(ns("displayResultsChoice")),
           plotOutput(ns("plot"),
                      brush=clickOpts(id=ns("plot_brush")),
                      click=clickOpts(id=ns("plot_click"))),
           tableOutput(ns("message")))

    } else {


      # #  if (!is.null(goSavedObject)){
      #      goResultsObject2=reactive({input[[ns("test")]]
      #        goSavedObject$goResultsObjectValue})
      #      goMethods2=reactive({input[[ns("test")]]
      #        goSavedObject$goMethods})
      # #  }
      #
      #      if (!is.null(goSavedObject)){
      #        goResultsObject=goResultsObject2
      #        goMethods=goMethods2
      #      }


      goStatusReady=reactive({
        is.null(goStatus) || goStatus$ready
      })


      if (is.null(goMethods)){
        stop("goMethods required for server call")
      }

      chooseResultsOutput=function(){
        goMethods=input[[ns("displayResults")]]
        print(goMethods)

        if (length(goMethods) > 1){
          reorderValueToFront(goMethods,input[[ns("displayResults")]])
          print(reorderValueToFront(goMethods,input[[ns("displayResults")]]))
        } else {
          1
        }
      }

      reactiveQQplotObject=reactive({
        interactiveLog10QqPlot(score(goResultsObject()[[chooseResultsOutput()]]),
                               n=50,force=2,size=2,colour="darkgreen")
      })

      output[[ns("plot")]]=renderPlot({

        if (goStatusReady()){
          if (is.null(title)){
            title="GO enrichment p-values"
          } else {
            if (is.reactive(title)){
              title=title()
            }
          }

          reactiveQQplotObject()$graph+ggtitle(stringr::str_wrap(title,title.nchar))
        }


      })

      output[[ns("displayResultsChoice")]]=renderUI({

        if (goStatusReady()){
          goTypes=goMethods()

          if (length(goTypes) > 1){
            selectInput(ns("displayResults"),
                        label="displaying results for",
                        choices=goTypes)
          } else {
            NULL
          }
        }

      })


      # display point information

      displayPointInfo=function(points){
        noPointsSelected=length(points$name)

        if (noPointsSelected > 1){
          GO.info(points$name,c("Term"))
        } else if (noPointsSelected == 1) {
          temp=GO.info(points$name,c("Term","Definition"))
          temp$descripton=paste(toupper(temp$Term),temp$Definition,sep=": ")
          temp$Term=NULL
          temp$Definition=NULL
          temp
        } else {
          data.frame(comment="Error: no points requested")
        }
      }

      output[[ns("message")]]=renderTable({

        if (goStatusReady()){
          # responses to both clicks and multiple point selection

          p.brushed=brushedPoints(reactiveQQplotObject()$data,
                                  brush = input[[ns("plot_brush")]],
                                  xvar="qq",
                                  yvar="pp")

          p.clicked=nearPoints(reactiveQQplotObject()$data,
                               coordinfo = input[[ns("plot_click")]],
                               maxpoints=1,
                               xvar="qq",
                               yvar="pp")


          if (!is.null(session)){
            # clears selection after use. Wrapper is to avoid an error
            # if session not defined
            session$resetBrush(ns("main.plot_brush"))
          }

          p=NULL

          if (nrow(p.brushed) > 0){
            p=p.brushed
          } else if (nrow(p.clicked) > 0){
            p=p.clicked
          }

          if (!is.null(p)){
            displayPointInfo(p)
          } else {
            data.frame(comment="no points selected")
          }
        }

      })

    }

}
