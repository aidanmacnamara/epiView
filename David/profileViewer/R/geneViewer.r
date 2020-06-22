#' panel to display user selected genes
#'
#' Allows users to copy and past lists of gene names into a text box and display the results as a series of profile plots.
#'
#' @param id shiny ID
#' @param input shiny input object
#' @param output shiny output object
#' @param ui TRUE selects the UI o/w server side
#' @param rv reactive list with data and current states
#'

#' @export

geneViewer=function(id,input=NULL,output=NULL,ui=T,rv=rv){

  ns=NS(id)

  if (ui){

    list(
      plotOutput(ns("plot")),
      uiOutput(ns("source.widget")),
      checkboxInput(ns("log.scale"), label = "log scale", value = FALSE),
      textAreaInput(ns("genes"),
                    label = "enter required genes",
                    placeholder = "genes deliminated by semicolons, spaces or commas",
                    rows=10),
      htmlOutput(ns("no.genes")),
      uiOutput(ns("controls"))
    )

  } else {

    # lists the active data sources

    dataSourceList=reactive({
      c("normalised",names(rv$raw))
    })

    # active data source picker

    output[[ns("source.widget")]]=renderUI({
      selectInput(ns("source"),"data source",
                  choices = dataSourceList(),
                  selected = 1)
    })


    output[[ns("controls")]]=renderUI({

      n = nGenes()

      if (n > 0){
        list(
          checkboxInput(ns("multiscale"), label = "different plot scales (multiple plots)", value = FALSE),
          checkboxInput(ns("mark.donors"), label = "mark  donors", value = FALSE),
          sliderInput(ns("plot.number"),
                      "scroll selected genes",
                      min=1,max=n,value=1),

          sliderInput(ns("number.of.plots"),
                      "max number of genes per plot",
                      min=1,max=32,
                      step=1,
                      value=1),
          sliderInput(ns("plot.size"),
                      "plot size",
                      min=1,max=6,step=0.1,
                      value=3)
        )
      }


    })

    requestedGenes=reactive({
      parseGeneNames(input[[ns("genes")]])
    })

    nGenes=reactive({
      length(requestedGenes())
    })

    output[[ns("no.genes")]]=renderText({
      paste(nGenes(),"genes provided")
    })


    output[[ns("plot")]] = renderPlot({


      genes=requestedGenes()

      if (length(genes) > 0){

        selection = iwindow(input[[ns("plot.number")]],
                            input[[ns("number.of.plots")]],
                            length(genes))

        size=input[[ns("plot.size")]]
        sourceType = input[[ns("source")]]

        if (sourceType == "normalised"){
          plotData=rv$counts
        } else {
          plotData=rv$raw[[sourceType]]
        }


        plotProfile(selectPlotData(plotData,gene=genes[selection]),
                    multiscale = input[[ns("multiscale")]],
                    log=input[[ns("log.scale")]],
                    id=input[[ns("mark.donors")]],
                    size=size)

      }

    })

  }
}
