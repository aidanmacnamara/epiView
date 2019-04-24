#' creates a tab view for Venn diagrams
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param cx
#'
#' @export
#'
#' @return


vennDiagramPanel=function(id,input,output,ui=T,cx=NULL){

  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # user interface

    fluidPage(
      HTML("<p>"),
      column(4,wellPanel(
        uiOutput(ns("select.sets.choice")))),
      column(8,tabsetPanel(
        tabPanel("full report",
                 tableOutput(ns("matrix")),
                 verbatimTextOutput(ns("comment"))),
        tabPanel("graphical",
                 plotOutput(ns("graph"))),
        tabPanel("two-way overlaps (restricted)",
                 tableOutput(ns("matrix2"))),
        tabPanel("download",
                 vennDiagramDownload(ns("download"),input,output))
      ))
    )


  } else {

    # ====================================================================================================
    # server size

    # ....................................................................................................
    # dynamic UI elements

    #  selection selector, or a message if less than two elements found

    output[[ns("select.sets.choice")]]=renderUI({

      namesOfSets=names(availableSets())

      if (length(namesOfSets) > 0){
        selectInput(ns("select.sets"),
                    label="select sets",
                    choices=invertList(namesOfSets),
                    multiple = TRUE,
                    selected = 1:length(namesOfSets))
      } else {
        HTML("<p><b>awaiting selections.</b> Use the viewer to select two or more selections for this analysis.</p>")
      }
    })

    # ....................................................................................................
    # reactive components

    # a list of the avaliable gene selections

    availableSets=reactive({
      selections=cx$selections

      if (length(selections) > 1){

        # first pick out all the items that are selected 'w==1'
        out=lapply(lapply(selections,"[[","data"),function(w){
          names(w)[w==1]})

        # and then pick out the names
        names(out)=unlist(lapply(selections,"[[","name"))

        # and return
        out
      } else {
        NULL
      }
    })

    # a list of the selected gene set selections

    setSelections=reactive({
      availableSets()[as.numeric(input[[ns("select.sets")]])]
    })

    # ....................................................................................................
    # output tables

    # result handler for the full Venn diagram output

    output[[ns("matrix")]]=renderTable({

      currentSets=setSelections()

      if (length(currentSets) >= 2){
        formatVennDiagram(computeVennDiagram(currentSets))
      } else {
        NULL
      }
    },
    rownames=TRUE)

    # result handler for the top level two-way intersection analysis

    output[[ns("matrix2")]]=renderTable({
      selections=cx$selections[as.numeric(input[[ns("select.sets")]])]

      if (length(selections) > 1){
        computeTwoWayVennDiagram(selections)
      } else {
        NULL
      }
    },
    rownames=TRUE,
    na="")

    # generic comment listing the number of selections and the corresponding gene numbers
    # and universe

    output[[ns("comment")]]=renderText({

      selections=cx$selections[as.numeric(input[[ns("select.sets")]])]

      nSelections=length(selections)

      if (nSelections > 0){
        statistics=countUniverse(selections)
       sprintf("%d selections with %d genes out of a possible total of gene universe of %d",
                      nSelections,
                      statistics$selected,
                      statistics$total)
      } else {
        NULL
      }

    })

    # graphical output

    output[[ns("graph")]]=renderPlot({
      # taken from comment code above
      selections=cx$selections[as.numeric(input[[ns("select.sets")]])]
      all.data=lapply(selections,"[[","data")
      allGenes=setListHelper(lapply(all.data,names),union)
      plotLimmaVennDiagram(setSelections(),allGenes=allGenes)
    })

    # download handler

    vennDiagramDownload(ns("download"),input,output,ui=F,setSelections)

  }
}


