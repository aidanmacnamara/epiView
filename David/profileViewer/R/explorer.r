#' explores one set of data, e.g. THP-1
#'
#' @param id shiny ID
#' @param input shiny input object
#' @param output shiny output object
#' @param ui TRUE selects the UI o/w server side
#' @param rv reactive list with data and current states
#'
#' @import rclipboard
#' @export

explorer=function(id,input=NULL,output=NULL,session=NULL,ui=T,rv=NULL){
  ns=NS(id)

  library(rclipboard)

  selectionPMA=reactiveValues(genes=list())

  if (ui){

    cat("ENTER: explorer (UI)\n")

    list(
      sidebarLayout(

        sidebarPanel(
          geneFilter(ns("gene.filter")),
          selectInput(ns("combine.rule"), label = "combination rule",
                      choices = list("pass both filters",
                                     "pass one filter"),
                      selected = 1)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("single",
                     plotOutput(ns("plot")),
                     uiOutput(ns("plot.scroller")),
                     sliderInput(ns("plot.size"),
                                 "plot size",
                                 min=1,max=6,step=0.1,
                                 value=3),
                     uiOutput(ns("source.widget")),
                     checkboxInput(ns("log.scale"), label = "log scale", value = FALSE),
                     uiOutput(ns("filters3")),
                     verbatimTextOutput(ns("total.gene.count")),
                     wellPanel(
                       selectInput(ns("multiview"), label = "number of plots to display",
                                   choices = list(1,6,9,12,15),
                                   selected = 1),
                       checkboxInput(ns("multiscale"), label = "different plot scales (multiple plots)", value = FALSE),
                       checkboxInput(ns("mark.donors"), label = "mark  donors", value = FALSE)
                     ),
                     geneCopyTool(ns("copy.button1")),

            ),
            tabPanel("combined",
                     plotOutput(ns("combined.plot")),
                     checkboxInput(ns("combined.plot.log.scale"),
                                   label="plot counts on log scale (recommended)",
                                   value=FALSE),
                     checkboxInput(ns("directions"), label = "split by primary direction", value = FALSE),
                     geneCopyTool(ns("copy.button2"))
            ),
            tabPanel("parameters",
                     plotOutput(ns("report.plots")),
                     verbatimTextOutput(ns("report.text"))),
            tabPanel("filters",
                     presets(ns("presets"))),
            tabPanel("distribution",
                     geneDistributions(ns("distribution"))),
            tabPanel("samples",
                     tableOutput(ns("samples")))
          )
        )
      )
    )

  } else {

    cat("ENTER: explorer (server)\n")

    # matching server parts

    geneFilter(ns("gene.filter"),input,output,session=session,ui=F,rv=rv,selectionPMA=selectionPMA)
    presets(ns("presets"),input,output,ui=F,rv=rv)
    geneCopyTool(ns("copy.button1"),input,output,ui=F,genes=selectedGenes,rv=rv)
    geneCopyTool(ns("copy.button2"),input,output,ui=F,genes=selectedGenes,rv=rv)
    geneDistributions(ns("distribution"),input,output,ui=F,rv=rv)

    # handles for input data

    alphaInter=reactive({
      as.numeric(input[[ns("gene.filter-agree.alpha")]])/100
    })

    alphaFinal=reactive({
      as.numeric(input[[ns("gene.filter-final.alpha")]])/100
    })

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


    # returns a list of genes selected by the filters
    selectedGenes=reactive({

      genesPMA=selectionPMA$genes

      # we only look at the contrasts for the genes already selected on PMA
      # NOTE: nasty cross referencing of the geneFilter()
      interGenesTable=addScore(subset(rv$contrasts$inter,
                                      gene %in% genesPMA),
                               alpha = alphaInter())

      nInterGenes=as.integer(input[[ns("gene.filter-agree.level")]])


      if (!is.null(nInterGenes)){
        sequence=order(interGenesTable$zero.score)
        interGenes=interGenesTable$gene[sequence[nInterGenes[1]:nInterGenes[2]]]
      } else {
        interGenes=NULL
      }

      finalGenesTable=addScore(subset(rv$contrasts$final,
                                      gene %in% genesPMA),
                               alpha = alphaFinal())
      nFinalGenes=as.integer(input[[ns("gene.filter-final.level")]])

      if (!is.null(nFinalGenes)){
        sequence=order(finalGenesTable$zero.score)
        finalGenes=finalGenesTable$gene[sequence[nFinalGenes[1]:nFinalGenes[2]]]
      } else {
        finalGenes=NULL
      }

      # further filter by primary values if needed
      if (length(rv$primaryValueFilterList) > 0){
        finalGenes=intersect(finalGenes,rv$primaryValueFilterList)
      }

      if (input[[ns("combine.rule")]] == "pass both filters"){
        intersect(interGenes,finalGenes)
      } else {
        union(interGenes,finalGenes)
      }

    })


    # create plot of data on screen

    output[[ns("plot")]]=renderPlot({

      genes=selectedGenes()
      index=as.integer(input[[ns("plot.number")]])

      n=as.integer(input[[ns("multiview")]])

      if (n>1){
        index=iwindow(index,n,length(genes))
      }

      size = input[[ns("plot.size")]]

      gene=genes[index]
      if (!is.null(gene)){

        sourceType = input[[ns("source")]]

        if (sourceType == "normalised"){
          plotData=rv$counts
        } else {
          plotData=rv$raw[[sourceType]]
        }

        plotProfile(selectPlotData(plotData,gene=gene),
                    multiscale = input[[ns("multiscale")]],
                    log=input[[ns("log.scale")]],
                    id=input[[ns("mark.donors")]],
                    size=size)
      }

    })

    output[[ns("samples")]]=renderTable({

      genes=selectedGenes()
      index=as.integer(input[[ns("plot.number")]])

      n=as.integer(input[[ns("multiview")]])

      if (n>1){
        index=iwindow(index,n,length(genes))
      }

      gene=genes[index]
      if (!is.null(gene)){

        A=as.data.frame(
          table(as.character(
            selectPlotData(rv$counts,gene=gene)$Label)))

        colnames(A)=c("sample","count")
        cbind(no=1:nrow(A),A)
      }

    })



    output[[ns("plot.scroller")]]=renderUI({

      n.plots=length(selectedGenes())

      sliderInput(ns("plot.number"),
                  "scroll selected genes",
                  min=1,max=n.plots,
                  value=1)

    })

    output[[ns("combined.plot")]]=renderPlot({

      nLimit=1000

      genes=selectedGenes()
      nGenes=length(genes)

      if (nGenes > 0){

        if (nGenes <= nLimit){
          g=plotCombinedProfiles(selectPlotData(rv$counts,genes),
                                 directions=input[[ns("directions")]])
          if (input[[ns("combined.plot.log.scale")]]){
            g+scale_y_log10()
          } else {
            g
          }
        } else {
          textPage(sprintf("more than %d genes",nLimit))
        }
      } else {
        textPage("no genes selected")
      }
    })



    onlyFiniteValues=function(x){
      # note, in R NA is not finite
      x[is.finite(x)]
    }

    pValues=reactive({
      plyr::rename(plyr::ldply(rv$contrasts,
                               function(w){
                                 data.frame(pvalue=subset(w,gene %in% selectedGenes())$pvalue)
                               }),c(".id"="contrast"))
    })


    output[[ns("report.plots")]]=renderPlot({

      a=pValues()
      ggplot(a,aes(contrast,-log10(a$pvalue))) +
        #     geom_boxplot() +
        geom_point(position=position_jitter(0.3,0),alpha="0.1",colour="blue") +
        labs(x="",y="-log10 pvalue")
    })





    output[[ns("report.text")]]=renderPrint({

      cat(">>> p-values (100% = max, 0% = min, inter -> agrees, final -> follows) \n\n")
      print(plyr::ddply(pValues(),
                        ~contrast,
                        function(w){
                          signif(quantile(onlyFiniteValues(w$pvalue)),5)
                        }))


      cat("\n")
      cat(">>> alpha settings\n")
      cat("agreement:",signif(alphaInter(),5),"\n")
      cat("final value:",signif(alphaFinal(),5))

      cat("\n\n")
      cat(">>> SE penalty scale factors\n")
      cat("agreement:",signif(qnorm(1-alphaInter()),5),"; ")
      cat("final value:",signif(qnorm(1-alphaFinal()),5))
    })

    ## >> post selection filtering

    output[[ns("filters3")]]=renderUI({

      # data range
      dataRange=signif(range(rv$counts$value),3)

      value=input[[ns("split.time")]]
      # note need the || idiom because value initially undefined

      if (is.null(value) || !value){

        wellPanel(list(HTML("<b><font color='orange'>filter by primary values</font></b><br>"),
                       sliderInput(ns("primary.range.1"),"average",
                                   min=dataRange[1],max=dataRange[2],value=dataRange),
                       checkboxInput(ns("split.time"),label="split by time",value=FALSE)

        ))

      } else {
        wellPanel(list(HTML("<b><font color='orange'>filter by primary values</font></b><br>"),
                       sliderInput(ns("primary.range.1"),"pre-value",
                                   min=dataRange[1],max=dataRange[2],value=dataRange),
                       sliderInput(ns("primary.range.2"),"post-value",
                                   min=dataRange[1],max=dataRange[2],value=dataRange),
                       checkboxInput(ns("split.time"),label="split by time",value=TRUE)
        ))
      }
    })

    primaryValueFilter=reactive({

      range1=input[[ns("primary.range.1")]]

      if (input[[ns("split.time")]]){
        range2=input[[ns("primary.range.2")]]

        temp = rv$minmax %>%
          filter(mean0 >= range1[1] & mean0 <= range1[2]) %>%
          filter(mean6 >= range2[1] & mean6 <= range2[2])

        temp$gene

      } else {
        temp = rv$minmax %>%
          mutate(mean=(n0*mean0+n6*mean6)/(n0+n6)) %>%
          filter(mean >= range1[1] & mean <= range1[2]) %>%
          select(gene)
        temp$gene
      }

    })

    updateGuides=reactive({
      rv$guides=input[[ns("primary.range.1")]]
      if (input[[ns("split.time")]]){
        rv$guides2=input[[ns("primary.range.2")]]
      } else {
        rv$guides2=NULL
      }
    })


    output[[ns("total.gene.count")]]=renderText({
      updateGuides()
      x=primaryValueFilter()
      rv$primaryValueFilterList=x
      paste("primary value filter:",length(x),"genes\n(applied last)")
    })




  }
}
