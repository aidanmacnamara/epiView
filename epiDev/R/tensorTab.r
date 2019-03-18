#' top level wrapper for a tensor tab
#'
#' @param id prefix for Shiny id's
#' @param input Shiny 'input' list
#' @param output Shiny 'output' list
#' @param ui if TRUE defines the UI component, o/w the server
#'
#' @return standard Shiny value
#'
#' @import shiny
#' @import ggplot2
#' @import rTensor
#' @import plyr
#' @import reshape
#'
#' @export

tensorTab=function(id,input,output,ui=T){

  ns=NS(id)

  # make tag-helper functions

  data.id="data"
  trans.id="trans"
  legend.id="legend"
  cp.id="cp"

  makeIdTagFunction=function(id){
    function(tag){paste0(ns(id),"-",tag)}
  }

  ns.data=makeIdTagFunction(data.id)
  ns.trans=makeIdTagFunction(trans.id)
  ns.legend=makeIdTagFunction(legend.id)
  ns.cp=makeIdTagFunction(cp.id)

  ns.cp.detail=makeIdTagFunction(paste(cp.id,"detail",sep="-"))

  # the active code

  if (ui){
    tabsetPanel(
      tensorDataPanel(ns(data.id),input,output),
      tensorDataTransPanel(ns(trans.id),input,output),
      tensorCpAnalysisPanel(ns(cp.id),input,output),
      colourLegendPanel(ns(legend.id),input,output)
    )
  } else {
    cat("... entering server side tensorTab\n")

    # >>> data services

    # fetches a copy of the full data from file
    # fetches a copy of the full data from file

    D0=reactive({
      load("gc.rda")
      a_gc
    })

    # a subsets by the mark selection

    D1=reactive({

      # first filter by marks selected over the UI

      x=D0()

      selection=as.numeric(input[[ns.data("mark-selection")]])

      if (length(selection) > 0){
        # is o/w not NULL but length zero
        x.tmp=x$tmp[selection]
      } else {
        x.tmp=x$tmp
      }

      # first trim by lines

      cell.lines=fullCellLines(x.tmp)

      for (i in 1:length(x.tmp)){
        x.tmp[[i]]$res=x.tmp[[i]]$res[cell.lines,]
        x.tmp[[i]]$annot=x.tmp[[i]]$annot[cell.lines,]
      }

      # and then by genes

      genes=availableGenes(x.tmp)

      for (i in 1:length(x.tmp)){
        x.tmp[[i]]$res=x.tmp[[i]]$res[,genes]
      }

      # apply data transformation if necessary

      x.tmp = transformData(x.tmp,input[[ns.trans("selection")]])

      x$tmp=x.tmp
      x1 <<-x
      x
    })

    # returns a list of cell lines

    cellLines=reactive({
      rownames(D1()$tmp[[1]]$res)
    })

    # returns a list of marks

    markNames=reactive({
      names(D1()$tmp)
    })

    # returns a 3-tensor version of D1()

    T3=reactive({
      foldToThreeTensor(D1()$tmp)
    })

    # dynamic UI components

    # ... to select marks for analysis

    output[[ns.data("mark-selection")]]=renderUI({

      markNames=names(D0()$tmp)
      markNamesList=1:length(markNames)
      names(markNamesList)=markNames

      checkboxGroupInput(ns.data("mark-selection"),
                         label="select marks",
                         choices=as.list(markNamesList),
                         selected=markNamesList,
                         inline=TRUE)
    })

    # ... to select the colour mapping tables


    output[[ns.legend("preset.values")]]=renderText({

      mappings=tensorDataOptions("colour.legend.mappings")
      legendMappingsParser(
        mappings[[input[[ns.legend("preset")]]]])
    })

    colourTable=reactive({
      legendMappingsParser(
        input[[ns.legend("selection")]]
      )
    })


    # responses

    # >>> data

    output[[ns("test")]]=renderPrint({
      date()
    })

    output[[ns.data("upload-status")]]=renderText({
      D0()
      "data uploaded"})



    output[[(ns.data("available-data"))]]=renderText({
      mark1=D1()$tmp[[1]]$res
      paste(ncol(mark1),"genes across",nrow(mark1),"lines available")
    })

    availableLines=reactive({
      a=D1()$tmp[[1]]$annot
      rename(ldply(split(a$Label,a$Project),andList),
             c(".id"="project","V1"="cell lines"))
    })

    output[[ns.data("available-lines")]]=renderTable({
      availableLines()
    })

    output[[ns.legend("available-lines")]]=renderTable({
      availableLines()
    })

    output[[ns.data("mark-project-summary")]]=
      renderTable({
        listMarksByLine(D0()$tmp)
      })

    # >>> transformations

    output[[ns.trans("density")]]=renderPlot({
      plotDistribution(D1()$tmp)
    })

    # >>> CP factorisation

    k=reactive({
      as.numeric(input[[ns.cp("k")]])
    })

    output[[ns.cp("x.axis.choice")]]=renderUI({

      selectInput(ns.cp("x.axis.choice"),
                  label = "x axis",
                  choices = 1:k(),
                  selected = 1)
    })

    output[[ns.cp("y.axis.choice")]]=renderUI({
      k0=k()
      selectInput(ns.cp("y.axis.choice"),
                  label = "y axis",
                  choices = 1:k0,
                  selected = min(2,k0))
    })

    plotDirs=reactive({
      as.numeric(c(
        input[[ns.cp("x.axis.choice")]],
        input[[ns.cp("y.axis.choice")]]
      ))
    })

    output[[ns.cp("tensor")]]=renderPrint({
      T3()
    })

    T3.factors=reactive({
      withProgress(message="please wait",value=0,{
        incProgress(0.5,detail="computing factors")
        cpFactor(T3(),k())
      })
    })



    output[[ns.cp("main.plot")]]=renderPlot({

      cp.obj=T3.factors()
      cp.obj <<- cp.obj

      print(plotDirs())


      simpleScoresPlot(cp.obj$U[[1]],
                       pcs=plotDirs(),
                       row=cellLines(),
                       colours=colourTable(),
                       axes=F,
                       size=input[[ns.cp("label.size")]],
                       scale.x=1,
                       scale.y=1)
    },
    height = tensorDataOptions("image.height"))

    output[[ns.cp.detail("title")]]=renderText({

      pcs=plotDirs()

      if (input[[ns.cp.detail("direction")]] == "horizontal"){
        pc=pcs[1]
      } else {
        pc=pcs[2]
      }

      paste0("<b>",k()," signal model - ",
             round(cp.obj$norm_percent,1),
             "%</b> (component ",pc," detail)")

    })

    output[[ns.cp.detail("mark.plot")]]=renderPlot({

      pcs=plotDirs()

      if (input[[ns.cp.detail("direction")]] == "horizontal"){
        pc=pcs[1]
      } else {
        pc=pcs[2]
      }
      print(pc)
      # print(sigma.mark())
      U3=T3.factors()$U[[3]]
      cpMarkerProfile(U3,pc,markNames())

    })

    output[[ns.cp.detail("gene.histogram")]]=renderPlot({

      pcs=plotDirs()

      if (input[[ns.cp.detail("direction")]] == "horizontal"){
        pc=pcs[1]
      } else {
        pc=pcs[2]
      }

      a=T3.factors()$U
      U2=a[[2]]
      U3=a[[3]]

      Layout=matrix(c(0,0.8,0,1,0.7,1,0,1),nrow=2,ncol=4,byrow=T)
      split.screen(Layout)

      screen(1)
      z=U2[,as.numeric(pc)]
      hist(z,xlab="gene loading",main="",col="gray")
      abline(v=0,col="red")

      screen(2)
      boxplot(z)
      abline(h=0,col="red")

      close.screen(all=T)
    })

  }

}

