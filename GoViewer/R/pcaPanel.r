#' the PCA panel
#'
#' @param id shiny widget id
#' @param input shiny input
#' @param output shiny ouput
#' @param ui false selects server, true the UI
#' @param cx a reactive list for data
#'
#' @import plyr
#' @export

pcaPanel=function(id,input,output,session=NULL,ui=T,cx=NULL){

  customDirection=reactiveValues(selected=NA,direction=NA)

  require(plyr)

  ns=NS(id)

  # ====================================================================================================
  # ui side

  if (ui){

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("end.point.selector")),
        uiOutput(ns("extended.end.point.selector")),
        #      bsTooltip(ns("end.point.selector"),"this is the current time"),
        selectInput(ns("transformation"),
                    label="tranformation",
                    choices=list("none"="none","log10"="log10","log2"="log2","square root"="sqrt")),
        selectInput(ns("display.type"),
                    label="display type",
                    choices=list("standard PCA"="pca","apply PCA directions to ..."="project")),
        uiOutput(ns("projection.end.point.selector")),
        fluidRow(
          column(6,uiOutput(ns("x.dir.selector"))),
          column(6,uiOutput(ns("y.dir.selector")))
        ),
        selectInput(ns("reflect.axis"),
                    label="reverse directions",
                    choices=list("no change"="-",
                                 "reverse x"="x",
                                 "reverse y"="y",
                                 "reverse x and y"="xy")),
        checkboxInput(ns("include.origin"),"include origin",TRUE),
        checkboxInput(ns("explore.direction.checkbox"),"explore directions",FALSE),
        uiOutput(ns("explore.direction.items")),
        uiOutput(ns("explore.direction.custom.buttons"))

      ),
      mainPanel(" ",

                tabsetPanel(
                  tabPanel("output",
                           uiOutput(ns("PCA.plot.area")),
                           fluidRow(
                             column(10,sliderInput(ns("point.size"),
                                                   label="plotting size",
                                                   min=1, max=6, value=3)),
                             column(2,textInput(ns("max.k"),
                                                label="max no PCs",
                                                value=6))
                           )
                  ),
                  tabPanel("general",
                           plotOutput(ns("info.plot")),
                           verbatimTextOutput(ns("testing")),
                           verbatimTextOutput(ns("pca.info.text"))),
                  tabPanel("axis interpretation"),
                  tabPanel("legend",
                           legendPanel("legend",input,output))
                )
      )
    )


  } else {

    # ====================================================================================================
    # server side

    legendPanel("legend",input,output,ui=F,cx=cx)


    # ....................................................................................................
    # reactive components

    controls=reactive({
      dirs=as.integer(c(input[[ns("x.dir")]],
                        input[[ns("y.dir")]]))

      list(x.dir=dirs[1],
           y.dir=dirs[2],
           xy.dirs=dirs,
           point.size=as.numeric(input[[ns("point.size")]]),
           include.origin=as.logical(input[[ns("include.origin")]]))
    })

    endPoints=reactive({
      names(cx$epi.data)
    })

    compoundEndpoint=reactive({
      as.numeric(input[[ns("end.point")]]) == length(endPoints())+1
    })

    xyDir=reactive({
      c(as.integer(input[[ns("x.dir")]]),
        as.integer(input[[ns("y.dir")]]))
    })

    maxKrange=reactive({
      1:as.integer(input[[ns("max.k")]])
    })

    ### local helper

    helper001=function(ep){

      if (guard(input,ns,"transformation") & !is.null(input[[ns(ep)]])){

        if (compoundEndpoint()){
          x=NULL

          if (guard(input,ns,"extended.end.point")){
            if (input[[ns("extended.end.point")]] == "H3K4me3 / H3K27me3"){
              x=cx$epi.data[["H3K4me3"]]$res / cx$epi.data[["H3K27me3"]]$res
            } else if (input[[ns("extended.end.point")]] == "RNA / H3K27me3"){
              x=cx$epi.data[["RNA"]]$res / cx$epi.data[["H3K27me3"]]$res
            } else {
              stop("internal error: selected compound endpoint not supported")
            }
          }


        } else {
          ep.name=endPoints()[as.integer(input[[ns(ep)]])]
          x=cx$epi.data[[ep.name]]$res
        }



        trans=input[[ns("transformation")]]

        if (trans == "log10"){
          x=log10(x)
        } else if (trans == "log2"){
          x=log2(x)
        } else if (trans == "sqrt"){
          x=sqrt(x)
        }
        x

      } else {
        NULL
      } # guard

    }

    # gives a view of the primary data used to define the PCA directions
    pcaData1=reactive({
      helper001("end.point")
    })

    # gives a view of the data to be plotted (projected onto the PCA components). If no projection is
    # selected, this is the same as pcaData1()
    pcaData2=reactive({
      if (input[[ns("display.type")]] == "project"){
        helper001("projection.end.point")
      } else {
        pcaData1()
      }
    })

    # ....................................................................................................
    # controls

    # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    # endpoints

    output[[ns("end.point.selector")]]=renderUI({

      selectInput(ns("end.point"),
                  label="response",
                  choices = inverseNumberedList(c(endPoints(),"compound")),
                  selected=1)
    })


    output[[ns("extended.end.point.selector")]]=renderUI({

      if (!guard(input,ns,"end.point")){
        return(NULL)
      }


      if (compoundEndpoint()){
        selectInput(ns("extended.end.point"),
                    label="compound response ...",
                    choices = c("H3K4me3 / H3K27me3",
                                "RNA / H3K27me3"),
                    selected=1)
      } else {
        NULL
      }

    })


    output[[ns("projection.end.point.selector")]]=renderUI({

      if (input[[ns("display.type")]] == "project"){
        selectInput(ns("projection.end.point"),
                    label="... values from",
                    choices = inverseNumberedList(endPoints()),
                    selected=1)
      } else {
        NULL
      }
    })


    # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    # directions

    output[[ns("x.dir.selector")]]=renderUI({

      selectInput(ns("x.dir"),
                  label="x",
                  choices=mirroredList(maxKrange()))

    })

    output[[ns("y.dir.selector")]]=renderUI({

      yRange=setdiff(maxKrange(),as.integer(input[[ns("x.dir")]]))

      selectInput(ns("y.dir"),
                  label="y",
                  choices=mirroredList(yRange))

    })


    output[[ns("explore.direction.items")]]=renderUI({

      if (input[[ns("explore.direction.checkbox")]]){
        list(
          selectInput(ns("explore.direction.type"),
                      label="compute gene information for",
                      choices=c("x-axis"="x",
                                "y-axis"="y",
                                "custom direction"="custom")),
          fluidRow(
            column(8,textInput(ns("explore.direction.title.text"),
                               label=NULL, value="",
                               placeholder = "description")),
            column(2,actionButton(ns("explore.direction.title.button"),
                                  label="save"))
          ),
          HTML("<p><i>Use this panel to select directions or axes you wish to
                 investigate. You can then explore which genes are pathways
                 drive them in the Gene Ontology (GO) viewer panel.</i></p>")
        )
      }

    })

    # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    # handle PCA plot and selection

    # ensured that clicking is only enabled if the custom direction view is selected

    pointSelectionEnabled=reactive({
      directionType=input[[ns("explore.direction.type")]]
      !is.null(directionType) && directionType == "custom"
    })

    output[[ns("PCA.plot.area")]]=renderUI({

      if (pointSelectionEnabled()){
        plotOutput(ns("PCA.plot"),click=ns("PCA.plot_select_point"))

      } else {
        plotOutput(ns("PCA.plot"))
      }

    })

    observeEvent(input[[ns("PCA.plot_select_point")]],{
      point=input[[ns("PCA.plot_select_point")]]
      n=length(cx$pca.selected.points)

      cx$pca.selected.points[[n+1]]=c(min(xyDir()),
                                      max(xyDir()),
                                      point$x,
                                      point$y)
    })

    observeEvent(input[[ns("dir.undo.point")]],{
      n=length(cx$pca.selected.points)
      if (n>0){
        cx$pca.selected.points[[n]]=NULL
      }
    })

    observeEvent(input[[ns("dir.clear.points")]],{
      cx$pca.selected.points=list()
    })


    output[[ns("explore.direction.custom.buttons")]]=renderUI({

      customDirection$selected=NA
      customDirection$direction=NA

      directionType=input[[ns("explore.direction.type")]]

      if (pointSelectionEnabled()){

        # add an informative message to say where directions have been defined

        dirDefined=subset(readPcaPointSelections(cx$pca.selected.points)$dir,!is.na(x))




        if (!is.null(dirDefined) && nrow(dirDefined) > 0){

          # ... first compute the SS from the PCA eigenvalues
          ev=pcaObj()$sdev^2
          percentExplained=100*ev/sum(ev)

          n=length(ev)
          w=wideDirectionFormat(n,dirDefined)
          dirCheck=checkPcaViewConnectivity(w)

          if (length(dirCheck$missing) > 0){
            message0=paste("<font color='red'><b>",
                           "compare one of",
                           paste(dirCheck$missing$group1,collapse=", "),
                           "with",
                           paste(dirCheck$missing$group2,collapse=", "),
                           "</b></font>")
          } else {
            print(w[dirCheck$selected,,drop=F])
            shortDir=findDirection(w[dirCheck$selected,,drop=F])
            longDir=rep(0,n)
            longDir[dirCheck$selected]=shortDir

            customDirection$selected=dirCheck$selected
            customDirection$direction=shortDir

            # need to display how much information looked and the selected direction gives

            # Need the squares of the components. Think of a case where two eigenvalues are equal. Then
            # a 45 degree vector in their shared plane will explain the same variation as either. This, normalised
            # has components 1/sqrt(2) for each, so need to square to get the 50:50 addition.
            # In general, this vector must always explain less variance than the largest displayed eigenvector. Note
            # also that adding extra components will reduce the amount of variance explained (the first two eignvectors
            # define a space that contains the most variance) but may return loadings that are more biologically relevant.

            message0=paste("<b>",
                           sprintf("%5.1f%% variance explained",
                                   sum(longDir^2*percentExplained)),
                           "</b>")
          }



          # # ... now report the selected directions
          # pcsSelected=unique(unlist(dirDefined[,c("i","j")]))
          # percentSelected=percentExplained[pcsSelected]
          #
          # # ... and then create the message
          # explainedSummary=paste(
          #   sprintf("%5.1f%%",sum(percentSelected)),
          #   "=",
          #   paste(sprintf("%5.1f%% [%d]",percentSelected,pcsSelected),
          #         collapse=" + "))

          message=paste("<b>defined for views",andList(ddply(dirDefined,
                                                             ~i+j,
                                                             function(w){c(pair=sprintf("(%d vs %d)",w$i,w$j))})$pair),
                        "</b><br>",
                        message0)
        } else {
          message="no directions defined"
        }

        list(actionButton(ns("dir.undo.point"),"undo"),
             actionButton(ns("dir.clear.points"),"reset"),
             HTML("<br>"),
             HTML(message))
      } else {
        NULL
      }
    })




    # ....................................................................................................
    # development

    output[[ns("testing")]]=renderText({

      if (guard(input,ns,"transformation")){

        x=pcaData1()
        x.range=range(x)
        #     save(x,file="x.rda")

        ep=as.integer(input[[ns("end.point")]])

        if (input[[ns("display.type")]] == "project"){
          projectionText=fpaste("\nprojected into %s PCA components",endPoints()[as.integer(input[[ns("projection.end.point")]])])
        } else {
          projectionText=""
        }

        fpaste(c("endpoint %s %s",
                 "x-axis: PC%d",
                 "y-axis: PC%d\n",
                 "data range: %6.4f, %6.4f"),
               endPoints()[ep],projectionText,
               xyDir()[1],xyDir()[2],
               x.range[1],x.range[2])


      } else {
        NULL
      } # guard


    })

    # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    # observations

    observeEvent(input[[ns("explore.direction.title.button")]],{

      inputString=input[[ns("explore.direction.title.text")]]

      if (guard(input,ns,"explore.direction.type")){

        directionType=input[[ns("explore.direction.type")]]

        if (directionType != "custom"){

          if (directionType == "x"){
            pcDir=xyDir()[1]
          } else {
            pcDir=xyDir()[2]
          }

          x=pcaObj()$rotation[,pcDir,drop=T]

          if (inputString == ""){
            inputString=sprintf("PCA direction (PC %d)",pcDir)
          } else {
            inputString=sprintf("%s (PC %d)",inputString,pcDir)
          }

          entry=goViewerInput(x=x,
                              name=inputString,
                              annotation = "SYMBOL",
                              type="ordered")

          showNotification(sprintf("direction saved as '%s'",
                                   inputString),
                           type="message",duration=1.5)

          cx$selections[[length(cx$selections)+1]]=entry
        } else {
          # custom direction

          if (any(is.na(customDirection$selected))){
            showNotification("custom direction not yet defined",
                             type="error",duration=2.0)
          } else {

            x0=pcaObj()$rotation[,customDirection$selected,drop=F] %*% customDirection$direction
            x=as.vector(x0)
            names(x)=rownames(x0)

            directionText=paste(customDirection$selected,collapse=", ")

            if (inputString == ""){
              description=sprintf("custom direction (PCs %s)",directionText)
            } else {
              description=sprintf("%s (PCs %s)",inputString,directionText)
            }

            entry=goViewerInput(x=x,
                                name=description,
                                annotation = "SYMBOL",
                                type="ordered")

            showNotification(sprintf("custom direction saved at '%s'",
                                     description),
                             type="message",duration=1.5)

            cx$selections[[length(cx$selections)+1]]=entry

          }

        }
      }
    }
    )


    # ....................................................................................................
    # output

    # local supporting object

    pcaObj=reactive({
      .pcaData1=pcaData1()
      if (!is.null(.pcaData1)){
        prcomp(.pcaData1)
      } else {
        NULL
      }
    })

    output[[ns("PCA.plot")]]=renderPlot({

      ctl=controls()
      .pcaObj=pcaObj()
      .pcaData2=pcaData2()

      if (guard(input,ns,"reflect.axis") & !is.null(.pcaObj) & !is.null(.pcaData2)){

        reflection=input[[ns("reflect.axis")]]


        # check to see if points have been selected to define custom directions in the PCA plot:
        # if so, interrogate their values to find the appropriate values to plot on this PCA view.
        # By default, nothing is added.

        if (pointSelectionEnabled()){
          additionalPoints=presentPcaPointSelections(
            ctl$xy.dirs,
            readPcaPointSelections(cx$pca.selected.points)$all
          )
        } else {
          additionalPoints=NULL
        }

        projectedPcaScoresPlot(.pcaObj,
                               .pcaData2,
                               pcs=ctl$xy.dirs,
                               size=ctl$point.size,
                               reflect.x=grepl("x",reflection),
                               reflect.y=grepl("y",reflection),
                               axes=ctl$include.origin,
                               colourDictionary=cx$active.colour.legend,
                               additionalPoints=additionalPoints)
      } else {
        textMessageAsPlot("wait ...")
      }

    })

    output[[ns("info.plot")]]=renderPlot({

      plot(pcaObj(),
           main="total variance explained")
    })



    output[[ns("pca.info.text")]]=renderPrint({
      varianceExplainedSummary(pcaObj(),
                               k=as.integer(input[[ns("max.k")]]))
    })

  }


}
