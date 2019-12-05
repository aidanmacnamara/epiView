#' main analysis panel
#'
#' @param id
#' @param input
#' @param output
#' @param session
#' @param ui
#' @param x.in a GoViewerInput object requesting the current contrast
#'
#' @return
#' @export

mainGoAnalysisPanel=function(id,input,output,session=NULL,ui=T,x.in=NULL,cx=NULL){

  # tho check whether the UI is OK to trigger an analysis
  goStatus=reactiveValues(ready=F)

  ns=NS(id)

  goType=c("BP","CC","MF")
  goMethod=list("Fisher"="fisher","KS"="ks") # keys presented in UI


  if (ui){

    # ====================================================================================================
    # user interface

    fluidPage(

      HTML("<br>"),
      h2(textOutput(ns("title")),col="blue"),
      HTML("<hr>"),

      column(4,
             tabPanel("analysis",
                      uiOutput(ns("input.data")),
                      selectInput(ns("GoType"),
                                  label="GO type",
                                  choices=echoNames(goType)),
                      uiOutput(ns("GoMethodChoice")),
                      uiOutput(ns("minNodeSize")),
                      uiOutput(ns("selectionSizeSlider")),
                      actionButton(ns("run"),label="run"),
                      actionButton(ns("record"),label="record")
                      # checkboxInput(ns("test"),label="test mode")
             )),

      column(8,tabsetPanel(
        tabPanel("summary",
                 verbatimTextOutput(ns("display"))),
        tabPanel("plot",
                 goQQviewer(ns("qq"),input,output)),
        tabPanel("scoring",
                 uiOutput(ns("displayResultsChoice")),
                 tableOutput(ns("GOscoresTable"))),
        tabPanel("network",
                 HTML("panel currently in development"),
                 uiOutput(ns("displayResultsChoice2")),
                 fillPage(plotOutput(ns("networkPlot")))),
        tabPanel("download",
                 # HTML("Download the currently selected GO output running current options if necessary<p>"),
                 goResultsDownload(ns("download"))
                 #HTML("<br>downloads from this page current not support. To download your analysis, first store
                  #    it with the record button and then access if from the review aanalysis table of the
                   #   organiser view.")
                 )

      ))
    )

  } else {

    # ====================================================================================================
    # server side

    if (is.null(cx)){
      stop("server call requires value for `cx`")
    }


    # ----------------------------------------------------------------------------------------------------
    # dynamic UI components

    # do we still need this???

    goMethods=reactive({
      input[[ns("GoMethod")]]
    })

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # main left hand-side controls

    # first select the contrast for analyis

    output[[ns("input.data")]]=renderUI({

      if (!is.null(cx)){

        selectInput(ns("contrast.choice"),
                    label="contrast",
                    choices = invertList(lapply(all.x(),"[[","name")))
      } else {
        NULL
      }
    })

    # then the required GO method

    output[[ns("GoMethodChoice")]]=renderUI({

      obj=x()

      if (!is.null(obj)){
        if (obj$type == "ordered"){
          selectInput(ns("GoMethod"),
                      label="GO method",
                      choices=goMethod,
                      multiple = TRUE,
                      selected = goMethod[[1]])
        } else {
          list(selectInput(ns("GoMethod"),
                           label="GO method",
                           choices=goMethod[["Fisher"]],
                           multiple = TRUE,
                           selected = goMethod[["Fisher"]]),
               HTML("gene selections can only be used with Fisher's method<p>"))
        }
      }
    })

    # for ordered lists (e.g. loadings or p-values) , the number of terms to be declared significant.

    output[[ns("selectionSizeSlider")]]=renderUI({

      obj=x()

      if (!is.null(obj)){
        if (obj$type == "ordered"){
          sliderInput(ns("selectionSizeValue"),
                      label="size of enrichment set",
                      min=1,
                      max=noTerms(),value=noTerms()/4)
        } else {
          HTML(sprintf("using user defined selection of <font color='red'>%d</font> genes<p>",sum(obj$data)))
        }
      }
    })


    # the size of the smallest considered GO term

    output[[ns("minNodeSize")]]=renderUI({
      sliderInput(ns("minNodeSize"),
                  label="min node size",
                  min=1,max=100,value=20)
    })

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # controls in output panels

    # selecting required GO for tabular output

    output[[ns("displayResultsChoice")]]=renderUI({

      goTypes=input[[ns("GoMethod")]]

      if (length(goTypes) > 1){
        selectInput(ns("displayResults"),
                    label="displaying results for",
                    choices=goTypes)
      } else {
        NULL
      }

    })


    # selecting required GO for network plot
    # [almost] duplicating this for now

    output[[ns("displayResultsChoice2")]]=renderUI({

      goTypes=input[[ns("GoMethod")]]

      if (goStatus$ready){
        if (length(goTypes) > 1){
          selectInput(ns("displayResults2"),
                      label="displaying results for",
                      choices=goTypes)
        } else {
          NULL
        }
      }

    })

    # ----------------------------------------------------------------------------------------------------
    # general reactive values

    # returns the main input objcet as soon as it is selected. Before it is
    # avaliable, a NULL is returned


    x=reactive({

      if (guard(input,ns,"contrast.choice")){

        if (is.null(cx)){
          x.in()
        } else {
          selection=input[[ns("contrast.choice")]]
          all.x()[[selection]]
        }
      } else {
        NULL
      }
    })


    # set up sliders and options. Returns zero if x() is NULL

    noTerms=reactive({
      length(x()$data)
    })

    all.x=reactive({
      renameListNames(c(cx$selections,
                        lapply(cx$contrast.data,
                               goViewerSourceToInput)))
    })




    # ----------------------------------------------------------------------------------------------------
    # intermediate analysis reactive objects

    # holds main GO object

    goObject=reactive({

      input[[ns("run")]]

      goType=isolate(input[[ns("GoType")]])
      minNodeSize=isolate(input[[ns("minNodeSize")]])
      selectionSizeValue=isolate(input[[ns("selectionSizeValue")]])

      cat(sprintf("building GO for '%s' on %d hits and a min node size of %d\n",
                  goType,
                  minNodeSize,
                  selectionSizeValue))

      # testMode=input[[ns("test")]]
      testMode=F

      if (testMode){

        cat("recalling test ontology ...\n")
        test.goObject

      } else {

        cat("building ontology ...\n")

        result=withProgress(message="building ontology",
                            value=0.75,
                            {
                              makeGOobject(x(),
                                           goType=goType,
                                           minNodeSize=minNodeSize,
                                           nPositives=selectionSizeValue)
                            })

        test.goObject <<- result
        result
      }

    })

    # a GO results object derived from the main GO object

    goResultsObject=reactive({

      # testMode=input[[ns("test")]]
      testMode=F

      if (testMode){

        cat("recalling test scoring results ...\n")
        test.goResultsObject

      } else {
        cat("building scoring results object ...\n")

        result=withProgress(message="scoring ontology",
                            value=0.75,{
                              goObject()
                              runGOtests(goObject(),algorithm="classic",tests=input[[ns("GoMethod")]])
                            })

        test.goResultsObject <<- result

        result

      }
    })

    # ----------------------------------------------------------------------------------------------------
    # support functions

    # ----------------------------------------------------------------------------------------------------
    # general outputs

    chooseResultsOutput=function(){
      goMethods=input[[ns("GoMethod")]]

      if (length(goMethods) > 1){
        reorderValueToFront(goMethods,input[[ns("displayResults")]])
      } else {
        1
      }
    }

    # panel title

    output[[ns("title")]]=renderText({
      x()$name
    })

    # the main display summary panel

    output[[ns("display")]]=renderText({
      sprintf("waiting for input\n%s",date())
    })


    # the GO summary table

    output[[ns("GOscoresTable")]]=renderTable({

      if (goStatus$ready){
        tabulateGoResults(goObject(),
                          goResultsObject()[chooseResultsOutput()],
                          k=20)
      }
    })


    # download tab (NOT CURRENTLY ACTIVE)

    goResultsDownload(ns("download"),input,output,
                      ui=F,
                      goObject,
                      goResultsObject,
                      goStatus=goStatus)

    # the network summary plot

    output[[ns("networkPlot")]]=renderPlot({
      if (goStatus$ready){
        fcat("selection for network plot: %s",chooseResultsOutput())
        showSigOfNodes(goObject(),score(goResultsObject()[[chooseResultsOutput()]]),firstSigNodes = 6,useInfo = 'all')

      }
     })



    # progress dialogue

    observeEvent(input[[ns("run")]],{

      goStatus$ready=T
      output[[ns("display")]]=renderText({"building ontology"})
      output[[ns("display")]]=renderPrint({goObject()})

    })



    # the QQ-plot actions are in a separate function


    goQQviewer(ns("qq"),input,output,session,ui=F,
               goObject=NULL,
               goResultsObject=goResultsObject,
               goMethods=goMethods,
               goStatus=goStatus)

    # ....................................................................................................
    # saving analyses

    # >> first a helper function to call the modal view with or without an error message


    saveHelper=function(msg.error=NULL){

      requestString(id.value=ns("record.save.value"),
                    id.button=ns("record.save.button"),
                    msg="record analysis view as ...",
                    msg.error=msg.error)

    }


    # >> open modal window

    observeEvent(input[[ns("record")]],{


      if (invalidated(goResultsObject)){
        showNotification("no analysis yet to save",type="error")
      } else {
        showModal(saveHelper())
      }


    })

    # cx$results[["test"]]=list(goObject,goResultsObject,goMethods)

    observeEvent(input[[ns("record.save.button")]],{

        reportName=input[[ns("record.save.value")]]

        if (is.null(reportName) || nchar(reportName) == 0){
          showModal(saveHelper(msg.error="type a name for your selection or press cancel"))
        } else {

          ordered=x()$type == "ordered"
          if (ordered){
            noEnriched=isolate(input[[ns("selectionSizeValue")]])
          } else {
            noEnriched=NULL
          }

          cx$results[[reportName]]=list(name=reportName,
                                        created=date(),
                                        description=goAnalysisDescripton(
                                          contrast=x()$name,
                                          GOtype=isolate(input[[ns("GoType")]]),
                                          GOmethod =isolate(input[[ns("GoMethod")]]),
                                          minNodeSize=isolate(input[[ns("minNodeSize")]]),
                                          ordered,
                                          noEnriched=noEnriched),
                                        goObjectValue=goObject(),
                                        goResultsObjectValue=goResultsObject(),
                                        goMethodsValue=goMethods())
          print(cx$results)

          showNotification("analysis results recorded",type="message",duration=0.5)
          removeModal()
        }

    })


  }

}
