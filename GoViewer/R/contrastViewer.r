#' top level contrast viewer code
#'
#' @param id
#' @param input
#' @param output
#' @param session
#' @param ui
#' @param cx
#'
#' @return
#' @export
#'
#' @examples
contrastViewer=function(id,input,output,session=NULL,ui=T,cx=NULL){

  require(ggplot2)

  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # user interface

    fluidPage(
      fluidRow(HTML("<p>")),
      column(4,
             radioButtons(ns("add.mode"),
                          label="brush action",
                          choices=list("add"="add","remove"="remove")),

             wellPanel(
               fluidRow(
                 column(8,uiOutput(ns("x-var"))),
                 column(4, uiOutput(ns("x-metric")))
               ),

               fluidRow(
                 column(8,uiOutput(ns("y-var"))),
                 column(4,uiOutput(ns("y-metric")))
               )),

             uiOutput(ns("log.scales.choice"))

             # selectInput(ns("log.scales"),
             #             label="tranformation",
             #             choices = list("(x,y)"="_",
             #                            "(log x, y)"="x",
             #                            "(x, log y)"="y",
             #                            "(log x, log y)"="xy"))
      ),

      column(8,
             plotOutput(ns("main.plot"),
                        brush=brushOpts(id=ns("main.plot_brush")),
                        click=clickOpts(id=ns("main.plot_click"))),
             HTML("<p>"),
             verbatimTextOutput(ns("message")),


             wellPanel(

               fluidRow(

                 column(2,
                        actionButton(ns("record"),label="record")),

                 column(2,
                        actionButton(ns("clear"),label="clear")),

                 column(8,
                        verbatimTextOutput(ns("message2")))),

               fluidRow(column(12,
                               checkboxInput(ns("symbol.filter.needed"),label="filter by symbols")
               )),

               uiOutput(ns("symbol.filter.panel")),

               fluidRow(column(12,
                               checkboxInput(ns("data.filter.needed"),label="filter by data")
               )),
               uiOutput(ns("data.filter.panel"))
             )
      )

    )

  } else {

    # ====================================================================================================
    # server side

    if (is.null(cx)){
      stop("server side requires data for 'cx'")
    }




    # ----------------------------------------------------------------------------------------------------
    # dynamic UI components

    # input selector for x-axis dataset

    output[[ns("x-var")]]=renderUI({
      endpointNames=invertList(unlist(lapply(x(),"[[","name")))
      selectInput(ns("x-var"),
                  label="x variable",
                  choices = endpointNames)
    })

    # input selector for x-axis dataset

    output[[ns("y-var")]]=renderUI({
      endpointNames=invertList(unlist(lapply(x(),"[[","name")))
      selectInput(ns("y-var"),
                  label="y variable",
                  choices = endpointNames)
    })

    # select x and y metrics based on data types

    metricForVar=function(varName="x-var",metricName="x",preference="p"){
      x.var=input[[ns(varName)]]

      if (!is.null(x.var)){
        metricNames=numericColnames(x()[[x.var]]$data) ### current fudge
        names(metricNames)=metricNames
        selectInput(ns(metricName),
                    label="metric",
                    choices = metricNames,
                    selected=preferredChoice(preference,metricNames,value=T))
      } else {
        NULL
      }
    }

    output[[ns("x-metric")]]=renderUI({
      metricForVar("x-var","x-metric","l2fc")
    })

    output[[ns("y-metric")]]=renderUI({
      metricForVar("y-var","y-metric","p")
    })

    # selects transformation with metric dependent defaults

    output[[ns("log.scales.choice")]]=renderUI({

      isLoglike=function(x){
        x %in% c("p","padj")
      }


      if (guard(input,ns,"x-metric y-metric")){

        choice=1+isLoglike(input[[ns("x-metric")]])+2*isLoglike(input[[ns("y-metric")]])

        choices=list("(x,y)"="_",
                     "(log x, y)"="x",
                     "(x, log y)"="y",
                     "(log x, log y)"="xy")

        ichoice=choices[[choice]]


      } else {

        choices=list("waiting"="wait")
        ichoice=choices[[1]]
      }

      selectInput(ns("log.scales"),
                  label="tranformation",
                  choices = choices,
                  selected = ichoice)



    })


    # symbolic filter menu

    output[[ns("symbol.filter.panel")]]=renderUI({

      if (input[[ns("symbol.filter.needed")]]){
        fluidRow(column(2,
                        actionButton(ns("maths.filter.button"),label="filter")),
                 column(10,
                        textInput(ns("maths.filter.value"),label=NULL,value="",placeholder="filter expression")))
      }

    })

    # symbolic filter menu

    output[[ns("data.filter.panel")]]=renderUI({

      if (input[[ns("data.filter.needed")]]){
        fluidRow(column(12,verbatimTextOutput(ns("data.filter.selection"))))
      }
    })

    output[[ns("data.filter.panel")]]=renderUI({

      if (input[[ns("data.filter.needed")]]){
        list(fluidRow(column(6,
                             selectInput(ns("data.filter.action"),
                                         label="action",
                                         choices = list("set to selected (recall)"="set",
                                                        "add selected"="add",
                                                        "remove selected"="remove",
                                                        "filter by selected"="filter",
                                                        "filter by not-selected"="antifilter"),
                                         selected="add"))),
             fluidRow(column(12,
                             uiOutput(ns("data.filter.selector")))),
             fluidRow(column(4,
                             actionButton(ns("data.filter.button"),label="apply filter"))))
      }

    })


    output[[ns("data.filter.selector")]]=renderUI({

      namesOfSets=names(availableSets())

      if (length(namesOfSets) > 0){
        selectInput(ns("data.filter.selection"),
                    label="select sets",
                    choices=invertList(namesOfSets),
                    multiple = FALSE,
                    selected = 1:length(namesOfSets))
      } else {
        HTML("<p><b>no selections defined</b> Save a selection to use as a filter for this screen</p>")
      }
    })

    observeEvent(input[[ns("data.filter.button")]],{

      namesOfSets=names(availableSets())

      selection=namesOfSets[as.numeric(input[[ns("data.filter.selection")]])]
      filterAction=input[[ns("data.filter.action")]]

      selectedGenes=availableSets()[[selection]]
      currentSelection=activeDataView()$data
      currentGenes=subset(currentSelection,selected==TRUE)$name

      if (filterAction == "set"){
        events$actions=list(list(action="add",names=selectedGenes))

      } else if (filterAction == "add"){
        n=length(events$actions)
        events$actions[[n+1]]=list(action="add",names=selectedGenes)

      } else if (filterAction == "remove"){
        n=length(events$actions)
        events$actions[[n+1]]=list(action="remove",names=selectedGenes)

      } else if (filterAction == "filter"){
        events$actions=list(list(action="add",names=intersect(currentGenes,selectedGenes)))

      } else if (filterAction == "antifilter"){
        events$actions=list(list(action="add",names=setdiff(currentGenes,selectedGenes)))

      } else {
        stop("filter action",filterAction,"not recognised")
      }

    })




    # ----------------------------------------------------------------------------------------------------
    # general reactive values and functions


    # a copy of  the full data
    x=reactive({
      cx$contrast.data
    })


    # a list of the avaliable gene selections
    availableSets=reactive({
      selections=cx$selections

      if (length(selections) > 0){ # note this different to the Venn diagram which requires > 1

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

    # summary object of data with selections indicated

    activeDataView=reactive({

      if (guard(input,ns,"x-var y-var x-metric y-metric")){

        x.var=input[[ns("x-var")]]
        y.var=input[[ns("y-var")]]
        x.metric=input[[ns("x-metric")]]
        y.metric=input[[ns("y-metric")]]

        if (all(c(x.var,y.var) %in% names(cx$contrast.data))){

          w=x()
          data=selectInputDataContrastView(x(),x.var,x.metric,y.var,y.metric)

          if (is.null(data)){
            return(NULL) # returns NULL no input view could be selected
          }

          # returns single FALSE if activePoints() is NULL
          data$selected= data$name %in% activePoints()

          list(x.var=x.var,
               y.var=y.var,
               x.metric=x.metric,
               y.metric=y.metric,
               x.var.name=w[[x.var]]$name,
               y.var.name=w[[y.var]]$name,
               data=data)
        } else {
          NULL # still waiting UI selection to sync with updated cx$contrast.data
        }
      } else {
        NULL # still awaiting selection from UI
      }


    })

    # ....................................................................................................
    # main data view

    output[[ns("main.plot")]]=renderPlot({
      w=activeDataView()

      if (!is.null(w)){

        trans=input[[ns("log.scales")]]

        if (trans == "wait"){

          textMessageAsPlot("wait ...")

        } else {

          g=ggplot(w$data,aes(x,y,colour=yesNo(selected)))
          g=g+scale_colour_manual(values=c("black","red"))
          g=g+geom_point(alpha=0.5)

          g=g+labs(x=with(w,sprintf("%s (%s)",x.var.name,x.metric)),
                   y=with(w,sprintf("%s (%s)",y.var.name,y.metric)),
                   colour="selected")



          if (grepl("x",trans)){
            g=g+scale_x_log10()
          }
          if (grepl("y",trans)){
            g=g+scale_y_log10()
          }
          g
        }
      } else {
        textMessageAsPlot("wait ...")
      }
    })


    # ....................................................................................................
    # brushing and point selection

    # active record of selected points

    events = reactiveValues(n=0,actions=list())

    activePoints=reactive({
      condenseAddActions(events$actions)
    })

    # handlers for (1) brushing events and (2) click selections

    captureBrushing=reactive({
      w=activeDataView()

      points=brushedPoints(as.data.frame(w$data),
                           brush=input[[ns("main.plot_brush")]],
                           xvar="x",
                           yvar="y")

      session$resetBrush(ns("main.plot_brush"))

      list(message=sprintf("%d points selected",nrow(points)),
           n=nrow(points),
           names=as.character(points$name),
           action=input[[ns("add.mode")]])
    })

    observeEvent(input[[ns("main.plot_brush")]],{
      n=events$n+1
      events$n=n
      events$actions[[n]]=captureBrushing()
    })

    # report actions to the screen

    output[[ns("message")]]=renderText({
      point=nearPoints(activeDataView()$data,
                       coordinfo = input[[ns("main.plot_click")]],
                       xvar="x",
                       yvar="y",
                       maxpoints=1)

      if (!is.null(point) && nrow(point)>0){
        with(point,
             sprintf("(%4.3g,%4.3g) %s",
                     x,y,name))
      } else {
        condenseAddActions(events$actions,last=T)
      }
    })

    # report number of selected points to the screen

    output[[ns("message2")]]=renderText({
      x=activePoints()
      sprintf("%d points selected",length(x))
    })

    # clear all selections

    observeEvent(input[[ns("clear")]],{
      events$actions=list()
    })

    # text based maths filter

    observeEvent(input[[ns("maths.filter.button")]],
                 {
                   inputString=input[[ns("maths.filter.value")]]

                   inputString=gsub("AND","&",inputString,ignore.case=T)
                   inputString=gsub("OR","|",inputString,ignore.case=T)

                   adv=activeDataView()

                   inputString=gsub(adv$x.metric,"x",inputString)
                   inputString=gsub(adv$y.metric,"y",inputString)


                   if (!securityFilterPass(inputString)){
                     showNotification("unexpected text detected input",type="error")
                   } else {

                     data=activeDataView()$data
                     print(head(data))
                     filter=applyRuleToDataview(data,
                                                rule=inputString,
                                                showError=T)
                     points=as.character(data[filter,"name"])

                     list(message=sprintf("%d points selected",length(points)),

                          n=length(points),
                          names=points,
                          action=input[[ns("add.mode")]])

                     n=events$n+1
                     events$n=n
                     events$actions[[n]]= list(message=sprintf("%d points selected",length(points)),
                                               n=length(points),
                                               names=points,
                                               action=input[[ns("add.mode")]])

                   }

                 })


    # ....................................................................................................
    # saving selections

    # >> first a helper function to call the modal view with or without an error message

    saveHelper=function(msg.error=NULL){

      requestString(id.value=ns("record.save.value"),
                    id.button=ns("record.save.button"),
                    msg="record selection as ...",
                    msg.error=msg.error)

    }


    # >> open modal window

    observeEvent(input[[ns("record")]],
                 showModal(saveHelper()))

    # >> respond to save button click

    observeEvent(input[[ns("record.save.button")]],{

      w=activeDataView()$data
      x=w$selected
      names(x)=w$name

      recordText=input[[ns("record.save.value")]]

      n=length(cx$selections)+1 # index of next addition

      if (is.null(recordText) || nchar(recordText) == 0){
        #       showNotification("Enter text to save",type="error")
        showModal(saveHelper(msg.error="type a name for your selection or press cancel"))
      } else {
        cx$selections[[n]]=
          goViewerInput(x,
                        name=recordText,
                        type="split")

        showNotification("selection recorded",type="message",duration=0.5)
        updateTextAreaInput(session,ns("record.text"),value="")
        removeModal()
      }
    })


  }

}
