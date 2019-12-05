#' selects group datasources (bundles) to be presented for analysis
#'
#' Panel allows users to select bundles of data (e.g. experiments or projects) to include in their downstream analysis.
#' Any number of such bundles can be selected but as yet no choices within bundles is supported. The user can also decide
#' whether the full bundle description is carried forward as a prefix.
#'
#' This function selects from a list cx$bundles and writes its selection to cx$contrast.data. By default (i.e. initially),
#' the first item cx$bundles[[1]] is selected
#'
#' @param id an id for the Shiny widget
#' @param input a Shiny input list
#' @param output a Shiny output list
#' @param cx a Shiny reactive list used to communicate data in and out
#'
#' @export

selectBundles=function(id,input=NULL,output=NULL,ui=T,cx=NULL){

  ns=NS(id)

  if (ui){

    # ====================================================================================================
    # user interface

    fluidPage(
      br(),
      uiOutput(ns("selection.list")),
      hr(),
      fluidRow(column(2, actionButton(ns("apply.selection"),
                                         label="make selection")),
               column(10,checkboxInput(ns("full.prefix"),
                             label="prefix with full datasource name",
                             value=FALSE))),
      br(),
      HTML("Multiple collections of data sources can also be selected. The number of datasets in each is shown in brackets after their name")
    )

  } else {

    # ====================================================================================================
    # server side


    # ----------------------------------------------------------------------------------------------------
    # dynamic UI components

    # ... a helper object

    bundles=reactive({
      cx$bundles
    })

    # ... summary report for bundle

    bundleInfo=reactive({
      info=lapply(bundles(),function(w){
        sprintf("%s (%d)",
                w$bundle, length(w$sources)) }
      )

      if (!is.null(names(bundles())) && input[[ns("full.prefix")]]){
        info=paste(names(bundles()),": ",info)
      }
      info
    })

    # ... create the bundle selection widget

    output[[ns("selection.list")]]=renderUI({

      info=bundleInfo()

      choices=1:length(info)
      names(choices)=info

      selectInput(
        ns("selections.made"),
        label="select experiments or data sources",
        selected=1,
        choices=choices,
        multiple=T,
        selectize=T)
    })

    # ----------------------------------------------------------------------------------------------------
    # assign selected datasources to the sessions active data source list

    observeEvent(input[[ns("apply.selection")]],{
      selections=input[[ns("selections.made")]]
      if (length(selections) > 0){
        cx$contrast.data=flattenBundles(bundles()[as.numeric(selections)],
                                    usePrefix = !input[[ns("full.prefix")]])
      }
    })
  }
}


