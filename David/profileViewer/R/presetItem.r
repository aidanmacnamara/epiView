#' Title
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param item
#' @param value
#' @param rv
#'
#' @return
#' @export

presetItem=function(id,input=NULL,output=NULL,ui=T,item=NULL,rv=rv){

  ns=NS(id)

  if (is.null(item)){
    item=id
  }

  if (ui){
    list(
      HTML("<font color='blue'>",id,"</font>"),
      numericInput(ns("value"),
                   label="treshold",
                   value=0.05),
      selectInput(ns("metric"),
                  label="level type",
                  choices=list("p-values","FDR","rank")),
      actionButton(ns("lower"),"lower"),
      actionButton(ns("upper"),"upper")
    )
  } else {

    observeEvent(input[[ns("lower")]],{
      rv$presets[[id]]=list(limit="lower",
                            type=as.character(input[[ns("metric")]]),
                            value=as.numeric(input[[ns("value")]]))
    })

    observeEvent(input[[ns("upper")]],{
      rv$presets[[id]]=list(limit="upper",
                            type=as.character(input[[ns("metric")]]),
                            value=as.numeric(input[[ns("value")]]))
    })

  }



}
