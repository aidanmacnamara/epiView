#' presets selection tab
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param rv
#'
#' @return
#' @export

presets=function(id,input=NULL,output=NULL,ui=T,rv=NULL){

  ns=NS(id)

  if (ui){

    list(HTML("<h4>use the following presets to preselect the filter values</h4>"),
         wellPanel(
           presetItem("primary")
         ),
         wellPanel(
           presetItem("PMA")
         ),
         textInput(ns("genes"), label = "enter filter genes (optional)", placeholder = "genes deliminated by semicolons, spaces or commas"),
         actionButton(ns("apply"),label="apply"),
         actionButton(ns("clear"),label="clear"),
         HTML("<p>"),
         htmlOutput(ns("message"))
    )


  } else {


    # observeEvent(input[[ns("primary.apply")]],{
    #   rv$presets$primaryRank=10000
    # })

    presetItem("primary",input,output,ui=F,rv=rv)
    presetItem("PMA",input,output,ui=F,rv=rv)

    observeEvent(input[[ns("apply")]],{
      cat("adding genes to filter\n")
      rv$presets$prefilter=parseGeneNames(input[[ns("genes")]])
    })

    observeEvent(input[[ns("clear")]],{
      cat("removing genes from filter\n")
      rv$presets$prefilter=NULL
    })

    output[[ns("message")]]=renderText({
      filter=rv$presets$prefilter
      if (is.null(filter)){
         "no filter applied"
      } else {
        paste("<font color='red'>prefilter of",length(filter),"genes applied</font>")
      }
    })

  }
}
