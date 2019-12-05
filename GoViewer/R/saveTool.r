#' widget to save and retrieve cx-list values
#'
#' A development code to save and copy values of the cx reactive array. Doing this by hand
#' since it appears to be difficult to overwrite directly using load even if you get the same
#' target environments. Data is stored as a file cx.rda.
#'
#' @param id widget's Shiny ID
#' @param input input list
#' @param output output list
#' @param ui TRUE (default) for UI calls, FALSE for server
#' @param cx the cx list
#'
#' @return
#' @export

saveTool=function(id,input,output=NULL,cx=NULL,ui=T){

  ns=NS(id)

  dumpName="cx.rda"

  if (ui){

    list(verbatimTextOutput(ns("summary")),
         actionButton(ns("record"),label="record cx"),
         actionButton(ns("recall"),label="recall cx")
    )

  } else {

    output[[ns("summary")]]=renderPrint({
      names(cx)
    })

    observeEvent(input[[ns("record")]],{
      cxx=cx
      save(cxx, file = dumpName)
      print("saved cx")
      showNotification("cx list recorded",type="message",duration=5)
    })

    observeEvent(input[[ns("recall")]],{
      load(dumpName)
      for (name in names(cxx)){
        cat("... copying",name,"\n")
        cx[[name]]=cxx[[name]]
      }
      showNotification("cx list recalled",type="message",duration=5)
    })

  }
}
