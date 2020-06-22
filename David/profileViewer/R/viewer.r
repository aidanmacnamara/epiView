#' top level function to run the app
#' @param the data source
#' @export
#' @import shiny
#'
#' @examples
#' viewer()
viewer=function(source="v2"){

  require(shiny)

  cat("... loading data\n")
  if (source == "v1"){
    stop("original data source 'v1' no longer supported")
    data(profiles)
  } else {
    data(newProfiles)


    rv.THP1=reactiveValues(counts=subset(newProfiles$counts, cell_type %in% c("primary","THP-1")),
                           contrasts=list(
                             primary=newProfiles$results$c.prime,
                             PMA=newProfiles$results$c.THP1.pma,
                             inter=newProfiles$results$c.THP1.pma.prime,
                             final=newProfiles$results$c.THP1.pma.macro),
                           n.genes=nrow(newProfiles$results[[1]]),
                           guides=list(),
                           guides2=list(),
                           minmax=newProfiles$minmax,
                           raw=newProfiles$raw,
                           presets=list()
    )


    rv.U937=reactiveValues(counts=subset(newProfiles$counts, cell_type %in% c("primary","U937")),
                           contrasts=list(
                             primary=newProfiles$results$c.prime,
                             PMA=newProfiles$results$c.U937.pma,
                             inter=newProfiles$results$c.U937.pma.prime,
                             final=newProfiles$results$c.U937.pma.macro),
                           n.genes=nrow(newProfiles$results[[1]]),
                           guides=list(),
                           guides2=list(),
                           minmax=newProfiles$minmax,
                           presets=list()
    )


  }
  cat("... data loaded\n")

  ui=fluidPage(
    tabsetPanel(
      tabPanel("THP1",explorer("THP1")),
      tabPanel("U937",explorer("U937")),
      tabPanel("gene viewer",geneViewer("genes")),
      tabPanel("notes","help",
               HTML(paste("<br><h3>Notes</h3>",
                          "<ul>",
                          "<li>This is an informal application to explore genes from a specific",
                          "experiment.</li>",
                          "<li>Note that the data displayed is the both for each cell line,",
                          "that is without filtering they will show the same data with no filtering",
                          "set. The only differences is the contrasts which are selected between the two",
                          "lines. To see the effect of this, click the top 5% option - the selected",
                          "genes will then different between lines.</li>",
                          "<li>Note that the primary and PMA genes are ranked on their how strongly their",
                          "p-values show a difference from zero whereas the parallel and final agreements",
                          "are based upon the minimum level at which they might be considered equivalent.",
                          "Proving that things are different or the same are statistically opposites and use",
                          "different techniques.</li>",
                          "</ul>",
                          sep="\n"))
      ),
      tabPanel("version",displayText("version"))
    )
  )

  cat("... ui defined\n")

  server=function(input,output,session){
    cat("- viewer server\n")
    explorer("THP1",input,output,session,ui=F,rv=rv.THP1)
    explorer("U937",input,output,session,ui=F,rv=rv.U937)
    geneViewer("genes",input,output,ui=F,rv=rv.THP1)
    displayText("version",input,output,ui=F,text=list(
      "input data"=newProfiles[c("created","origin")]))
  }

  cat("...server defined\n")


  shinyApp(ui=ui,server=server)


}

# viewer()
