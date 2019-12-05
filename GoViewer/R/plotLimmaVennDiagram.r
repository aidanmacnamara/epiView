#' uses Lima to plot a graphically Venn diagram
#'
#' Makes a simple Venn diagram using \[limma]{vennDiagram} from the Limma package.
#'
#' @param sets a list of vectors of gene names. The name of each list entry gives
#'    gives the set name and the vector entries the set occupancy
#' @param allGenes a character vector. By default the total universe is simply the union of all of
#'    sets. This option defines an alternative universe which may be larger or
#'    smaller than the set of observed genes.
#'
#' @return produces a graph onto the screen
#'
#' @import limma
#' @export
#'
#' @examples
#' plotLimmaVennDiagram(list(A=c("A","B"),
#'                           B=c("B","C","D"),
#'                           C=c("A","E")))
#'
#' plotLimmaVennDiagram(list(A=c("A","B"),
#'                           B=c("B","C","D"),
#'                           C=c("A","E")),
#'                           allGenes=LETTERS)
#'
#' plotLimmaVennDiagram(list(A=c("A","B"),
#'                           B=c("B","C","D"),
#'                           C=c("A","E")),
#'                      allGenes=c("A","B","C"))

plotLimmaVennDiagram=function(sets,allGenes=NULL){

  library(limma)

  if (length(sets) > 0){
    if (is.null(allGenes)){
      allGenes=unique(unlist(sets))
    }

    mask=rep(0,length(allGenes))
    names(mask)=allGenes

    members=list()

    for (set in names(sets)){
      mask0=mask
      mask0[intersect(sets[[set]],
                      allGenes)]=1
      members[[set]]=mask0
    }

    vennDiagram(as.data.frame(members))
  } else {
    textMessageAsPlot("no\nselections\nspecified")
  }


}


