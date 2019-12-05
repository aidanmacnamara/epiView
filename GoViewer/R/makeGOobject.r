#' makes a basic GO object against which enrichment tests can be run
#'
#' @param x a data object as created by goViewerInput
#' @param goType character, type of gene ontology -
#' @param minNodeSize the minimum node size
#' @param nPositives the number of positive terms to be selected
#'
#' @return
#' @export
#'
#' @examples
#'
#' # using our example dataset
#' data(P2)
#'
#' P2a=P2[[1]]
#' P2a
#'
#' head(P2a$data)
#'
#' goObject=makeGOobject(P2a)
#' results=runTest(goObject,algorithm="classic",statistic = "fisher")
#'
#' # displays results to screen
#' showSigOfNodes(goObject,score(results),firstSigNodes = 6,useInfo = 'all')

makeGOobject=function(x,
                      goType="BP",
                      minNodeSize=20,
                      nPositives=100,
                      ...){
  require(topGO)

    if (x$type == "ordered"){

      new("topGOdata",
          description=x$name,
          ontology=goType,
          allGenes=x$data,
          geneSelection=makeTopFilter(nPositives),
          nodeSize=minNodeSize,
          annot=annFUN.org,
          ID="SYMBOL",
          mapping="org.Hs.eg")

    } else if (x$type == "split") {

      new("topGOdata",
          description=x$name,
          ontology=goType,
          allGenes=as.factor(x$data+0),
          nodeSize=minNodeSize,
          annot=annFUN.org,
          ID="SYMBOL",
          mapping="org.Hs.eg")

    } else {
      stop("data type not understood")
    }


}



