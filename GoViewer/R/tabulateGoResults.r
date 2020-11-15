#' gives a tabular summary of the top k GOs
#'
#' @param goObject a defined GO generated by \link{makeGOobject}
#' @param runObj a list of GO tests results returned by \link{runGOtests}
#' @param k the number of GO's to be listed
#' @param ... additional arguments passed ot \link[topGO]{GenTable}
#'
#' @return a table of the top k GOs returned by \link[topGO]{GenTable}
#' @export
#'
#' @examples
#' #basic example
#'
#' P2a=P2[[1]]
#' P2a
#'
#' head(P2a$data)
#'
#' goObject=makeGOobject(P2a)
#' runObj=runGOtests(goObject,c("ks","fisher"))
#'
#' GenTable(goObject,runObj[[1]])
#' GenTable(goObject,a=runObj[[1]],b=runObj[[2]])
#'
#' # alternative front wrapper
#' tabulateGoResults(goObject,runObj)

tabulateGoResults=function(goObject,runObj,k=20,...){

  do.call("GenTable",
          c(list(goObject),
            runObj,
            list(topNodes=k),
            ...))

}