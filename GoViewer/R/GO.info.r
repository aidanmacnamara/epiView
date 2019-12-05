#' retrieves information for one or more GO terms
#'
#' @param go a vector or one or more go terms for which information is to be displayed
#' @param terms a list of terms from the GO to be displayed
#'
#' @return a dataframe with rows for the requested ontologies and columns for the rows. The first column is the
#'    GO term ID.
#' @export
#'
#' @examples
#' GO.info(c("GO:0042254","GO:0006996","GO:0006364"),
#'         c("Ontology","Term"))
#'
#' # you can also get information by hand for any single GO
#'
#' library(GO.db)
#' GOTERM[["GO:0006364"]]
#'
#' ## and a list of available slots (arguments for terms)
#'
#' slotNames(GOTERM[["GO:0006364"]])

GO.info=function(go=c("GO:0006996","GO:0006997"),
                 terms=c("Term","Ontology")){

  require(GO.db)
  require(plyr)
  require(reshape)

  result=data.frame(id=go)

  result=ddply(result,
        ~id,
        function(w){
       goterm=GOTERM[[as.character(w$id)]]

          adply(terms,
                1,
                function(term){
                  c(term=term,value=slot(goterm,term))
                })

        })

 cast(result,id~term)
}



