#' formats a Venn diagram table for nicer display
#'
#' A pretty formatter for \link{computeVennDiagram}. You may need to suppress any
#' quotes for printing.
#'
#' @param x a Venn diagram table generate by \link{computeVennDiagram}
#' @param checked symbol for included set, by default "x"
#'
#' @return a data frame showing an improved representation of x
#' @export
#'
#' @examples
#' #' Venn=computeVennDiagram(list(c(4,1,2,3,5,6,7),
#'                           c(1,2,3,5,6,7,9),
#'                           c(5,6,7,9,8)))
#' Venn
#'
#' formatVennDiagram(Venn)
#'
#' # alternative symbols are supported but may not
#' # always display as expected on all systems
#' formatVennDiagram(Venn,"?")

formatVennDiagram=function(x,checked="x"){


  apply(x,1:2,function(w){
    ifelse(w,checked,"")
  })

}


