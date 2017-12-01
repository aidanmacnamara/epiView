#' returns a list of all the available gene lists for each mark
#'
#' Designed to be used with \link{listIntersect}, this returns a
#' list of all the genes completely recorded (no missing cell lines)
#' as a list.
#'
#' @param x a list containing data for each mark type. The data entry
#'    is assumed to be the $res component of each slot.
#'
#' @return a list. For each slot in the input list, names of fully
#'    represented genes in the slot $res component are listed
#' @export

availableGenesByMarks=function(x){

  lapply(x,
         function(u){
           z=apply(u$res,2,function(w){
             all(!is.na(w))
           })
           names(z)[z]
         }
  )

}
