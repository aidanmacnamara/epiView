#' counts the total number of observed and selected entries
#'
#' Counts the total number of observed and selected entries across
#' a list of selection objects. The aim is to give the size of the complement of
#' the Venn diagramm.
#'
#' @param Q a list of goViewer 'split' objects
#'
#' @return a list giving (i) the total number of genes observed and (ii) the number selected
#' @export
#'
#' @examples
#'
#' data(P2)
#' Q2=P2[1:3]
#'
#' demo=function(x,a=500,b=1000){
#'   x$type="split"
#'   x$data=0*x$data
#'   x$data[a:b]=x$data[a:b]+1
#'   x
#' }
#'
#' Q2=list(demo(P2[[1]],100,500),
#'         demo(P2[[2]],100,1000),
#'         demo(P2[[3]],5000,6000))
#'
#' countUniverse(Q2)

countUniverse=function(Q){

  save(Q,file="zz")

  all.data=lapply(Q,"[[","data")

  selected.data=lapply(all.data,function(w){w[w==1]})


  list(total=length(setListHelper(lapply(all.data,names),union)),
       selected=length(setListHelper(lapply(selected.data,names),union)))
}


