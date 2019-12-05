#' selects the data for the contrast view
#'
#' Data is returned as three column dataset or NULL if no match can be found.
#'
#' @param D a data source object - see \link{goViewerInputsource} for details
#' @param x.var the response (e.g. a contrast) to be plotted on the x-axis
#' @param x.metric the metric of x.var to be plotted
#' @param y.var the response for the y-axis
#' @param y.metric the metric of the y-axis response
#'
#' @return a three column data frame with three columns: name, x and y where name is the
#'    gene or entity tested. If no match can be found, NULL is returned
#' @export
#'
#' @examples
#' data(P2.source)
#'
#' z=selectInputDataContrastView(P2.source,
#'                       "d1","l2fc",
#'                       "d1","p")
#' head(z)

selectInputDataContrastView=function(D,
                             x.var,x.metric,
                             y.var,y.metric){

  D.x=D[[x.var]]$data
  D.y=D[[y.var]]$data

  if (all(x.metric %in% colnames(D.x),y.metric %in% colnames(D.y))){
    x=D.x[c("name",x.metric)]
    y=D.x[c("name",y.metric)]

    out=merge(x,y,by="name")
    colnames(out)=c("name","x","y")
    out
  } else {
    print("metric not found")
    NULL
  }

}

