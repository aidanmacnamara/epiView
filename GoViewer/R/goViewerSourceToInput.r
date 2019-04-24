#' converts between a goViewer input source object and a simple imput object
#'
#' See \link{goViewerInput} and \link{goViewerInputSource} for details
#' @param source a input source object
#' @param value  the data value upon which an ordering is to be based
#'
#' @return a goViewer input object
#' @export
#'
#' @examples
#' # a single case
#'
#' data("P2.source")
#' names(P2.source)
#' x=P2.source[[1]]
#' x
#' head(x$data)
#'
#' goViewerSourceToInput(x)
#'
#' # or over a list
#'
#' lapply(P2.source,goViewerSourceToInput)

goViewerSourceToInput=function(source,value="p"){

  values=source$data[[value]]
  names(values)=source$data$name

  obj=list(name=sprintf("%s (%s)",source$name,value),
           date=date(),
           type="ordered",
           annotation=source$annotation,
           data=values)

  class(obj)="GoViewer.input"
  obj

}




