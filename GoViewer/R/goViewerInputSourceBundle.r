#' creates a bundle of input source objects
#'
#' Creates a bundle of input source objects corresponding to say a study or an experiment. These will
#' be grouped together in the data selectton menu
#'
#' @param x a list of input source objects or a single input source object
#' @param bundle a name for the bundle, for example the study number or the experiment. This will be used
#'    to identify it in the app.
#'
#' @return a GoViewer.source.bundle object
#' @export
#'
#' @examples
#' data(P2.source)
#' goViewerInputSourceBundle(P2.source,"project 2")
goViewerInputSourceBundle=function(x,
                                   bundle="[no name given]"){

  if (class(x)=="GoViewer.source"){
    x=list(x)
  } else if (class(x) == "list"){

    noFalseClass=sum(unlist(lapply(P2.source,class))!="GoViewer.source")

    if (noFalseClass > 0){
      stop("at least one member of the input list is not of class 'GoViewer.source'")
    }
  } else {
    stop("expecting a GoViewer.source object or a list of the like")
  }

  out=list(sources=x,
       bundle=bundle)
  class(out)="GoViewer.source.bundle"
  out

}
