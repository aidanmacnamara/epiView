#' prints a GoViewer.source.bundle
#'
#' @param x a GoViewer.source.bundle object
#'
#' @export
#'
#' @examples
#'
#' data(P2.source)
#' goViewerInputSourceBundle(P2.source,"project 2")

print.GoViewer.source.bundle=function(x){

  fcat("
*** goViewerInputSource bundle '%s'\n
A bundle containing %d input source files:\n",
      x$bundle,
      length(x$sources))

print(x$sources)
}

