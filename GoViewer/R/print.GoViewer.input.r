#' prints an GoViewer data input object
#'
#' @param x a GoView data input object
#'
#' @export
#'
#' @examples
#' genes=runif(10)
#' names(genes)=paste("gene",LETTERS[1:10])
#'
#' goViewerInput(genes,
#'               name="test",
#'               annotation="test only")

print.GoViewer.input=function(x){

  cat(sprintf(paste0("GoViewer input object: '%s'\n",
                    "- created: %s\n",
                    "- length: %d\n",
                    "- mean/proportion: %8.6f\n",
                    "- type: %s\n",
                    "- annotation: %s\n"),
              x$name,
              x$date,
              length(x$data),
              mean(x$data),
              x$type,
              x$annotation)
  )

  if (x$type == "split"){
    cat(sprintf("- count: %d\n",sum(x$data)))
  }

}
