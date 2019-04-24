#' prints an GoViewer data input source object
#'
#' @param x a GoView data input object
#'
#' @export

print.GoViewer.source=function(x){

  cat(sprintf(paste0("GoViewer input source object: '%s'\n",
                    "- created: %s\n",
                    "- dimension: %d genes x %d columns\n",
                    "- annotation: %s\n"),
              x$name,
              x$date,
              nrow(x$data),
              ncol(x$data),
              x$annotation)
  )

}
