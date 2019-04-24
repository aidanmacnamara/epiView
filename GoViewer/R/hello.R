#' hello world program
#'
#' Undergoes periodic revisions to test the version control systems
#'
#' @param thing a string containing what you want to greet.
#'
#' @return
#' @export
#'
#' @examples
#' hello()
#' hello("moon")
hello <- function(thing="world") {
  print(sprintf("Hello, %s!",thing))
}
