#' plot message to screen as a plot
#'
#' Prints text to the screen as a base graphics plot. Note, no border is
#' printed.
#'
#' @param msg the message to be printed. Multiline messages are supported
#'
#' @export
#'
#' @examples
#' textMessageAsPlot("hello")
#' textMessageAsPlot(sprintf("hello world!\nthe current time\n is %s\nsee you again\nsoon",date()))

textMessageAsPlot=function(msg="wait ..."){
  plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
  text(0,0,msg)
}
