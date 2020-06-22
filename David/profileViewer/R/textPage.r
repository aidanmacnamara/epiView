#' plots one or more lines of text as a graphical plot
#'
#' Plots a string of text as a graphical plot. This may be done as a
#' single line or multiple if new line characters are included. Lines
#' and text are horizontally and vertically centered and centre justified.
#'
#' @param x the text to be plotted
#'
#' @return
#' @export
#'
#' @examples
#' textPage()
#' textPage("hello")
#' textPage("hello\nand\nwelcome")

textPage=function(x=date()){

  plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
  text(0,0,x)

}

