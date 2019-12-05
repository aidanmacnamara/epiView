#' creates a simple -log10 p-value qqplot
#'
#' Constructs a simple log10 scale qq-plot for p-values. The log scale is preferred
#' to add focus to small highly significant points and since the angle made from
#' the origin is related to the corresponding FDR. The function also can be used to
#' annotate the top n p-values using the ggrepl package and thin out the plotting
#' of less signficant results.
#'
#' @param p a vector of p-values, possibly named
#' @param n if selected the number of smallest p-values to be annotated. Labels are taken from the names() of
#'   p if provided: otherwise they are numbered in order of increasing p-value. The default NA labels no points.
#' @param thin if selected, thinks out all entries after the thin[1]-th element, reporting only the thin[2]-th. The thined
#'   out elements are also printed in gray.
#' @param print.top prints the names of the top n genes if they are provided as names(p)
#' @param ... optional additional parameters to be passed to \link[ggrepl]{geom_text_repel}.
#'
#' @return a list:
#'    \itemize{
#'       \item data - a copy of the data used to create the plot
#'       \item graph -ggplot2 plot object
#'       }
#'
#' @export
#'
#' @examples


interactiveLog10QqPlot=function(p,n=NA,thin=NA,print.top=F,...){

  require(ggplot2)
  require(ggrepel)
  require(latex2exp)

  haveNames=!is.null(names(p))
  A=data.frame(p=p)

  if (haveNames){
    A$name=names(p)
  }

  # compute values
  N=nrow(A)
  A=A[order(A$p),,drop=F]
  A$q=(1:N)/N
  A$pp=-log10(A$p)
  A$qq=-log10(A$q)

  #construct basic plot

  if (any(is.na(thin))){
    A1=A
    g=ggplot(A1,aes(qq,pp))
    A1$colour="black"
  } else {

    if (thin[1] <= n){
      stop("the first element of thin is less than or equal to n")
    } else {
      A1=thinBottom(A,thin[1],thin[2],thined=T)
      A1$colour=ifelse(A1$thined,"gray","black")
      g=ggplot(A1,aes(qq,pp))
    }

  }

  # g=g+geom_abline(slope=1,intercept=0,colour="gray")
  g=g+geom_point(colour=A1$colour)+labs(x=TeX("$-\\log_{10} \\, p$ of expected value"),
                                        y=TeX("$-\\log_{10} \\, p$ of observed value"))


  # annotate top plots if required
  if (!is.na(n)){

    if (n > N){
      warning("n exceeds the number of points and will so be ignored")
    } else {
      A.top=A[1:n,]

      if (!haveNames){
        A.top$name=paste(1:n)
      } else {
        if (print.top){
          print(A.top$name)
        }
      }



      g=g+geom_text_repel(
        data=A.top,
        aes(qq,pp,
            label=name),
        ...)
    }
  }

  # return the result
  list(data=A,graph=g)
}


# x=runif(50)
# interactiveLog10QqPlot(x)$graph
#
# names(x)=sprintf("gene %02d",1:length(x))
# a=interactiveLog10QqPlot(x,n=10)
# a$graph
# head(a$data)


#' log10QqPlot(x,n=10)$graph
#' log10QqPlot(x,n=30)$graph
#' log10QqPlot(x,n=10,thin=c(20,5))$graph # example with thining


