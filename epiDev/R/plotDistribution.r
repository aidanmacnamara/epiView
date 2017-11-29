#' plots data distributions
#'
#' Creates plots of data distributions by mark.
#'
#' @param x a list of the form of Aidan's global_choice()$tmp
#' @param n the number of samples to pull for each mark to
#'    create the distribution density plot
#'
#' @return a ggplot2 plotting object
#' @export

plotDistribution=function(x,n=1000){

  # unpack and thin out data

  a=rename(ldply(x,
               function(w){

                 if (is.array(w$res)){
                   y=sample(as.vector(w$res),n)
                 } else {
                   y=sample(unlist(w$res),n)
                 }

                 data.frame(value=y)
               }),
         c(".id"="mark"))

  # create a distribution plot

  g=ggplot(a,aes(value,fill=mark))
  g=g+facet_wrap(~mark,scale="free")
  g=g+geom_density(alpha=0.4)
  # g=g+scale_fill_brewer(palette="Dark2")
  g

}

