#' reads and processes a list of point selections fror directions in a PCA plot
#'
#' Clicks made by the user to select custom directions in a PCA plot are stored  as list of vectors containing in order
#' (i) the displayed x direction, (ii) the displayed y direction, (iii) the clicked x value and (iv) the clicked y value.
#' Since only two clicks are allowed for each PCA direction pair, the code first computes the last two selected values for each
#' input pair and then, if two values are found, computes the direction reported in the plane. If only one point is found in
#' a plane (and the direction is hence not defined), values of NA are returned for the direction. Values of NULL are returned
#' for both these values if no input points are found. See examples for a full illustration.
#'
#' Note that internally, we will always assume the indices representing the viewed principle components are in ascending order.
#' This is to avoid false distrinctions between a (1 vs 2) and a (2 vs 1) view say.
#'
#' @param z a list of vectors reporting input events are described above
#'
#' @return a list of two components: $all a data frame containing the last one or two points clicked for each input PCA pair and $dir
#'    a data frame of the corresponding directions, NA if not defined.
#'
#' @import plyr
#' @export
#'
#' @examples
#' readPcaPointSelections(list(
#'   c(1,2,10,15),
#'   c(1,2,11,16),
#'   c(1,2,11,17),
#'   c(1,3,15,20),
#'   c(1,3,10,22),
#'   c(1,4,44,18)))

readPcaPointSelections=function(z){

  require(plyr)

  if (length(z) > 0){

    x=ldply(z)
    n=nrow(x)
    colnames(x)=c("i","j","x","y")

    # check that the PCs are correctly ordered
    if (any(with(x,i>=j))){
      stop("PC indices should always be stored in ascending order")
    }

    x2 = ddply(x,~i+j,tail,2)

    list(all=x2,
         dir=ddply(x2,~i+j,
                   function(w){
                     if (nrow(w) > 1){
                       data.frame(i=w[1,1],
                                  j=w[1,2],
                                  x=diff(w[,3]),
                                  y=diff(w[,4]))
                     } else {
                       data.frame(i=w[1,1],
                                  j=w[1,2],
                                  x=NA,
                                  y=NA)
                     }
                   }))
  } else {
    list(all=NULL,dir=NULL)
  }
}
