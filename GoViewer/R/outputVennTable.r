#' produces a summary table with gene memberships
#'
#' takes a list of sets of genes and returns their union as a dataset with columns indicating whether
#' each given gene is a member of an original set. Inclusion is indicated as a binary or optionally
#' a user given text
#'
#' @param Q a list of gene sets
#' @param legend an optional argument giving the output value for false 'note in set' and true 'in set'
#'
#' @return a data frame with the union of gene names and columns indicating set membership
#' @export
#'
#' @examples
#' Q=list("one example"=1:3,
#'        "second example"=2:4,
#'        "last example"=4:10)
#'
#' outputVennTable(Q)
#' outputVennTable(Q,c("-","included"))


outputVennTable=function(Q,legend=NULL){

  out=data.frame(gene=unique(unlist(Q)))


  for (set in names(Q)){

    out[[set]] = out$gene %in% Q[[set]]

  }

  if (!is.null(legend)){

    out[,-1]=apply(out[,-1],
                   1:2,
                   function(w){
                     legend[w+1]
                   })
   }


  out

}



