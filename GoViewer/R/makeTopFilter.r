#' makes a p-value filter
#'
#' Makes a p-value filter as required, for example, by the package topGO. Allows users to either pick out the top n genes or those with a
#' p-value less than a given p.
#' Alternatively, if can simply take a list of genes to be selected setthe corresponding entries to true. Note that the former option actually filters on rank. Importantly, this function returns a single value
#' function which 'remembers' its parameter settings. Implemented with lexical scoping, this is fully illustrated by the examples. Uses
#' AND logic: if two criteria are selected, the union of hits from both are true.
#'
#' @param n a rank above which genes are not selected
#' @param p a pvalue above which genes are not selected
#' @param labels a list of gene names to be marked as selected
#'
#' @return a function returning a boolean vector or a vector of p-values indicating whether or not each entry furfills the criteria with which
#'    it was defined
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' p=runif(10)
#' names(p)=LETTERS[1:10]
#' p
#' sort(p)
#'
#' # ... and a series of bespoke filters
#'
#' f1=makeTopFilter(n=3)
#' f2=makeTopFilter(p=0.4)
#' f3=makeTopFilter(n=3,p=0.4)
#' f4=makeTopFilter(labels=c("B","F","G"))
#'
#' # which we test and compare giving the required results
#'
#' data.frame(p,
#'            f1=f1(p),
#'            f2=f2(p),
#'            f3=f3(p),
#'            f4=f4(p))
#'
#' # advanced users will note that you can see the attached options (the environment) as follows:
#'
#' ls.str(environment(f1))
#' ls.str(environment(f2))
#' ls.str(environment(f3))
#' ls.str(environment(f4))


makeTopFilter=function(n=NULL,p=NULL,labels=NULL){

  # uses lexical scoping to return a filter with the required filters
  # note this code function returns a FUNCTION.

  function(x){


    result=rep(F,length(x))

    if (!is.null(n)){
      result[rank(x) <= n]=T
    }

    if (!is.null(p)){
      result[x <= p]=T
    }

    if (!is.null(labels)){
      result = names(x) %in% as.character(labels)
    }

    names(result)=names(x)

    result
  }
}




