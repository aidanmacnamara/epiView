#' applies a logical test to a dataset
#'
#' Used to allow the formula-based selection of genes etc. from a shiney apply, evaluates an expression within
#' a data frame, typically returning a logical vector of the same length as the data frame as rows. No security check
#' is made to avoid kidnappping systems functions but this can be coded via \link{securityFilterPass}.
#'
#' @param x a input data frame on which the rule is to be applied
#' @param rule a string containing the expression to be evalated
#' @param showError if true, sends a Shiny show notification to the screen in case of error
#'
#' @return the output from the expression, typically a logical vector
#' @export
#'
#' @examples
##' # first a helper function
#'
#' demo.plot=function(A,z="r1"){
#'   A$col="black"
#'   A$col[A[[z]]]="red"
#'   print(A)
#'   plot(A[,1:2],col=A$col)
#' }
#'
#' # create some test data
#'
#' set.seed(1)
#' x=data.frame(x=rnorm(26),
#'              y=rnorm(26))
#' rownames(x)=LETTERS[1:26]
#'
#' # and some simple rules
#'
#' x$r1=applyRuleToDataview(x,"x > 0.5")
#' x$r2=applyRuleToDataview(x,"abs(x) > 0.5")
#' x$r3=applyRuleToDataview(x,"abs(y) > 0.5 | abs(y) > 0.5")
#' x$r4=applyRuleToDataview(x,"x > y")
#'
#' par(mfrow=c(2,2))
#' demo.plot(x,"r1")
#' demo.plot(x,"r2")
#' demo.plot(x,"r3")
#' demo.plot(x,"r4")
#' par(mfrow=c(1,1))
#'
#' # does not test for unexpected characters but will give
#' # null for erroneous expressions
#'
#' applyRuleToDataview(x,"a > y")
#' applyRuleToDataview(x,"a//s > y")

applyRuleToDataview=function(x,rule,showError=F){

  result=try(eval(parse(text=rule),
                  envir=x),
             silent=T)

  if (class(result) == "try-error"){
    if (showError){
      showNotification(sprintf("expression '%s' could not be understoond",rule))
    }
    NULL
  } else {
    result
  }


}
