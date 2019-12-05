#' security check for selection formulae
#'
#' A basic security check to allow simple expressions to specify regions to be safely evaluated from
#' user given input. Using the defaults, only expressions involving abs(), x and y, inequalities and
#' numbers are allowed. Unchecked, evaluated expressions can issue system's commands and hence
#' compromise a server.
#'
#' @param x string to be checked
#' @param allowedSymbols a list of allowed symbols. Symbols with a meaning in regular expressions should
#'    be escaped as in the defaults.
#'
#' @return a logical: true if the string is OK, false if it contains unaccepted symbols.
#' @export
#'
#' @examples
#'
#' securityFilterPass("abs(x) > 2e+00 & y < 1e-10")
#' securityFilterPass("abs(x) > 2e+00 & y < 1e-10 | call(x)")

securityFilterPass=function(x,
                            allowedSymbols=c("x","y","e","abs\\(","\\)","\\.",
                                             "=","<",">","&","\\|","\\+","-","\\d")){

  for (symbol in allowedSymbols){
    x=gsub(symbol," ",x)
  }

  !grepl("\\S",x)
}


