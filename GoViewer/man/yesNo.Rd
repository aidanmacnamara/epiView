% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yesNo.r
\name{yesNo}
\alias{yesNo}
\title{boolean yes-no formatter}
\usage{
yesNo(x, yesno = c("yes", "no"))
}
\arguments{
\item{x}{a boolean variable (or 0's and 1's)}

\item{yesno}{an optional length two vector containing a translation of true and false}
}
\description{
Prints yes or no for true or false. Supports vectors and alternative
choices of words.
}
\examples{
yesNo(c(T,T,F))
yesNo(c(T,T,F),yesno=c("ja","nein"))
}
