% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preferredChoice.r
\name{preferredChoice}
\alias{preferredChoice}
\title{gives position of first match in a preference list}
\usage{
preferredChoice(preference, possibilities, default = 1, value = F)
}
\arguments{
\item{preference}{a value to be matched}

\item{possibilities}{a vector of possible matches}

\item{default}{the index of the default value in x returned if there is no match}

\item{value}{if true, the value rather than index of the result is returned}
}
\value{
a index or entry from possibilities
}
\description{
Gives the position or value of the first match in a list of possibilities.
Default values can be provided for the case where there is not hit.
}
\examples{
preferredChoice("b",letters[1:4],99)
preferredChoice("x",letters[1:4],99)

preferredChoice("b",letters[1:4],1,value=T)
preferredChoice("x",letters[1:4],1,value=T)
}
