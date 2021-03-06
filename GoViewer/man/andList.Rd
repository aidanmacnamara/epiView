% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/andList.r
\name{andList}
\alias{andList}
\title{lists the elements of a vector as a comma delimited list with final 'and' or 'or'}
\usage{
andList(x, and = "and")
}
\arguments{
\item{x}{the vector to be listed. If necessary, this will be coerced to character strings.}

\item{and}{normally "and" but can be set to any other word between the final two words.}
}
\value{
the required string
}
\description{
Generates user readable text output in plain text format of the form "Monday, Tuesday and Wednesday".
}
\examples{
andList(c("red","black","blue"))
andList(1:10)
andList(1:20,"or")
}
