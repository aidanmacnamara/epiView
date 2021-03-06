% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasetAsString.r
\name{datasetAsString}
\alias{datasetAsString}
\title{makes a text copy of a dataset}
\usage{
datasetAsString(x)
}
\arguments{
\item{x}{a data frame}
}
\value{
a string, a text representation of the above
}
\description{
Creates a text representation of a dataset in standard column format
separating columns by tabs and rows by new lines (R's \n). Note that
printed copies of the output are a text only representation of the string;
use cat() to correctly 'display' the tabs and new lines.
}
\examples{

a=data.frame(a=1:4,b=letters[1:4])
print(a)
cat(datasetAsString(a))
}
