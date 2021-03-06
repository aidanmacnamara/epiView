% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addScore.r
\name{addScore}
\alias{addScore}
\title{equivalence based score to test proximity to zero}
\usage{
addScore(w, alpha = 0.1)
}
\arguments{
\item{w}{a table of DESeq2 like contrast results}

\item{alpha}{a TOST alpha level, the TOST will use a (1-2*alpha)% CI}
}
\value{
a copy of w with an additional columns zero.score
}
\description{
Returns the absolute value of a fold change estimate plus a penalty for the its uncertainly
based on a TOST equivalence framework.
}
\examples{
data("profiles")
head(addScore(profiles$contrasts$c.prime,0.05))
}
