% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outputVennTable.r
\name{outputVennTable}
\alias{outputVennTable}
\title{produces a summary table with gene memberships}
\usage{
outputVennTable(Q, legend = NULL)
}
\arguments{
\item{Q}{a list of gene sets}

\item{legend}{an optional argument giving the output value for false 'note in set' and true 'in set'}
}
\value{
a data frame with the union of gene names and columns indicating set membership
}
\description{
takes a list of sets of genes and returns their union as a dataset with columns indicating whether
each given gene is a member of an original set. Inclusion is indicated as a binary or optionally
a user given text
}
\examples{
Q=list("one example"=1:3,
       "second example"=2:4,
       "last example"=4:10)

outputVennTable(Q)
outputVennTable(Q,c("-","included"))
}
