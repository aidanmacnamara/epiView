% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GoViewer.r
\name{goViewer}
\alias{goViewer}
\title{top level GO viewer wiget}
\usage{
goViewer(id, input, output, session = NULL, ui = T, cx = NULL)
}
\arguments{
\item{id}{prefix for all shiny names}

\item{input}{shiney input object}

\item{output}{shiny output object}

\item{session}{shiny ooutput session}

\item{ui}{true selects UI code, false server}

\item{x.all}{a contains a list of goViewerInput objects like \link{P2} (assumed reactive)}
}
\description{
Used as part of a demo shiny app
}
