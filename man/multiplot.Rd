\name{multiplot}
\alias{multiplot}
\title{ Plot multiple ggplots}
\usage{multiplot(..., plotlist=NULL, file, cols=1, layout=NULL)}
\arguments{
\item{...}{ multiple ggplot objects to be plotted}
\item{plotlist}{Optional: list filled with ggplot objects to be plotted}
\item{file}{Currently unused}
\item{cols}{Number of columns in layout}
\item{layout}{A matrix specifying the layout. If present, 'cols' is ignored.}
}
\value{List with
}
\description{
  ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)  \cr
  If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.}
\author{
http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_\%28ggplot2\%29/
}
