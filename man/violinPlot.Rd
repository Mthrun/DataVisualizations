\name{violinPlot}
\alias{violinPlot}

\title{violinPlot}

\description{
violinPlot with PDE
}

\usage{
violinPlot(...,  grownage = 10, 

    add = FALSE, axes = TRUE, handlelog = NA, wd = NA,
    
    maxwidth = 0.8, names, 
    
    horizontal = FALSE, side = "no", 
    
    frame.plot = axes, border = NULL, innerborder = NA, at = NULL, 
    
    boxwex = 1, ylim = NULL, xlim = NULL, show.names = NA)
}

\arguments{
  \item{\dots}{data which to perform the violinPlot on. This data can consist of dataframes, vectors and/or formulas. For each formula, a dataset can be specified with data=[dataset], and a subset can be specified with subset=[subset]. If subset/data arguments are passed, but there are not enough subset/data arguments, they are reused. Additionally, na.action, drop.unused.levels and xlev can be passed to \code{\link{model.frame}} in the same way. Also, parameters for \code{\link{axis}} and \code{\link{title}} can be passed.}
  \item{grownage}{the width of a bean grows linearly with the count of points, until \code{grownage} is reached.}
  \item{add}{if true, do not start a new plot}
  \item{axes}{if false, no axes are drawn.}
  \item{handlelog}{if \code{handlelog} then all the calculations are done using a log-scale. By default this is determined using the log parameter.}
  \item{wd}{the linear transformation that determines the width of the beans. By default determined using \code{maxwidth}, and returned.}
  \item{maxwidth}{the maximum width of a bean.}
  \item{names}{a vector of names for the groups.}
  \item{horizontal}{if true, the violinPlot is horizontal}
  \item{side}{the side on which the beans are plot. Default is \code{"no"}, for symmetric beans. \code{"first"}, \code{"second"} and \code{"both"} are also supported.}
  \item{frame.plot}{if true, plots a frame.}
  \item{border}{the color for the border around a bean. \code{NULL} for \code{par("fg")}, \code{NA} for no border. If border is a vector, it specifies the color per bean, and colors are reused if necessary.}
  \item{innerborder}{a color (vector) for the border inside the bean(s). Especially useful if \code{side="both"}. Use \code{NA} for no inner border. Colors are reused if necessary. The inner border is drawn as the last step in the drawing process.}
  \item{at}{the positions at which a bean should be drawn.}
  \item{boxwex}{a scale factor applied to all beans. Compatible with \code{\link{boxplot}}.}
  \item{ylim}{the range to plot.}
  \item{xlim}{the range to plot the beans at.}
  \item{show.names}{if true, plots the names as axis labels}
}

\details{
  Most parameters are compatible with \code{\link{boxplot}} and
  \code{\link{stripchart}}. For compatibility, arguments with the
  name "formula" or "x" are used as data. However, data or formulas
  do not need to be named "x" or "formula". The function handles
  (combinations of) dataframes, vectors and/or formulas.
}

\value{
  \item{bw}{the bandwidth (bw) used.}
  \item{wd}{the bean width (wd) used.}
  \item{names}{a vector of names for the groups.}
}

\references{
  Kampstra, P. (2008) Beanplot: A Boxplot Alternative for Visual Comparison of Distributions.
  \emph{Journal of Statistical Software, Code Snippets}, \bold{28}(1), 1-9.
  URL \url{http://www.jstatsoft.org/v28/c01/}
}

\author{
  Original: Peter Kampstra <beanplot@kampstra.net>
  Reduced Functionality with better density Estimation: MT
}

\note{
  In case of more than 5000 values per bean, the autodetection of the log parameter is approximated. In such cases, use
  \code{log=""} or \code{log="y"}. Also, the what parameter can help to omit parts that are not useful in certain plots, 
  and take time to draw.
}

\seealso{
  \code{\link{boxplot}}, \code{\link{stripchart}}, \code{\link{density}},
  \code{\link{rug}}, \code{\link[vioplot]{vioplot}} in package \code{vioplot}
}

\examples{
#violinPlot(rnorm(22),rnorm(22),rnorm(22),main="Test!",rnorm(3))

}
\keyword{hplot}
