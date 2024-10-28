\name{Classplot}
\alias{Classplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classplot
}
\description{
Allows to plot one time series or feauture with a classification as a labeled scatter plot with a line.
The colors are the labels defined by the classification.
}
\usage{
Classplot(X, Y, Cls, Plotter,Names = NULL, na.rm = FALSE, 

xlab = "X", ylab = "Y", main = "Class Plot", Colors = NULL,

Size = 8,PointBorderCol="black",

LineColor = NULL, LineWidth = 1, LineType  = NULL, 

Showgrid  = TRUE, pch,  AnnotateIt = FALSE, SaveIt = FALSE, 

Nudge_x_Names = 0, Nudge_y_Names = 0, Legend = "", SmallClassesOnTop = TRUE,

\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
[1:n] numeric vector or time
}
  \item{Y}{
[1:n] numeric vector of feature
}
  \item{Cls}{
[1:n] numeric vector of k classes, if not set per default every point is in first class
}
  \item{Names}{
[1:n] character vector of k classes, if not set per default Cls is used, if set, names the legend and the points
}
  \item{na.rm}{Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE}
  
  \item{xlab}{
Optional, string for xlabel
}
  \item{ylab}{
Optional, string for ylabel
}
  \item{main}{
Optional, string for title of plot
}
  \item{Colors}{
Optional, [1;k] string defining the k colors, one per class
}
  \item{AnnotateIt}{
Optional, in case of \code{Plotter==ggplot} and given \code{Names} annotates each point if TRUE
}
  \item{Size}{
Optional, size of points, beware: default is appropriate for  "\code{plotly}", or "\code{native}" but should smaller for "\code{ggplot}"
}
  \item{PointBorderCol}{
Optional, string, color of the dot outline for "\code{plotly}" for "\code{ggplot}". If \code{FALSE} and \code{Plotter="ggplot"} or \code{Plotter="plotly"}, no borders for points which is useful if many points overlap.
}

  \item{LineColor}{
Optional, name of color, in plotly then all points are connected by a curve, 

in ggplot2 all points of one class ae connected by a curve of the color the class
}
  \item{LineWidth}{
Optional, number defining the width of the curve (plotly only)
}
  \item{LineType}{
Optional, string defining the type of the curve in plotly only, "\code{dot}", "\code{dash}", "\code{-}"

for ggplot2: just set =1 here and then the curve is plotted
}
  \item{Showgrid}{
Optional, boolean (plotly only)
}
  \item{Plotter}{
Optional, either "\code{ggplot}" (default if \code{Names} given), "\code{plotly}" (default if no \code{Names} given), or "\code{native}"
}
\item{pch}{ [1:n] numeric vector of length n of the cases of Cls for the k classes. It defines the symbols to use, for native \code{Plotter} or \code{ggplot}, usally k can be in a range from zero to 25}
  \item{SaveIt}{
Optional, boolean, if true saves plot as html (plotly) or png (ggplot2)
}
  \item{Nudge_x_Names}{
Optional, numerical scalar, for \code{Plotter} "\code{ggplot}" only, if \code{Names} are set, moves them consistently respective to x-axis within units of x-axis
}
  \item{Nudge_y_Names}{
Optional, numerical scalar, for \code{Plotter} "\code{ggplot}" only, if \code{Names} are set, moves them consistently respective to y-axis within units of y-axis
}
  \item{SmallClassesOnTop}{
Optional, boolean, decide if small classes should be plotted on top for visibility (default setting) or not.
}
  \item{Legend}{
Optional, if argument is not missing, character string defining the title of the legend which automatically enables the legend
}

\item{\dots}{
Further arguments for \code{ggplot2::ggplot},or  \code{plotly::plot_ly}, or \code{\link{plot}} (except "\code{pch}"" and "\code{type}") depending on \code{Plotter}
}
}

\details{
The mapping of colors to the labels of \code{Cls} is consecutive, i.e., the label with the smallest value in \code{Cls} gets the first color in \code{Colors}. The Colors are plotted in order from label with the highest number of points to the label with the lowest number of points beeing on top.

Default is "\code{plotly}" if \code{Names} are \code{NULL}. However,  ggplot2 is preferable in case that \code{Names} parameter is used because overlapping text labels are avoided. In that case  the default is "\code{ggplot}". Note that ggplot2 options are currently slightly restricted. 

For example, the function is usefull to see if temporal clustering has time dependent variations and for Hidden Markov Models (see Mthrun/RHmm on GitHub).
}
\value{
plotly object or ggplot2 objected depending on \code{Plotter}
}

\author{
Michael Thrun
}

\seealso{
\code{\link{DualaxisClassplot}}
}
\examples{
data(Lsun3D)
Classplot(Lsun3D$Data[,1],Lsun3D$Data[,2],Lsun3D$Cls)

#ggplot 2 with different symbols
  Classplot(
    Lsun3D$Data[, 1],
    Lsun3D$Data[, 2],
    Lsun3D$Cls,
    Plotter = "ggplot2",
    Size = 3,
    pch = Lsun3D$Cls + 5
  )

#plotly with line
data(Lsun3D)
Classplot(Lsun3D$Data[,1],Lsun3D$Data[,2],Lsun3D$Cls,
LineType="-",LineColor = "green")
\donttest{
#ggplot2 with annotations
data(Lsun3D)
ind=sample(1:nrow(Lsun3D$Data),20)
Classplot(Lsun3D$Data[ind,1],Lsun3D$Data[ind,2],Lsun3D$Cls[ind],
Names = rownames(Lsun3D$Data)[ind],Size =1,
Plotter = "ggplot2",AnnotateIt = TRUE)
}

\donttest{
#ggplot2 with labels and legend per class
data(Lsun3D)
Classplot(Lsun3D$Data[,1],Lsun3D$Data[,2],Lsun3D$Cls,
Names = paste0("C",Lsun3D$Cls),Size =2,Legend ="Classes")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Classplot}% use one of  RShowDoc("KEYWORDS")
\keyword{dotplot}% use one of  RShowDoc("KEYWORDS")