\name{CrossTablePlot}
\alias{CrossTablePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CrossTablePlot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CrossTablePlot(Data, xbins = seq(0, 100, 5), ybins = seq(0, 100, 5), PlotIt = T, NormalizationFactor = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{xbins}{
%%     ~~Describe \code{xbins} here~~
}
  \item{ybins}{
%%     ~~Describe \code{ybins} here~~
}
  \item{PlotIt}{
%%     ~~Describe \code{PlotIt} here~~
}
  \item{NormalizationFactor}{
%%     ~~Describe \code{NormalizationFactor} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Data, xbins = seq(0, 100, 5), ybins = seq(0, 100, 5), 
    PlotIt = T, NormalizationFactor = 1) 
{
    width = abs(xbins[2] - xbins[1])
    aint = findInterval(Data[, 1], ybins, left.open = FALSE, 
        rightmost.closed = FALSE)
    bint = findInterval(Data[, 2], ybins, left.open = FALSE, 
        rightmost.closed = FALSE)
    u = unique(aint)
    leera = setdiff(1:length(xbins), u)
    u = unique(bint)
    leerb = setdiff(1:length(xbins), u)
    CrossTable = table(c(aint, leera, rep(1, length(leerb))), 
        c(bint, rep(1, length(leera)), leerb))
    frequency <- as.data.frame(CrossTable)
    frequency[, 1] = as.numeric(frequency[, 1])
    frequency[, 2] = as.numeric(frequency[, 2])
    for (i in 1:length(leera)) {
        frequency[frequency[, 1] == leera[i], 3] = 0
    }
    for (i in 1:length(leerb)) {
        frequency[frequency[, 2] == leerb[i], 3] = 0
    }
    frequency2D = diag(length(xbins)) * 0
    frequency2D[cbind(frequency[, 1], frequency[, 2])] = frequency[, 
        3]/NormalizationFactor
    if (PlotIt) {
        cols = DataVisualizations::HeatmapColors
        image(xbins, ybins, frequency2D, xlab = colnames(Data)[1], 
            ylab = colnames(Data)[2], col = cols)
        text(frequency[, 1] * width - width, frequency[, 2] * 
            width - width, labels = round(frequency[, 3]/NormalizationFactor, 
            0))
    }
    return(invisible(CrossTable/NormalizationFactor))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
