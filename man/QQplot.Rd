\name{QQplot}
\alias{QQplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QQplot with a Linear Fit
}
\description{
Qantile-quantile plot with a linear fit
}
\usage{
QQplot(X,Y,Type=8,NoQuantiles=10000,xlab, ylab,col="red",main='',
lwd=3,pch=20,subplot=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{[1:n] numerical vector,
First Feature}
  \item{Y}{
1:n] numerical vector, Second Feature to compare first feature with}
  \item{Type}{
  an integer between 1 and 9 selecting one of the nine quantile algorithms detailed in \code{quantile }
  }
  \item{NoQuantiles}{
  number of quantiles used in QQ-plot, if number is low and the data has outliers, there may be empty space visible in the plot}
  
  \item{xlab}{
  x label, see \code{plot}
...}
  \item{ylab}{
    y label, see \code{plot}
}
  \item{col}{
    color of line, see \code{plot}}
  \item{main}{
  title of plot, see \code{plot}
  }
    \item{lwd}{
  line width of plot, see \code{plot}
  }
    \item{pch}{
  type of point, see \code{plot}
  }
    \item{subplot}{
    FALSE: par is set specifically, TRUE: assumption is the usage as a subfigure, par has to be set by the user, no checks are performed, labels have to be set by the user
  }
  \item{\dots}{
other parameters for \code{qqplot}
}
}
\details{
Output is the evaluation of a linear (regression) fit of \code{lm} called 'line' and a quantile quantile plot (QQplot). Per default 10.000 quantiles are chosen, but in the case of very large data vectors one can reduce the quantiles for faster computation.
The 100 percentiles used for the regression line are of darker blue than the quantiles chosen by the user.
}
\value{
List with

\item{Quantiles}{[1:NoQuantiles,1:2] quantiles in y and y}
\item{Residuals}{Output of the Regression with \code{residuals.lm(line)}}
\item{Summary}{Output of the Regression with \code{summaryline)}}
\item{Anova}{Output of the Regression with \code{anova(line)}}
}
\references{
Michael, J. R.: The stabilized probability plot, Biometrika, Vol. 70(1), pp. 11-17, 1983.
}
\author{
Michael Thrun
}
\keyword{qqplot}% use one of  RShowDoc("KEYWORDS")
\examples{
data(MTY)
NormalDistribution=rnorm(50000)
QQplot(NormalDistribution,MTY)
}
