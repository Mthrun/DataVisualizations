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
QQplot(X,Y,xlab, ylab,col="red",main='',...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{[1:n] numerical vector,
First Feature}
  \item{Y}{
1:n] numerical vector, Second Feature to compare first feature with}
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
  \item{\dots}{
other parameters for \code{qqplot}
}
}
\details{
Output is the evaluation of a linear fit of \code{lm} called 'line' and a quantile quantile plot (QQplot).
}
\value{
List with
\item{Residuals}{Output of\code{residuals.lm(line)}}
\item{Summary}{Output of\code{summaryline)}}
\item{Anova}{Output of\code{anova(line)}}
}
\references{
Michael, J. R.: The stabilized probability plot, Biometrika, Vol. 70(1), pp. 11-17, 1983.
}
\author{
Michael Thrun
}
\keyword{qqplot}% use one of  RShowDoc("KEYWORDS")

