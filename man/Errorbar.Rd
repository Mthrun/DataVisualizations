\name{Errorbar}
\alias{Errorbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Errorbar
}
\description{
Plots error bars at Xvalue positions for one or more than one classes
}
\usage{
Errorbar(Xvalues, Ymatrix, Cls, ClassNames, ClassCols, ClassShape, MeanFun = median, SDfun, JitterPosition = 0.5, main = "Error bar plot", xlab, ylab, WhiskerWidth = 7, Whisker_lwd = 1, BW = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xvalues}{[1:m] Numerical vector, positions of error bars in on x-axis for the m variables}
  \item{Ymatrix}{[1:n,1:d] of n cases and d=m*k variables with for which the error-bar statistics defined by MeanFun and SDfun should be computed}
  \item{Cls}{Optional, [1:d] numerical vector of k classes for the d variables. Each class is one method that will be shown as distinctive set of error bars in the plot}
  \item{ClassNames}{Optional, [1:k] character vector of k methods}
  \item{ClassCols}{Optional, [1:k] character vector of k colors}
  \item{ClassShape}{Optional, [1:k] numerical vector of k shapes, see pch in \code{\link{Classplot}}
 for details}
  \item{MeanFun}{Optional, error bar statstic of mean points, default=median}
  \item{SDfun}{Optional, error bar statstic for the length of whiskers, default is the robust estimation of standard deviation}
  \item{JitterPosition}{Optional, how much in values of Xvalues should the error bars jitter around Xvalues to not overlap}
  \item{main}{Optional, title of plot}
  \item{xlab}{Optional, x-axis label}
  \item{ylab}{Optional, y-axis label}
  \item{WhiskerWidth}{Optional, scalar above zero defining the width of the end of the whiskers}
  \item{Whisker_lwd}{Optional, scalar obove zero defining the thickness of the whisker lines}
  \item{BW}{Optional, FALSE: usual ggplot2 background and style which is good for screen visualizations. Default: TRUE: theme_bw() is used which is more appropriate for publications}
}
\value{
List with
\item{ggobj }{The ggplot object of the Errorbar}
\item{Statistics }{[1:(d*k)1:4] data frame of statstics per class used for plotting}
}
\details{
 If k=1, e.g., one method is used, d=m and Cls=rep(1,m).  All vector [1:k] assume the occurance of the classes in Cls as ordered with increasing value.
 
 Statistics are provided in long table format with the column names Xvalues, Mean, SD and Method. 
 The method column specifies the names of the k classes.

}


\author{
Michael Thrun
}

\examples{
data('FundamentalData_Q1_2018')
Data=as.matrix(FundamentalData_Q1_2018$Data)
Cls = FundamentalData_Q1_2018$Cls
Class1Data = matrix(NA, nrow = nrow(Data), ncol = 2)
Class2Data = matrix(NA, nrow = nrow(Data), ncol = 2)
Class1Data[which(Cls==1), ] = Data[which(Cls==1), c("TotalAssets", "TotalLiabilities")]
Class2Data[which(Cls==2), ] = Data[which(Cls==2), c("TotalAssets", "TotalLiabilities")]
YMatrix = cbind(Class1Data, 
                Class2Data)

Errorbar(c("TotalRevenue","GrossProfit"), 
         YMatrix, c(1,1,2,2), 
         ClassNames=c("Class 1", "Class 2"), 
         main="Errorbar-plot Q1 2018 for total revenue and gross profit",
         xlab="GrossProfit/TotalRevenue", 
         ylab="Median +- std", 
         WhiskerWidth = 1)
}

\keyword{Errorbar}
\keyword{Errorbarplot}
