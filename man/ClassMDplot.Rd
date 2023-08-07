\name{ClassMDplot}
\alias{ClassMDplot}
\title{
Class MDplot for Data w.r.t. all classes
}
\description{
  Creates a Mirrored-Density plot w.r.t. to each class of a numerical vector of data.
}
\details{
Further examples for the ClassMDplot can be found in \url{https://md-plot.readthedocs.io/en/latest/application/example_application.html}.

 The \code{Cls} vector is reordered from lowest to highest number.
  The \code{ClassNames} vector and \code{ColorSequence} vectors are matched by this ordering of \code{Cls}, i.e. the lowest number gets the first color or class name.
}
\usage{
ClassMDplot(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence,

                         ClassNames = NULL, PlotLegend = TRUE,Ordering = "Columnwise",
                         
                         main = 'MDplot for each Class',
                         
                         xlab = 'Classes', ylab = 'PDE of Data per Class',
                         
                         Fill = 'darkblue', MinimalAmoutOfData=40,
                         
                         MinimalAmoutOfUniqueData=12,SampleSize=1e+05,\dots)
}
\arguments{
  \item{Data}{
  [1:n] Vector of the data to be plotted
  }
  \item{Cls}{
  [1:n] Vector of class identifiers of k clusters one number is the label of one cluster
  }
  \item{ColorSequence}{
  Optional: [1:k] vector, The sequence of colors used, Default: DataVisualizations::DefaultColorSequence
  }
  \item{ClassNames}{
  Optional: [1:k] named numerical vector, The names of the classes. Default: Class 1 - Class k with k beeing the number of classes
  }
  \item{PlotLegend}{
  Optional: Add a legent to plot. Default: TRUE)
  }
  \item{Ordering}{
  Optional: Ordering of Classes, please see \code{MDplot} for details)
  }
  \item{main}{
  Optional: Title of the plot. Default: MDplot for each Class
  }
  \item{Fill}{
  Optional: [1:k] Vector with the colors, the MD's are to be colored with. If only one value is given, all MD's are colored in the same color.
  }
  \item{xlab}{
  Optional: Title of the x axis. Default: "Classes"
  }
  \item{ylab}{
  Optional: Title of the y axis. Default: "Data"
  }
      \item{MinimalAmoutOfData}{
    Optional: numeric value defining a threshold. Below this threshold no density estimation is performed and a Jitter plot with a median line is drawn. Please see \code{\link{MDplot}} for details.
  }
  
        \item{MinimalAmoutOfUniqueData}{
    Optional: numeric value defining a threshold. Below this threshold no density estimation and statistical testing is performed and a Jitter plot is drawn. Only Data Science experts should change this value after they understand how the density is estimated (see [Ultsch, 2005]).
  }
      \item{SampleSize}{
    Optional: numeric value defining a threshold. Above this thresholdclass-wise uniform sampling of finite cases is performed in order to shorten computation time. If required, \code{SampleSize=n} can be set to omit this procedure. 
  }
    \item{\dots}{
    Further arguments that are documented in \code{MDplot} except for \code{OnlyPlotOutput} which is always true.
  }
}

\value{
  A List of
  \item{ClassData}{The matrix [1:m,1:NoOfClasses] used to plot with the reordered Cls, rows are filled partly with NaN, m is the length of the number of data in largest class.}
  \item{ggobject}{The ggplot2 plot object}
  in mode \code{invisible}
}
\author{
  Michael Thrun, Felix Pape
}
\examples{
\dontshow{
# sampling is online done
# because otherwise the example takes too long
# in the CRAN check
data(ITS)
ind=sample(length(ITS),1000)

#clustering only for testing
Classification=c(rep(1,8805),rep(2,2389))
DataVisualizations::ClassMDplot(ITS[ind],Classification[ind])
}

\donttest{
data(ITS)

#shortcut for example if AdaptGauss not installed
Classification = kmeans(ITS, centers = 2)$cluster

#better approach
#please download package from cran
#model=AdaptGauss::AdaptGauss(ITS)
#Classification=AdaptGauss::ClassifyByDecisionBoundaries(ITS,

#DecisionBoundaries = AdaptGauss::BayesDecisionBoundaries(model$Means,model$SDs,model$Weights))
ClassNames=c(1,2)
names(ClassNames)=c("Insert name \n of Class 1","Insert name \n  of Class 2")
ClassMDplot(ITS,Classification,ClassNames = ClassNames)
}
}

\references{
Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), Paderborn, Germany, 2018. 
}

\note{
Function is still experimental because \code{ColorSequence} does not work yet, because we are unable to specify the colors in ggplot2. If someone knows a solution, please mail the maintainer of the package. Similar issue for \code{PlotLegend}.}

\seealso{
\url{https://md-plot.readthedocs.io/en/latest/application/example_application.html}

\code{\link{MDplot}}
\url{https://pypi.org/project/md-plot/}
}