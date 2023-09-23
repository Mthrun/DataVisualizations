\name{MDplot}
\alias{MDplot}
\title{
Mirrored Density plot (MD-plot)
}
\description{
This function creates a MD-plot for each variable of the data matrix. 
The MD-plot is a visualization for a boxplot-like shape of the PDF published in [Thrun et al., 2020] with the default ordering by shape. It is an improvement of violin or so-called bean plots and posses advantages in comparison to the conventional well-known box plot [Thrun et al., 2020]. 

A complete guide about the MDplot can be found in \url{https://md-plot.readthedocs.io/en/latest/index.html}.
}
\usage{
MDplot(Data, Names, Ordering='Default', Scaling="None",

Fill='darkblue', RobustGaussian=TRUE, GaussianColor='magenta',

Gaussian_lwd=1.5, BoxPlot=FALSE,BoxColor='darkred', 

MDscaling='width', LineColor='black', LineSize=0.01,

QuantityThreshold=50, UniqueValuesThreshold=12,

SampleSize=5e+05,SizeOfJitteredPoints=1,OnlyPlotOutput=TRUE,

main="MD-plot",ylab="Range of values in which PDE is estimated",

BW=FALSE,ForceNames=FALSE)
}

\arguments{
  \item{Data}{
    [1:n,1:d] Numerical Matrix containing the n cases of d variables. Each column is one variable. A data.frame is automatically transformed to a numerical matrix.}
  \item{Names}{
    Optional: [1:d] Names of the variables. If missing, the columnnames of data are used. If not missing, than the names can be cleaned or not (see \code{ForceNames}).
  }
      \item{Ordering}{
    Optional: string, either \code{Default}, \code{Columnwise} or \code{AsIs}, \code{Alphabetical}, \code{Average}, \code{Bimodal}, \code{Variance}  or \code{Statistics}. Please see details for explanation.
  }
      \item{Scaling}{
    Optional, Default is \code{None}, \code{Percentalize}, \code{CompleteRobust}, \code{Robust} or \code{Log}, Please see details for explanation.
  }

    \item{Fill}{
    Optional: String or Vector, which gives the color(s) with which MDs are to be filled with.
  }
    \item{RobustGaussian}{
    Optional: If TRUE: each MDplot of a variable is overlayed with a roubustly estimated unimodal Gaussian distribution in the range of this variable,
	if statistical testing does not yield a significant p.value. In this case the packages \pkg{moments}, \pkg{diptest} and \pkg{signal} are required.
  }
      \item{GaussianColor}{
    Optional: string, color of robustly estimated gaussian, only for \code{RobustGaussian=TRUE}.
  }
  \item{Gaussian_lwd}{
    Optional: numerical, line width of robustly estimated gaussian, only for \code{RobustGaussian=TRUE}.
  }
     \item{BoxPlot}{
    Optional: If TRUE: each MDplot is overlayed with a Box-Whisker Diagram.
  }
      \item{BoxColor}{
    Optional: string, color of Boxplot, only for \code{BoxPlot=TRUE}.
  }
    \item{MDscaling}{
    Optional: if "area", all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to  the number of observations. If "width" (default), all MDs have the same maximum width.
  }
     \item{LineColor}{
    Optional: string, color of line around the mirrored densities. \code{NA} disables this features which is usefull if ones wants to avoid vertical lines leading to outliers.
  }
    \item{LineSize}{
    Optional: numerical, linewidth of line around the mirrored densities.
  }
    \item{QuantityThreshold}{
    Optional: numeric value defining the threshold of the minimal amount of values in data. Below this threshold no density estimation is performed and a 1D scatter plot with jittered points is drawn. Only Data Science experts should change this value after they understand  how the density is estimated (see [Ultsch, 2005]).
  }
      \item{UniqueValuesThreshold}{
    Optional: numeric value defining the threshold of the minimal amount of unique values in data. Below this threshold no density estimation and statistical testing is performed and a 1D scatter plot with jittered points drawn. Only Data Science experts should change this value after they understand how the density is estimated (see [Ultsch, 2005]).
  }
      \item{SampleSize}{
    Optional: numeric value defining a threshold. Above this threshold uniform sampling of finite cases is performed in order to shorten computation time.If \pkg{rowr} is not installed, uniform sampling of all cases
    is performed. If required, \code{SampleSize=n} can be set to omit this procedure. 
  }
   \item{SizeOfJitteredPoints}{
	 Optional: scalar. If not enough unique values for density estimation are given, data points are jittered. This parameter defines the size of the points. 
  }
  \item{OnlyPlotOutput}{
   Optional: Default TRUE only a ggplot object is given back, if FALSE: Additinally, scaled data and ordering are the output of this function in a \code{list}.
  }
     \item{main}{
     string defining the (centered) title of the plot
     }
     \item{ylab}{
     string defining the y label, PDE= pareto density estimation (see [Ultsch, 2005])
     }
      \item{BW}{
      FALSE: usual ggplot2 background and style which is good for screen visualizations
      
      TRUE: theme_bw() is used which is more appropriate for publications
    }
    
    \item{ForceNames}{
      FALSE: Per Default column names are cleaned for propper plotting
      
      TRUE: forces to set the column names as given. Beware, this can result in plotting errors.
    }
}

\value{

In the default case of \code{OnlyPlotOutput==TRUE}: The ggplot object of the MD-plot. 

Otherwise for \code{OnlyPlotOutput==FALSE}: A list of
      \item{ggplotObj}{
   The ggplot object of the MD-plot.
  }
        \item{Ordering}{
   The ordering of columns of data defined by  \code{Ordering}.
  }
        \item{DataOrdered}{
   [1:n,1:d] matrix of ordered and scaled data defined by \code{Ordering} and \code{Scaling}.
  }
  
  Note that the package \pkg{ggExtra} is not necessarily required but if given the feature names are automatically rotated. 
}

\author{
Michael Thrun, Felix Pape contributed with the idea to use ggplot2 as the basic framework.
}
\examples{
\dontshow{
x <- cbind(A = runif(200, 1, 3), B = c(rnorm(100,0,1),rnorm(100,2.4,1)))
MDplot(x)

MDplot(rpois(100,1000))
}
\donttest{
x = cbind(
    A = runif(2000, 1, 5),
    B = c(rnorm(1000, 0, 1), rnorm(1000, 2.6, 1)),
    C = c(rnorm(2000, 2.5, 1)),
    D = rpois(2000, 5)
  )
MDplot(x)
}
\dontshow{
#worst case szenario
# one variable random
#one variable only 10 cases no name
#second variable no name, ten unique cases
Data=structure(c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, -0.0393120245472473, 1.83627122040061, -0.109770573325087, 
-1.01037250829189, -1.23813311175269, 1.52711088395972, 0.885521985229627, 
-1.23499902003538, -0.776410371740625, 1.2841949033221, 0.193760939175263, 
0.481404979713261, 0.601345679024234, 0.646921755978838, 0.706307147163898, 
0.834053863072768, 0.130131568294019, 0.910480525577441, 0.954021320911124, 
0.0200626619625837, 0.887974956305698, 0.398595118895173, 0.633204030804336, 
0.828125820495188, 0.405009237816557, 0.12401070818305, 0.0718156488146633, 
0.758104813285172, 0.696702135261148, 0.522065132390708, 0.910167601890862, 
0.92003503930755, 0.203844893025234, 0.201002374058589, 0.352767747594044, 
0.799360475502908, 0.0215734702069312, 0.630723522277549, 0.212071146350354, 
0.886195732513443, 0.667665995424613, 0.852023241110146, 0.603792746085674, 
0.824619617545977, 0.426947308471426, 0.841525871772319, 0.754500712966546, 
0.408950130455196, 0.829532583244145, 0.585258471313864, 0.636891445145011, 
0.741905940696597, 0.812800564337522, 0.354737375164405, 0.707993973745033, 
0.440743881510571, 0.350462233414873, 0.831067819381133, 0.624896374996752, 
0.759756428189576, 0.0916956132277846, 0.198386165546253, 0.694313021376729, 
0.95162225398235, 0.524074750952423, 0.547433242900297, 0.690702012740076, 
0.780068216146901, 0.631904909387231, 0.241777470335364, 0.354435598943383, 
0.0481027155183256, 0.900913363555446, 0.649821698432788, 0.661771818296984, 
0.670809138799086, 0.785616835346445, 0.396758396411315, 0.719061772339046, 
0.605793445836753, 0.193718662718311, 0.454754204489291, 0.191870308946818, 
0.62292082188651, 0.468462550546974, 0.984963268041611, 0.845834592357278, 
0.853445921558887, 0.712458800524473, 0.828843767754734, 0.211880749324337, 
0.809977566124871, 0.0160791038069874, 0.632794039789587, 0.120573428692296, 
0.688435051357374, 0.197073273127899, 0.20057232491672, 0.0266912879887968, 
0.277176550356671, 0.396465837024152, 0.0808039274998009, 0.106191948521882, 
0.985139257507399, 0.320198103552684, 0.86583884130232, 0.689437984023243, 
0.93079084623605, 0.485144496196881, 0.151472200872377, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.9347080134321, 
0.595704924548045, 0.595704924548045, 0.907554242527112, 0.851929440163076, 
0.902240498922765, 0.595704924548045, 0.902240498922765, 0.595704924548045, 
0.155777201289311, 0.400634071789682, 0.125229959376156, 0.862448507919908, 
0.902240498922765, 0.862448507919908, 0.907554242527112, 0.902240498922765, 
0.155777201289311, 0.595704924548045, 0.851929440163076, 0.9347080134321, 
0.400634071789682, 0.851929440163076, 0.902240498922765, 0.851929440163076, 
0.907554242527112, 0.851929440163076, 0.400634071789682, 0.902240498922765, 
0.155777201289311, 0.9347080134321, 0.902240498922765, 0.181812125025317, 
0.181812125025317, 0.400634071789682, 0.9347080134321, 0.902240498922765, 
0.907554242527112, 0.125229959376156, 0.851929440163076, 0.125229959376156, 
0.125229959376156, 0.902240498922765, 0.902240498922765, 0.907554242527112, 
0.400634071789682, 0.862448507919908, 0.400634071789682, 0.595704924548045, 
0.400634071789682), .Dim = c(100L, 3L), .Dimnames = list(NULL, 
    c("x", "", "")))
MDplot(Data)    
}
}

\note{
1.) One would assume that in the first of the two following cases ggplot2 only adjusts the plotting region but: 

\code{MDplot(MTY)+ylim(c(0,7000))} is equal to \code{MDplot(MTY[MTY<7000])}. 

This means in both cases the data is clipped and AFTERWARDS the density estimation is performed. 

2.) Because of a (sometimes) strange behavior of either ggplot2 or reshape2, numerical column names are changed to character by adding 'C_' which can disabled using \code{ForceNames=TRUE}.

3.) Columnnames will be automatically deblanked and cleaned. To force specific columnnames the input \code{Names} can be used in combination with \code{ForceNames=TRUE}. However, this can result in plotting errors or other strange behavior.

4.) Overlaying MD-plots with robustly estimated gaussians seldomly will yield magenta  (or other \code{GaussianColor}) lines overlaying more than the violin plot they should overlay, because the width of the two plots is not the same (but I am unable to set it strictly in ggplot). In such a case just call the function again.
}

\details{
In short, the MD-plot can be described as a PDE optimized violin plot. The Pareto Density Estimation (PDE) is an approach to estimate the probability density function (pdf) [Ultsch, 2005].

The MD-plot is in the process of beeing peer-reviewed [Thrun/Ultsch, 2019]. 

Statistical testing is performed with \code{\link[diptest]{dip.test}} and \code{\link[moments]{agostino.test}}.

For the paramter \code{Ordering} the following options are possible:
\describe{
\item{\code{Default}}{Ordering of plots by convex/concav/unimodal/nonunimodal shapes using statistical criteria. In this case the \pkg{signal} is required.}

\item{\code{Columnwise}}{Ordering of plots by the order of columns of \code{Data}.}
\item{\code{AsIs}}{Synonym of Columnwise: Ordering of plots by the order of columns of \code{Data}.}

\item{\code{Alphabetical}}{Ordering of plots by the order of columns of \code{Data} sorted in alphabetical order by column names.}

\item{\code{Average}}{Ordering of plots by the order of columns of \code{Data} sorted in order of increasing column-wise average}

\item{\code{Bimodal}}{Ordering of plots by the order of columns of \code{Data} sorted in order of decreasing bimodality amplitude[Zhang et al., 2003]}

\item{\code{Variance}}{Ordering of plots by the order of columns of \code{Data} sorted in order of increasing inter-quartile range}

\item{\code{Statistics}}{Ordering of plots depending on the logarithm of the p-vlaues of statistical testing. In this case the packages \pkg{moments}, \pkg{diptest} and \pkg{signal} are required.}
}

For the paramter \code{Scaling} the following options are possible:
\describe{
\item{\code{None}}{No Scaling of data is done.}

\item{\code{Percentalize}}{Data is scaled between zero and 100.}

\item{\code{CompleteRobust}}{Data is first robustly scaled between zero and 1, then centered to zero and outliers are capped by a robustly formula described in \code{\link{RobustNormalization}}. }

\item{\code{Robust}}{Data is robustly scaled between zero and 1 by a formula described in the \code{\link{RobustNormalization}}. }

\item{\code{Log}}{Data is transformed with a sgined log allowing for negative values to be transformed with a logarithm of base 10, please see \code{\link{SignedLog}} for details.}
}
}
\references{
[Thrun et al., 2020]	Thrun, M. C., Gehlert, T. & Ultsch, A.: Analyzing the Fine Structure of Distributions, PLoS ONE, Vol. 15(10), pp. 1-66, DOI 10.1371/journal.pone.0238835, 2020. 

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in  Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

[Zhang et al., 2003] Zhang, C., Mapes, B., & Soden, B.: Bimodality in tropical water vapour, Quarterly Journalof the Royal Meteorological Society, 129(594), 2847-2866, 2003.
}
\seealso{
\url{https://md-plot.readthedocs.io/en/latest/index.html}

\code{\link{ClassMDplot}}

\url{https://pypi.org/project/md-plot/}
}
\concept{violin plot}
\keyword{geom_violin}
\keyword{PDE}
\keyword{violin}
\keyword{MDplot}
\keyword{MD-plot}
\keyword{MD}
\keyword{pdf}
\concept{Mirrored Density plot}
\concept{Mirrored Density}
\keyword{Density}
\keyword{density}
\keyword{pdf}
\concept{probability density function}
\concept{bean plot}
\concept{vase plot}
\keyword{multivariate}
\concept{box plot}
\concept{box whisker diagramm}
\concept{schematic plot}