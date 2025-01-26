\name{stat_pde_density}
\alias{stat_pde_density}
\title{
Calculate Pareto density estimation for ggplot2 plots
}
\description{
This function enables to replace the default density estimation for ggplot2 plots with the Pareto density estimation [Ultsch, 2005].
It is used for the PDE-Optimized violin plot published in [Thrun et al, 2018].
}
\usage{
stat_pde_density(mapping = NULL, data = NULL,
geom = "violin", bounds = bounds, 
position = "dodge", ..., 
                    trim = TRUE, scale =
                    "area", na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = TRUE)
}
\arguments{
  \item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data.}

\item{geom}{The geometric object to use display the data}

\item{bounds}{bounds}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{color = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{trim}{This parameter only matters if you are displaying multiple
densities in one plot. If \code{FALSE}, the default, each density is
computed on the full range of the data. If \code{TRUE}, each density
is computed over the range of that group: this typically means the
estimated x values will not line up, and hence you won't be able to
stack density values.}

\item{scale}{When used with \code{\link[ggplot2:geom_violin]{geom_violin}}: if "area" (default), all violins have the same area (before trimming
the tails). If "count", areas are scaled proportionally to the number of
observations. If "width", all violins have the same maximum width.}

\item{na.rm}{If \code{FALSE} (the default), removes missing values with
a warning.  If \code{TRUE}, silently removes missing values.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\details{
Pareto Density Estimation (PDE) is a method for the estimation of probability density functions using hyperspheres. The Pareto-radius of the hyperspheres is derived from the optimization of information for minimal set size. It is shown, that Pareto Density is the best estimate for clusters of Gaussian structure. The method is shown to be robust when cluster overlap and when the variances differ across clusters.
}
\references{
Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

[Thrun et al, 2018] Thrun, M. C., Pape, F., & Ultsch, A.: Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 
}
\author{
Felix Pape
}
\seealso{
\code{\link[ggplot2:stat_density]{stat_density}}
}
\examples{
miris <- reshape2::melt(iris)

ggplot2::ggplot(miris, 

mapping = ggplot2::aes(y = .data$value, x = .data$variable)) +

ggplot2::geom_violin(stat = "PDEdensity")
}