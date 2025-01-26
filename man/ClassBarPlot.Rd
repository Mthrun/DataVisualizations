\name{ClassBarPlot}
\alias{ClassBarPlot}
\title{ClassBarPlot}
\description{
Represent values for each class and instance as bar plot with optional error
deviation, e.g., mean values of features depending on class with standard
deviation.
}
\usage{
ClassBarPlot(Values, Cls, Deviation, Names, ClassColors,

            ylab = "Values", xlab = "Instances", PlotIt = TRUE)
}
\arguments{
\item{Values}{[1:n] Numeric vector with values (y-axis) in matching order to 
Cls, Deviation and Names.}
\item{Cls}{[1:n] Numeric vector of classes in matching order to Values and
Deviation and Names.}
\item{Deviation}{[1:n] Numeric vector with deviation in matching order to Values
and Cls and Names.}
\item{Names}{[1:n] Character or numeric vector of instances (x-axis) in matching
order to Values and Cls and Deviation.}
\item{ClassColors}{Character vector of color names stating either the colors
for each class or defining colors matching the class vector cls.}
\item{ylab}{Character stating y label.}
\item{xlab}{Character stating x label.}
\item{PlotIt}{Logical value indicating visual output TRUE => create visual
output FALSE => do not create visual output (Default: Boolean=TRUE).}
}
\value{
ggplot2 object
}
\author{
Quirin Stier
}
\examples{
# Compute means and counts
tmpVar1 <- aggregate(Sepal.Length ~ Species, data = iris, FUN = function(x) c(mean = mean(x), n = length(x)))
tmpVar2 <- aggregate(Sepal.Width ~ Species, data = iris, FUN = function(x) c(mean = mean(x), n = length(x)))
tmpVar3 <- aggregate(Petal.Length ~ Species, data = iris, FUN = function(x) c(mean = mean(x), n = length(x)))
tmpVar4 <- aggregate(Petal.Width ~ Species, data = iris, FUN = function(x) c(mean = mean(x), n = length(x)))

# Extract mean and count
tmpVar1_mean <- tmpVar1$Sepal.Length[, "mean"]
tmpVar2_mean <- tmpVar2$Sepal.Width[, "mean"]
tmpVar3_mean <- tmpVar3$Petal.Length[, "mean"]
tmpVar4_mean <- tmpVar4$Petal.Width[, "mean"]

# Compute standard deviations
tmpVar5 <- aggregate(Sepal.Length ~ Species, data = iris, FUN = sd)
tmpVar6 <- aggregate(Sepal.Width ~ Species, data = iris, FUN = sd)
tmpVar7 <- aggregate(Petal.Length ~ Species, data = iris, FUN = sd)
tmpVar8 <- aggregate(Petal.Width ~ Species, data = iris, FUN = sd)

# Combine results
Values <- c(tmpVar1_mean, tmpVar2_mean, tmpVar3_mean, tmpVar4_mean)
Class <- rep(1:3, 4)
Deviation <- c(tmpVar5$Sepal.Length, tmpVar6$Sepal.Width, tmpVar7$Petal.Length, tmpVar8$Petal.Width)
  
  if(length(Values) == length(Class)){
    ClassBarPlot(Values = Values, Cls = Class, Deviation = Deviation)
  }
  
}
\keyword{ClassBarPlot}% use one of  RShowDoc("KEYWORDS")