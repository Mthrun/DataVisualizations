\name{OpposingViolinBiclassPlot}
\alias{OpposingViolinBiclassPlot}
\title{OpposingViolinBiclassPlot}
\description{Creates a set of two violin plots opposing each other}
\usage{
OpposingViolinBiclassPlot(ListData, Names, BoxPlots = FALSE,
Subtitle = c("AttributeA", "AttributeB"),
Title = "Opposing Violin Biclass Plot")
}
\arguments{
\item{ListData}{List of k matrices as elements where each element has shape
[1:n, 1:2]}
\item{Names}{Vector of character names for each element of ListData}
\item{BoxPlots}{Optional: Boolean variable BoxPlots = TRUE shows a box plot drawn into the violin
plot. BoxPlots = FALSE shows no box plot. Default: BoxPlots = FALSE}
\item{Subtitle}{Optional: Vector of character names for two classes. The classes are
described as features contained in the matrix [1:n, 1:2]}
\item{Title}{Optional: Character containing the title of the plot.}
}
\value{Plotly object.}
\author{Quirin Stier}

