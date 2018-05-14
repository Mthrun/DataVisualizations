\name{DrawWorldWithCls}
\alias{DrawWorldWithCls}
\title{Plot a classificated world map}
\description{
  This function plots a world map where the single countries get colored differently by using a classification
}

\arguments{
  \item{CountryCode}{Vector of Countrys belonging to the Cls}
  \item{Cls}{Classes belonging to the Countries from CountryCode}
  \item{JoinCode}{ System that is used for the CountryCodes. Possible are: "ISO3", "UN"}
  \item{Title}{Title that will be written above the map}
  \item{Colors}{Vector that colors for classes will be selected from}
}
\value{
a plot
}

\author{
Florian Lerch
}
