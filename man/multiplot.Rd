\name{Multiplot}
\alias{Multiplot}
\title{Plot multiple ggplots objects in one panel}
\usage{Multiplot(..., Plotlist=NULL, ColNo=1, LayoutMat,Plotter =
                    "native"),main,main_fontsize=16)}
\arguments{
\item{...}{ multiple ggplot objects to be plotted}
\item{Plotlist}{Optional: list filled with ggplot objects to be plotted}
\item{ColNo}{Number of columns in layout}
\item{LayoutMat}{A matrix specifying the layout. If present, 'ColNo' is ignored.}
  \item{Plotter}{
Optional, either "\code{ggplot}", or "\code{native}"
}
  \item{main}{
Optional, string specifying the title of the overall figure
}
  \item{main_fontsize}{
Optional, scalar defining the font size of the title
}
}
\value{List with
Plotlist
}
\description{
  ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)  \cr
  For example, if the layout is specified as the matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.}

\examples{
data(Lsun3D)
Data=Lsun3D$Data
Cls=Lsun3D$Cls
obj1=Classplot(Data[,1],Data[,2],Cls=Cls,Plotter="ggplot",Size=3,main="Top plot")
obj2=Classplot(Data[,2],Data[,3],Cls=Cls,Plotter="ggplot",Size=3,main="Middle plot")
obj3=Classplot(Data[,1],Data[,3],Cls=Cls,Plotter="ggplot",Size=3,main="Bottom plot")
V=Multiplot(obj1,obj2,obj3)
}

\author{
Winston Chang
}