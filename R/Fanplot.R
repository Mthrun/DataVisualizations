 Fanplot = fanPlot=function(Datavector,Names,Labels,MaxNumberOfSlices,main='',col,MaxPercentage=FALSE,ShrinkPies=0.05,Rline=1.1){
#    fanPlot(Datavector,Names,Labels,main='',col,MaxPercentage=FALSE,ShrinkPies=0.05,Rline=1.1)
# 
#      An better alternative to the pie chart represents amount of values given in data.
#   
# 
# INPUT
#      \item{Datavector   [1:n] a vector of n non unique values
#      \item{Names   names to search for in Datavector, if not set \code{unique} of Datavector is calculated.
#      Labels   [1:k] Labels if they are specially named, if not Names are used.
#      MaxNumberOfSlices    integer, how many slices should be presented at maximum?
#      \item{main}{
#        title below the fan pie, see \code{plot}
#      }
#      \item{col}{default as other colors in this packages, else the same as in \code{plot}
#      }
#      \item{MaxPercentage}{
#        default FALSE; if true the biggest slice is 100 percent instead of the biggest procentual count
#      }
#      \item{ShrinkPies}{
#        distrance between biggest and smalles slice of the pie
#      }
#      \item{Rline}{ The distance between text and pie is defined here as the length of the line in numerical numbers
#      }
#  
# 
#      A normal pie plot is dificult to interpret for a human observer, because humans are not trained well to observe angles [Gohil, 2015, p. 102]. Therefore, the fan plot is used. As proposed in [Gohil 2015] the \code{fan.plot}() of the \code{plotrix} package is used to solve this problem.
# 
# # OUTPUT
#    silent output by calling \code{invisible} of a list with
#      \item{Percentages}{
#        [1:k] percent values visualized in fanplot
#      }
#      \item{Labels}{
#        [1:k] see input \code{Labels}, only relevant ones

# 
#      [Gohil, 2015]  Gohil, Atmajitsinh. R data Visualization cookbook. Packt Publishing Ltd, 2015.
  
   V=internpiechart(Datavector,Names,Labels,MaxNumberOfSlices,col)
   
   Labels=V$Labels
   pct=V$Percents
   colors=V$Cols

   main=paste(length(Datavector),main)
   
  requireNamespace('plotrix')
  pct[pct==0]=0.004

  plotrix::fan.plot(pct,labels=Labels,col=colors,max.span=pi,align="left",main='',include.sumx=MaxPercentage,shrink=ShrinkPies,label.radius = Rline)
   xy <- par("usr")

   #text(0, xy[3], main, cex = 1.5, adj = c(0.5, 1))
   mtext(main,side = 1,cex = 1.5)
  
   return(invisible(list(Percents=pct,Labels=V$Names)))
}
