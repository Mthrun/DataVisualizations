QQplotForStandardiziation=function(Data,TransData, xug=-3, xog=3,xlab ='Normal', yDataLab='Data',yTransDataLab='Trasformated Data',Symbol4Gerade="red",main='',...){

# qqnormfit(x,xug,xog)
# % QQ-Plot von Daten und Transforierten Daten im Vergleich und jeweils mit Ausgleichsgerade
# % INPUT
# % Data        zu zeichnende Variable
# % TransData   zu zeichnende transformierte Variable
# % xug, xog im interval [xug,xog] wird eine geade interpoliert

# % OPTIONAL
# % Symbol4Gerade    plotsymbol f?r Ausgleichsgerade per default = 'r-'
  
# xlab			        legend for x-axis
# yDatalab			    legend for y-axis
# yTransDatalab  		legend for y-axis
# main			        title for plot  
  
  polyfit=function (x, y, n, r = FALSE) 
{
    ptemp <- lm(y ~ poly(x, degree = n, raw = r))
    p <- ptemp$coefficients
    names(p) <- NULL
    p <- rev(p)
    return(p)
}
erfinv=function (x) 
{
    qnorm((1 + x)/2)/sqrt(2)
}
polyval=function (v, x) 
{
    n <- length(v)
    y <- x * 0 + v[1]
    for (i in 2:n) {
        y <- v[i] + x * y
    }
    return(y)
}
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
 m <- graphics::layout(matrix(c(1, 1,1,1,2,2,2,2), 2, 4))

 par(oma=c(0,0,1,0))#c(u,li,o,re) in
par(pty="s")# Plot immer quadratisch

 #QQPlot of Data
 x=Data
 ylab=yDataLab
 qqnorm(x, col="blue", pch=20, xlab = xlab, ylab = ylab,main='', ...) 
grid(lty='dashed',col='black')
x <- sort(na.last=T,x)
n <- length(x)
X <- ((1:n)-1/2)/n
Y <- sqrt(2)* erfinv(2*X-1)
ind <- which(Y >= xug & Y <= xog)
gx <- Y[ind]
gy <- x[ind]

gerade <- polyfit(gx,gy,1, TRUE);      # polynomdarstellung in Matlab 1. grades (in absteigender Reihenfolge)
yint <-  polyval(gerade,gx);     # die interpolierten punkte gemaess Gerade

lines(gx,yint, col = Symbol4Gerade, lwd = 3)


 #QQPlot of TransData
 x=TransData
 ylab=yTransDataLab
par(pty="s")# Plot immer quadratisch

 qqnorm(x, col="blue", pch=20, xlab = xlab, ylab = ylab,main='', ...)
grid(lty='dashed',col='black')
x <- sort(na.last=T,x)
n <- length(x)
X <- ((1:n)-1/2)/n
Y <- sqrt(2)* erfinv(2*X-1)
ind <- which(Y >= xug & Y <= xog)
gx <- Y[ind]
gy <- x[ind]

gerade <- polyfit(gx,gy,1, TRUE);      # polynomdarstellung in Matlab 1. grades (in absteigender Reihenfolge)
yint <-  polyval(gerade,gx);     # die interpolierten punkte gemaess Gerade

lines(gx,yint, col = Symbol4Gerade, lwd = 3)
  par(def.par)
title(main)
}