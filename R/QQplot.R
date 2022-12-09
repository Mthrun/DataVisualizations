QQplot=function(X,Y,Type=8,NoQuantiles=10000,xlab, ylab,col="red",main='',lwd=3,pch=20,subplot=FALSE,...){

# qqnormfit(x,xug,xog)
# % QQ-Plot von Daten und Transforierten Daten im Vergleich und jeweils mit Ausgleichsgerade
# % INPUT
# % X   First Feature
# % Y   Second Feature to compare first feature with

# % OPTIONAL
# % Symbol4Gerade    plotsymbol f?r Ausgleichsgerade per default = 'r-'
  
# xlab			        legend for x-axis
# ylab			        legend for x-axis
# main			        title for plot  
 if(isFALSE(subplot)){
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
  
  X=checkFeature(X,varname = 'X',Funname = "QQplot")
  Y=checkFeature(Y,varname = 'Y',Funname = "QQplot")
  
 def.par <- par(no.readonly = TRUE) # save default, for resetting...

 par(oma=c(0,0,1,0))#c(u,li,o,re) in
 par(pty="s")# Plot immer quadratisch
 } 
 #QQPlot of X
  q_x = quantile(
    X,
    probs = c(1:NoQuantiles) / NoQuantiles,
    na.rm = T,
    type = Type
  )
  q_y = quantile(
    Y,
    probs = c(1:NoQuantiles) / NoQuantiles,
    na.rm = T,
    type = Type
  )
  plot(q_x,q_y, col="dodgerblue1", pch=pch, xlab = xlab, ylab = ylab,main=main, ...)
 #quants=qqplot(X,Y, col="blue", pch=pch, xlab = xlab, ylab = ylab,main=main, ...) 
 grid(lty='dashed',col='black')

 #use only percentiles for regression
 #to be more robust against outliers
 pct_x = quantile(
   X,
   probs = c(1:100) / 100,
   na.rm = T,
   type = Type
 )
 pct_y = quantile(
   Y,
   probs = c(1:100) / 100,
   na.rm = T,
   type = Type
 )
 points(pct_x,pct_y, pch=pch, col="blue")
 line=lm(pct_y~pct_x)
 abline(line, col = col, lwd = lwd)
 Summary=summary(line)
 if(isFALSE(subplot)){
   par(def.par)
 }
 return(invisible(list(Quantiles=cbind(q_x,q_y),Residuals=residuals.lm(line),Anova=anova(line),Summary=Summary)))
}