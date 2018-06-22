QQplotWithFit=function(X,Y,xlab ='X', ylab='Y',col="red",main='',...){

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
 def.par <- par(no.readonly = TRUE) # save default, for resetting...

 par(oma=c(0,0,1,0))#c(u,li,o,re) in
 par(pty="s")# Plot immer quadratisch

 #QQPlot of X
 qqplot(X,Y, col="blue", pch=20, xlab = xlab, ylab = ylab,main=main, ...) 
 grid(lty='dashed',col='black')

 line=lm(Y~X)
 abline(line, col = col, lwd = 3)
 par(def.par)
 Summary=summary(line)
 return(invisible(list(Residuals=residuals.lm(line),Anova=anova(line),Summary=Summary)))
}