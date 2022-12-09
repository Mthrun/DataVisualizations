CCDFplot=function(Feature,LogLogPlot=TRUE,PlotIt=TRUE,pch=0,xlab,ylab,main,...){
# V = ccdfplot(Feature,LogLogPlot,LogBase,pch)
# plot Complementary Cumulative Distribution Function (ccdf) in Log/Log 
# uses ecdf, CCDF(x) = 1-cdf(x)
#
# INPUT
# Feature(1:n)              Vector of Feature to be plotted
# or
# Feature = [x,pdf(x)]         a probability density funciton pdf(x) is given 
# Feature = [x,pdf(x),cdf(x)]  the cdf is given, pdf(x) is ignored
#
# OPTIONAL
# pch              for plot default:  pch=0 for Line, other numbers see documentation about pch     
#
# LogLogPlot            if LogLogPlot==T (default) do a log/log plot 
#
# OUTPUT Liste V
# V$CCDFuniqX,V$CCDFuniqY    CCDFuniqY= 1-cdf(CCDFuniqX), such that plot(CCDFuniqX,CCDFuniqY)...) 
#                         functions

# in /dbt/Plot/
# Author MT 02/2015
#requireRpackage('pracma')
  if(missing(xlab)) xlab=deparse1(substitute(Feature))
  
  if(missing(ylab))
  ylab='ccdf = 1-cdf'
  
  
  if(missing(main)){
    if(isTRUE(LogLogPlot)){
      main='Log Complementary Cumulative Distribution Function (log-ccdf)'
    }else{
      main='Complementary Cumulative Distribution Function (ccdf)'
    }
  }

  
if(is.matrix(Feature)|| is.data.frame(Feature)){
  n=nrow(Feature)
  d=ncol(Feature)
  if(d==1){
    CCDFuniqX = as.vector(Feature)
    f_ecd=ecdf(CCDFuniqX)
    CCDFuniqY = 1-f_ecd(CCDFuniqX)
  }else if(d==2){
        CCDFuniqX = Feature[,1]
        cdf = CCDFuniqX*0
#         for(i in 2:length(cdf))
#             cdf(i) = trapz(CCDFuniqX(1:i),Feature(1:i,2));

        CCDFuniqY = 1-pracma::trapz(CCDFuniqX,Feature[,2])
  }else if(d==3){
        CCDFuniqX = Feature[,1]
        CCDFuniqY = 1-Feature[,3]
  }else{
     stop('Wrong number of columns in Feature')
  }
}else if(is.vector(Feature)){
  #n=length(Feature) 
  CCDFuniqX = Feature
  f_ecd=ecdf(Feature)
  CCDFuniqY = 1-f_ecd(Feature)
}else{
  stop('Wrong Feature Input given')
}

# jetzt gibts  CCDFuniqX und CCDFuniqY sortiert
ind=order(CCDFuniqX,decreasing=T)
CCDFuniqX=CCDFuniqX[ind]
CCDFuniqY=CCDFuniqY[ind]
if(isTRUE(PlotIt)){
  if(isTRUE(LogLogPlot)){
    #options(scipen=5)
    if(pch==0){
      plot(CCDFuniqX,CCDFuniqY,type='l',log="xy",xlab=xlab,ylab=ylab,main=main,xaxs='i',las=1,...)
    }else{
      plot(CCDFuniqX,CCDFuniqY,pch=pch,log="xy",xlab=xlab,ylab=ylab,main=main,xaxs='i',las=1,...)
    }
    #grid()
  }else{ # kein log log plot
    if(pch==0){
      plot(CDFuniqX,CCDFuniqY,type='l',ylim=c(0,1),xlab=xlab,ylab=ylab,main=main,xaxs='i',yaxs='i',las=1,...)
    }else{
      plot(CDFuniqX,CCDFuniqY,pch=pch,ylim=c(0,1),xlab=xlab,ylab=ylab,main=main,xaxs='i',yaxs='i',las=1,...)
    } 
  } # if LogLogPlot==T
}
#grid on;

return(list(CCDFuniqX=CCDFuniqX,CCDFuniqY=CCDFuniqY))
}