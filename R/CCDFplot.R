CCDFplot=function(Data,LineType=0,LogLogPlot=TRUE){
# V = ccdfplot(Data,LogLogPlot,LogBase,LineType)
# plot Complementary Cumulative Distribution Function (ccdf) in Log/Log 
# uses ecdf, CCDF(x) = 1-cdf(x)
#
# INPUT
# Data(1:n)              Vector of data to be plotted
# or
# Data = [x,pdf(x)]         a probability density funciton pdf(x) is given 
# Data = [x,pdf(x),cdf(x)]  the cdf is given, pdf(x) is ignored
#
# OPTIONAL
# LineType              for plot default:  LineType=0 for Line, other numbers see documentation about pch     
#
# LogLogPlot            if LogLogPlot==T (default) do a log/log plot 
#
# OUTPUT Liste V
# V$CCDFuniqX,V$CCDFuniqY    CCDFuniqY= 1-cdf(CCDFuniqX), such that plot(CCDFuniqX,CCDFuniqY)...) 
#                         functions

# in /dbt/Plot/
# Author MT 02/2015
#requireRpackage('pracma')
if(is.matrix(Data)|| is.data.frame(Data)){
  n=nrow(Data)
  d=ncol(Data)
  if(d==1){
    CCDFuniqX = as.vector(Data)
    f_ecd=ecdf(CCDFuniqX)
    CCDFuniqY = 1-f_ecd(CCDFuniqX)
  }else if(d==2){
        CCDFuniqX = Data[,1]
        cdf = CCDFuniqX*0
#         for(i in 2:length(cdf))
#             cdf(i) = trapz(CCDFuniqX(1:i),Data(1:i,2));

        CCDFuniqY = 1-pracma::trapz(CCDFuniqX,Data[,2])
  }else if(d==3){
        CCDFuniqX = Data[,1]
        CCDFuniqY = 1-Data[,3]
  }else{
     stop('Wrong number of columns in Data')
  }
}else if(is.vector(Data)){
  #n=length(Data) 
  CCDFuniqX = Data
  f_ecd=ecdf(Data)
  CCDFuniqY = 1-f_ecd(Data)
}else{
  stop('Wrong Data Input given')
}

# jetzt gibts  CCDFuniqX und CCDFuniqY sortiert
ind=order(CCDFuniqX,decreasing=T)
CCDFuniqX=CCDFuniqX[ind]
CCDFuniqY=CCDFuniqY[ind]
if(LogLogPlot){
  #options(scipen=5)
  if(LineType==0){
    plot(CCDFuniqX,CCDFuniqY,type='l',log="xy",ylab='ccdf = 1-cdf',xlab='data',main='Log Complementary Cumulative Distribution Function (log-ccdf)',xaxs='i',las=1)
  }else{
    plot(CCDFuniqX,CCDFuniqY,pch=LineType,log="xy",ylab='ccdf = 1-cdf',xlab='data',main='Log Complementary Cumulative Distribution Function (log-ccdf)',xaxs='i',las=1)
  }
  #grid()
}else{ # kein log log plot
  if(LineType==0){
    plot(CDFuniqX,CCDFuniqY,type='l',ylim=c(0,1),ylab='ccdf = 1-cdf',xlab='data',main='Complementary Cumulative Distribution Function (ccdf)',xaxs='i',yaxs='i',las=1)
  }else{
    plot(CDFuniqX,CCDFuniqY,pch=LineType,ylim=c(0,1),ylab='ccdf = 1-cdf',xlab='data',main='Complementary Cumulative Distribution Function (ccdf)',xaxs='i',yaxs='i',las=1)
  } 
} # if LogLogPlot==T

#grid on;

return(list(CCDFuniqX=CCDFuniqX,CCDFuniqY=CCDFuniqY))
}