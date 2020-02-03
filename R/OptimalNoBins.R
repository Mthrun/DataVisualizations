OptimalNoBins = function(Data){
#OptimalNrOfBins = OptNrOfBinsV2(Data)
# DESCRIPTION
# Berechung der optimalen Anzahl von Bins fuer ein Histogramm
# nach Keating/Scott 99
# INPUT
# Data               [1:n] vector
# OUTPUT
# OptimalNrOfBins   die bestmoegliche ANzahl von Bins, minimal jedoch 10
#                    Verwendung fuer hist(data,OptimalNrOfBins);
#Anzahl vorhandene Daten
#MT 2019
    if(is.matrix(Data)) nData <- colSums(!is.nan(Data))
    if(is.vector(Data)) nData <- sum(!is.nan(Data))
      
    if(nData<1){
      optNrOfBins<-0
    }else{    
      sigma<-sd(Data,na.rm=TRUE)    
      p<-quantile(Data,c(0.25,0.75),type=8,na.rm = T)
      interquartilRange<-p[2]-p[1]     
      sigmaSir<-min(sigma,interquartilRange/1.349)
      optBinWidth<-3.49*sigmaSir/(nData)^(1/3)
      if(optBinWidth>0){
        optNrOfBins<-max(ceiling((max(Data,na.rm=TRUE)-min(Data,na.rm=TRUE))/optBinWidth),10)
      }else{
        optNrOfBins<-10
      }
    }                  
 return (optNrOfBins) 
}