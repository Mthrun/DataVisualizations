bimodal=function(Data,PlotIt=FALSE,na.rm=T){
  
  if(!is.vector(Data)){
    Data=as.vector(Data)
    warning('Data has to be a vector but is not, calling as.vector()')
  }
  if(isTRUE(na.rm)){
    Data=Data[is.finite(Data)]
  }
    convexconcave=function(x,fx,PlotIt=FALSE){
    # [Kruemmung,ProConvex,ProConcave,SecondDerivative,ErsteAbleitung] = convexconcave(x,fx,PlotIt) 
    # Abschaetzung, in wieweit eine Funktion konvex oder konkav ist.
    # 
    # INPUT
    # x(1:n)             x           values of function
    # fx(1:n)            fx = f(x)   correponding y values
    # OPTIONAL
    # PlotIt              ==1  plot the function with konvex & concave parts, default: PlotIt==0 
    # OUTPUT
    # Kruemmung          (ProConvex-ProConcave)/n*100
    # ProConvex          (longest successive part where SecondDerivative>0)/n *100
    # ProConcave         (longest successive part where SecondDerivative<0)/n *100
    # SecondDerivative   finite and filtered approximation to second derivative of f(x)
    # ErsteAbleitung     finite and filtered approximation to first  derivaive of f(x)
    
    # author: Michael Thrun, reimplemented from matlab of ALU 2006

    # CONSTANTS
      requireNamespace('signal')
    EPS = 1.5  # minimale Kruemmung >0
    
    # [row,col]=size(x)   if col>row   x= x'   end  # Spaltenvektor machen
    # [row,col]=size(fx)  if col>row   fx= fx'  end  # Spaltenvektor machen
    Anz = max(c(length(x),1))
    
    ErsteAbleitung = diff(fx)/ diff(x)   #an approximate derivative.
    ErsteAbleitung = c(0,ErsteAbleitung)  #so lang wie x machen
    windowSize = 13 
    ErsteAbleitung = signal::filter(matrix(1,1,windowSize)/windowSize,1,ErsteAbleitung) 
    
    SecondDerivative = diff(ErsteAbleitung)/diff(x) 
    SecondDerivative = c(0,SecondDerivative)  # Gliche laenge wie x
    windowSize = 15 
    SecondDerivative = signal::filter(matrix(1,1,windowSize)/windowSize,1,SecondDerivative) 
    
    Next = c(2:Anz,Anz) 
    PosOrNeg = x*0  
    PosOrNeg[SecondDerivative >  EPS] = 1  
    PosOrNeg[SecondDerivative < -EPS] =-1  
    NextIdentical = PosOrNeg == PosOrNeg[Next] # true if the next is on the same side
    PosRuns =  NextIdentical & (SecondDerivative > EPS) 
    NegRuns =  NextIdentical & (SecondDerivative < -EPS) 
    
    MaxPosRunLength = 0 
    PosRunLength = 0 
    MaxNegRunLength = 0 
    NegRunLength = 0 
    for(i in 1:Anz){
      if(PosRuns[i]){
      PosRunLength=PosRunLength+1  
      }else{
        PosRunLength =0 
      }
      if(NegRuns[i]){
      NegRunLength=NegRunLength+1  
      }else{
        NegRunLength =0 
      }
      MaxPosRunLength = max(c(MaxPosRunLength,PosRunLength)) 
      MaxNegRunLength = max(c(MaxNegRunLength,NegRunLength))     
    }  # for i=1:Anz
    
    ProConvex  = MaxPosRunLength/Anz*100 
    ProConcave = MaxNegRunLength/Anz*100 
    Kruemmung  = (ProConvex-ProConcave)/Anz*100 
    
    if(isTRUE(PlotIt)){
      PosInd = which(PosRuns!=0) 
      NegInd = which(NegRuns!=0) 
      plot(x,fx,col='blue',xlab='x',ylab='f(x)',main='',pch=5,sub = 'green = convex, red = concave') #
      points(x[PosInd],fx[PosInd],col='green',pch=4)
      points(x[NegInd],fx[NegInd],col='red',pch=6) 
    }
    
    return(list(Kruemmung=Kruemmung,ProConvex=ProConvex,ProConcave=ProConcave,SecondDerivative=SecondDerivative,ErsteAbleitung=ErsteAbleitung))
  }

# [Bimodal,ProConvex,ProConcave] = BiModal(Data,PlotIt) 
# Abschaetzung, in wieweit eine empirische Datenvertielung zwei Modi hat.
# 
# INPUT
# Data(1:n)           Daten als Zeilenvektoren
# OPTIONAL
# PlotIt              ==1  plot the function with konvex & concave parts, default: PlotIt==0 
#
# OUTPUT
# Bimodal            in [0,1] indication (probability) whether Data is bimodal        
# ProConvex          (longest successive part where SecondDerivative>0)/n *100
# ProConcave         (longest successive part where SecondDerivative<0)/n *100

# author: Michael Thrun, reimplemented from matlab of ALU 2006

 
fx = quantile(Data, c(1:99)/100, type = 5, na.rm = TRUE)
Percent =seq(from=0.01,by=0.01,to=0.99) 
x= qnorm(Percent,0,1) 
V = convexconcave(x,fx,PlotIt) 
Kruemmung = V$Kruemmung
ProConvex = V$ProConvex
ProConcave = V$ProConcave
SecondDerivative = V$SecondDerivative
ErsteAbleitung = V$ErsteAbleitung

Bimodal = pnorm(min(ProConvex,ProConcave),7,3) 
#Bimodal = round(Bimodal,3) 

if(isTRUE(PlotIt)){
  title(paste('Bimodal =',round(Bimodal,3)*100,'- ProConvex =',round(ProConvex,2),'- ProConcave = ',round(ProConcave,2)))
}
return(list(Bimodal=Bimodal,ProConvex=ProConvex,ProConcave=ProConcave))
}
