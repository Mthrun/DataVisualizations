MAplot=function(x,y,islog=TRUE,densityplot=FALSE,main='MA-plot',xlab,ylab,Cls){
#res=MAplot(x,y,islog=TRUE,densityplot=FALSE,main='Minus versus Add plot',xlab='A',ylab='M',Cls)  
#Minus versus Add plot
#   Bland-Altman plot [Altman/Bland, 1983].
# #INPUT
#   \item{x}{[1:n] numerical vector of a feature/variable
#   }
#   \item{y}{[1:n] another numerical vector of a feature/variable
#   }
#   \item{islog}{ TRUE: MAplot, FALSE: M=x-y versus a=0.5(x+y)
#   }
#   \item{densityplot}{ FALSE: Scatterplot, TRUE: density scatter plot with PDE
#   }
#   \item{main}{see \code{plot}
#   }  
#   \item{xlab}{  see \code{plot}
#   }
#   \item{ylab}{
#     see \code{plot}
#   }
#   \item{Cls}{
#     prior Classification as a numeric vector.
#   }
# 
# #OUTPUT
#   \item{MA}{[1:n,2] Matrix of Minus component of two features and Add component of two features
#   }
#   \item{ggplot}{see \code{ggplot2} output, if densityplot=TRUE, else NULL
#   }
# 
# author: Michael Thrun

   x=checkFeature(x,'x')
   y=checkFeature(y,'y')
 
  if(length(x)!=length(x)) stop('Length of x does not equal length of y')
  
  if(isTRUE(islog)){
    absX = abs(x)
    s = sign(x)
    newX=s * log10(absX + 1)
    
    absY = abs(y)
    s2 = sign(y)
    newY=s * log10(absY + 1)
    
   M=newX-newY
   A=0.5*(newX+newY)
   
    if(missing(xlab)) xlab='log(sqrt(x*y))'
   if(missing(ylab)) ylab='log(x/y)'
   
  }else{
    M=x-y
    A=0.5*(x+y)
    if(missing(xlab)) xlab='0.5(x+y)'
    if(missing(ylab)) ylab='x-y'
  }
  
  if(isTRUE(densityplot)){
    if(!missing(Cls)){warning('Cls not usable in density plots')}
    ggplot=PDEscatter(x = x,y = y,xlab = xlab,ylab = ylab,main = main)
    print(ggplot)
  }else{
    ggplot=NULL
    if(!missing(Cls)){
      Cls=checkCls(Cls,length(x)) #Normalized Cls of equal length
      k=length(unique(Cls))
      UniqueColors=DataVisualizations::DefaultColorSequence[1:k]
      Colors=rep('black',length(x))
      for(i in 1:length(Cls)){
        Colors[i]=UniqueColors[Cls[i]]
      }
      plot(A,M,main=main,xlab = xlab,ylab=ylab,pch=20,col=Colors)
    }else{
      plot(A,M,main=main,xlab = xlab,ylab=ylab,pch=20)
    }
  }
   MA=cbind(M,A)
   colnames(MA)=c('M_y','A_x')
  return(invisible(list(MA=MA,ggplot=ggplot)))
}