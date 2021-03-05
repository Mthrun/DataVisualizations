MAplot=function(X,Y,islog=TRUE,densityplot=FALSE,main='MA-plot',xlab,ylab,Cls){
#res=MAplot(X,Y,islog=TRUE,densityplot=FALSE,main='Minus versus Add plot',xlab='A',ylab='M',Cls)  
#Minus versus Add plot
#   Bland-Altman plot [Altman/Bland, 1983].
# #INPUT
#   \item{X}{[1:n] numerical vector of a feature/variable
#   }
#   \item{Y}{[1:n] another numerical vector of a feature/variable
#   }
#   \item{islog}{ TRUE: MAplot, FALSE: M=X-Y versus a=0.5(X+Y)
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
  if(missing(xlab)) xlabstr=deparse1(substitute(X))
  if(missing(ylab)) ylabstr=deparse1(substitute(Y))
   X=checkFeature(X,'X')
   Y=checkFeature(Y,'Y')
 
  if(length(X)!=length(X)) stop('Length of X does not equal length of Y')
  
  if(isTRUE(islog)){
    absX = abs(X)
    s = sign(X)
    newX=s * log10(absX + 1)
    
    absY = abs(Y)
    s2 = sign(Y)
    newY=s * log10(absY + 1)
    
   M=newX-newY
   A=0.5*(newX+newY)
   
    if(missing(xlab)) xlab=paste0('log(sqrt(',xlabstr,' * ',ylabstr,'))')
   if(missing(ylab)) ylab=paste0('log(',xlabstr,' / ',ylabstr,')')
   
  }else{
    M=X-Y
    A=0.5*(X+Y)
    if(missing(xlab)) xlab=paste0('0.5*(',xlabstr,' + ',ylabstr,')')
    if(missing(ylab)) ylab=paste0(xlabstr,' - ',ylabstr)
  }
  
  if(isTRUE(densityplot)){
    if(!missing(Cls)){warning('Cls not usable in density plots')}
    ggplot=PDEscatter(X = X,Y = Y,xlab = xlab,ylab = ylab,main = main)
    print(ggplot)
  }else{
    ggplot=NULL
    if(!missing(Cls)){
      Cls=checkCls(Cls,length(X)) #Normalized Cls of equal length
      k=length(unique(Cls))
      UniqueColors=DataVisualizations::DefaultColorSequence[1:k]
      Colors=rep('black',length(X))
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