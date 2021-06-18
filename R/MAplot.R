MAplot=function(X,Y,islog=TRUE,LoA=FALSE,CI=FALSE,densityplot=FALSE,main,xlab,ylab,Cls,lwd=2,...){
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
  if(missing(main)){
    if(isTRUE(islog)) 
      main="MA-plot"
    else
      main="Bland-Altman plot"
  }
  X=checkFeature(x = X,varname = "X")
  Y=checkFeature(x = Y,varname = "Y")

  if(length(X)!=length(X)) stop('Length of X does not equal length of Y')
  
  if(isTRUE(islog)){
    #absX = abs(X)
    # s = sign(X)
    # newX=s * log10(absX + 1)
    # 
    # absY = abs(Y)
    # s2 = sign(Y)
    # newY=s * log10(absY + 1)
   # M=newX-newY
   # A=0.5*(newX+newY)
    X=SignedLog(X,Base = "Ten")
    Y=SignedLog(Y,Base = "Ten")
    #M=X-Y
    #A=0.5*(X+Y)
    
   if(missing(xlab)) xlab=paste0('log(sqrt(',xlabstr,' * ',ylabstr,'))')
   if(missing(ylab)) ylab=paste0('log(',xlabstr,' / ',ylabstr,')')
   
  }else{
    if(missing(xlab)) xlab=paste0('0.5*(',xlabstr,' + ',ylabstr,')')
    if(missing(ylab)) ylab=paste0(xlabstr,' - ',ylabstr)
  }
  M=X-Y
  A=0.5*(X+Y)
  
  if(!(isFALSE(LoA)&isFALSE(CI))){
    meanlinevalie=mean(M,na.rm = TRUE)
    n=length(M)
    s=sd(M,na.rm = TRUE)
    loaval=1.96*s
  
    if(isTRUE(CI)){
      error <- qt(0.95,df=n-1)*s/sqrt(n)
      cis=1.96*s+error
    }
    if(isTRUE(densityplot))
      warning("LoA=TRUE is ignored if densityplot=TRUE")
  }

  if(isTRUE(densityplot)){
    if(!missing(Cls)){warning('Cls not usable in density plots')}
    
    Handle=DensityScatter(X = X,Y = Y,xlab = xlab,ylab = ylab,main = main,PlotIt = T,...)$Handle
    #print(Handle)
  }else{
    Handle=NULL
    ylim=c(min(M,na.rm = TRUE)*1.05,max(M,na.rm = TRUE)*1.05)
    if(isTRUE(LoA)){
      ylim[1]=min(c(ylim[1],-1.05*loaval))
      ylim[2]=max(c(ylim[2],1.05*loaval))
    }
    if(isTRUE(CI)){
      ylim[1]=min(c(ylim[1],-1.05*cis))
      ylim[2]=max(c(ylim[2],1.05*cis))
    }
    
    if(!missing(Cls)){
      Cls=checkCls(Cls,length(X)) #Normalized Cls of equal length
      # k=length(unique(Cls))
      # UniqueColors=DataVisualizations::DefaultColorSequence[1:k]
      # Colors=rep('black',length(X))
      # for(i in 1:length(Cls)){
      #   Colors[i]=UniqueColors[Cls[i]]
      # }
      Classplot(A,M,Cls=Cls,main=main,xlab = xlab,ylab=ylab,ylim=ylim,Plotter = "native",...)
    }else{
      Classplot(A,M,main=main,xlab = xlab,ylab=ylab,ylim=ylim,Plotter = "native",Colors = "black",...)
    }
    if(isTRUE(LoA)){
      abline(h =meanlinevalie,col="blue",lwd=lwd)
      abline(h =meanlinevalie+loaval,col="green",lwd=lwd)#,lty="dashed")
      abline(h =meanlinevalie-loaval,col="green",lwd=lwd)#,lty="dashed")
      if(isTRUE(CI)){
        abline(h =meanlinevalie+cis,col="red",lty="dashed",lwd=lwd)
        abline(h =meanlinevalie-cis,col="red",lty="dashed",lwd=lwd)
      }
    }else{
      if(isTRUE(CI)){
        abline(h =meanlinevalie,col="blue",lwd=lwd)
        abline(h =meanlinevalie+cis,col="red",lty="dashed",lwd=lwd)
        abline(h =meanlinevalie-cis,col="red",lty="dashed",lwd=lwd)
      }
    }
  }
   MA=cbind(M,A)
   colnames(MA)=c('M_y','A_x')
  return(invisible(list(MA=MA,Handle=Handle)))
}