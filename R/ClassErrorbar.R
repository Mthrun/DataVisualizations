ClassErrorbar=function(Xvalues,Ymatrix,Cls,ClassNames,ClassCols,ClassShape,MeanFun=median,SDfun,JitterPosition=0.5,main="Error bar plot",xlab,ylab,WhiskerWidth=7,Whisker_lwd=1,BW=TRUE){
## ClassErrorbar(Percentage_Ncases,Yvalues,Cls,ClassNames = Methods,ClassCols = Cols,ClassShape = ClassShape)$ggobj
# Plots error bars at Xvalues positions for one or more than one classes
# INPUT
# Xvalues             numerical vector [1:m] positions of error bars in on x-axis for the d variables
# Ymatrix             numerical matric [1:n,1:d] of n cases and d=m*k variables with values for which the error-bar statistics defined by MeanFun  and SDfun should be computed
# Cls                 Optional, numerical vector [1:d] of k classes for the d variables, each class is one method that will be shown as dinstinctive set of error bars in the plot
# ClassNames          Optional, character vector [1:k] of k methods
# ClassCols           Optional, character vector [1:k] of k colors
# ClassShape          Optional, numerical vector [1:k] of k shapes, see pch for details
# MeanFu              Optional, error bar statstic of mean points, default=median
# SDfun               Optional, error bar statstic for the length of whiskers, default is the robust estimation of standard deviation
# JitterPosition      Optional, how much in values of Xvalues should the error bars jitter around Xvalues to not overlap
# main                Optional, title of plot
# xlab                Optional, x-axis label
# ylab                Optional, y-axis label
# BW                  Optional, FALSE: usual ggplot2 background and style which is good for screen visualizations
#                     Default: TRUE: theme_bw() is used which is more appropriate for publications
# WhiskerWidth        Optional, scalar above zero defining the width of the end of the whiskers
# Whisker_lwd         Optional, scalar obove zero defining the thickness of the whisker lines
# OUTPUT
# ggobj               The ggplot object of the Errorbar
# Statistics          [1:(d*k)1:4] data frame in long table format of statstics per class used for plotting   
# 
# DETAILS  
# If k=1, e.g., one method is used, d=m and Cls=rep(1,m).  All vector [1:k] assume the occurance of the classes in Cls as ordered with increasing value.
# Statistics are provided in long table format with the column names Xvalues, Mean, SD and Method. 
# The method column specifies the names of the k classes.
#
#author:              MCT, 2023

  #set optionals
  if(missing(xlab)) xlab=deparse1(substitute(Xvalues))
  if(missing(ylab)) ylab=paste0("Mean(",deparse1(substitute(Ymatrix)),") +- std per class")
  
  if(missing(Cls)){
    Cls=rep(1,ncol(Ymatrix))
  }else{
    if(length(Cls)!=ncol(Ymatrix)){
      warning("ClassErrorbar: length of Cls does not equal number of columns in Ymatrix")
    }
  }
  
  u=sort(unique(Cls))
  
  if(missing(ClassShape)) ClassShape=rep(20,length(u))
  if(missing(ClassNames)) ClassNames=paste("Method",u)
  if(missing(ClassCols)){
    def= c("red", "black", "blue","green")
    Cols=setdiff(DataVisualizations::DefaultColorSequence,def)
    ClassCols=c(def,Cols)[1:length(u)]
  }
  #robust estimation of standard deviation

  
  sdrobust_hlp=function(x,na.rm){
    if(na.rm) x=x[is.finite(x)]
    
    DataVisualizations::Stdrobust(x)
  }
  if(missing(SDfun)) SDfun=sdrobust_hlp
  
#compute statistics per class
  for(i in 1:length(u)){
    Means=apply(Ymatrix[,Cls==u[i]], 2, MeanFun,na.rm=T)
    SDs=apply(Ymatrix[,Cls==u[i]], 2, SDfun,na.rm=T)
    if(i==1){
      df=data.frame(Xvalues=Xvalues,Yvalues=Means,sd=SDs,Method=ClassNames[i])
    }else{
      df=rbind(df,data.frame(Xvalues=Xvalues,Yvalues=Means,sd=SDs,Method=ClassNames[i]))
    }
  }
#plotting
  df=cbind(df,lower=df$Yvalues-df$sd,upper=df$Yvalues+df$sd)
  pd <- ggplot2::position_dodge(JitterPosition) # move them .05 to the left and right
  #for geom_errorbar one can set either colour for all the same outside of aes() or color per class inside of aes()
  # ggobj=ggplot2::ggplot(df, ggplot2::aes(x="Xvalues", y="Yvalues", colour="Method", group="Method"))+ggplot2::geom_errorbar(data = df,ggplot2::aes(ymin=Yvalues-sd, ymax=Yvalues+sd,colour=Method), lwd=Whisker_lwd,width=WhiskerWidth, position=pd)+
  #   ggplot2::geom_point(ggplot2::aes(shape="Method", color="Method", size="Method"),position=pd, size=3)+
  #   ggplot2::scale_shape_manual(values=ClassShape)+
  #   ggplot2::scale_colour_manual(values =ClassCols)
  ggobj=ggplot2::ggplot(df, ggplot2::aes(x=.data$Xvalues, y=.data$Yvalues, colour=.data$Method, group=.data$Method))+geom_errorbar(data = df,ggplot2::aes(ymin=.data$lower, ymax=.data$upper,colour=.data$Method), lwd=Whisker_lwd,width=WhiskerWidth, position=pd)+
    geom_point(ggplot2::aes(shape=.data$Method, color=.data$Method, size=.data$Method),position=pd, size=3)+
    scale_shape_manual(values=ClassShape)+
    scale_colour_manual(values =ClassCols)
  #setting labels and centering title
  ggobj=ggobj+ggplot2::xlab(xlab)+ggplot2::ylab(ylab )+ggplot2::ggtitle(main)+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  #setting background,requires centering title again
  if(isTRUE(BW))
    ggobj=ggobj+ggplot2::theme_bw()+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(list(ggobj=ggobj,Statistics=df))
}