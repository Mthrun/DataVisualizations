ClassBarPlot = function(Values, Class, Deviation, Names,
                        ylab = "Values", xlab = "Instances", PlotIt = TRUE){
  # Classbarplot(Values, Class)
  # Classbarplot(Values, Class, Deviation, Names)
  # 
  # DESCRIPTION
  # Represent values for each class and instance as bar plot with optional
  # error deviation, e.g., mean values of features depending on class with
  # standard deviation.
  # 
  # INPUT
  # Values[1:n]        Numeric vector with values (y-axis) in matching order to 
  #                    Class, Deviation and Names
  # Class[1:n]         Numeric vector of classes in matching order to Values
  #                    and Deviation and Names
  # 
  # OPTIONAL
  # Deviation[1:n]     Numeric vector with deviation in matching order to Values
  #                    and Class and Names
  # Names[1:n]         Character or numeric vector of instances (x-axis) in
  #                    matching order to Values and Class and Deviation
  # ylab               Character stating y label.
  # xlab               Character stating x label.
  # PlotIt             Logical value indicating visual output
  #                    TRUE => create visual output
  #                    FALSE => do not create visual output
  #                    (Default: Boolean=TRUE)
  # OUTPUT
  # ggplot2 object for saving or further manipulation.
  # 
  # Author: QMS October 2024
  # 
  
  if(length(Values) != length(Class)){
    stop("Classbarplot.R: Length of vectors Values and Class must equal.")
  }
  
  tmpDF   = as.data.frame(cbind(Values, Class))
  tmpVar1 = as.vector(tmpDF %>% group_by(Class) %>% summarise(total_count=n()))
  tmpVar2 = tmpVar1$total_count
  
  if(!all(tmpVar2 == tmpVar2[1])){
    stop("Classbarplot.R: Provide values for each class and each instance on the x-axis.")
  }
  
  UCls      = unique(Class)
  NumCls    = length(UCls)
  ColNaming = c("Values", "NamesX", "Names", "Class")
  
  if(!missing(Names)){
    if(length(Names) != length(Values)){
      stop("Classbarplot.R: Length of vectors Values and Names must equal.")
    }
    NamesX = rep(1:tmpVar2[1], NumCls)
  }else{
    Names  = rep(1:tmpVar2[1], NumCls)
    NamesX = Names
  }
  
  MatCBP = cbind(Values, NamesX, Names, Class)
  
  if(!missing(Deviation)){
    MatCBP    = cbind(MatCBP, Deviation)
    ColNaming = c(ColNaming, "Deviation")
  }
  
  # DataFrame:
  # as.data.frame(cbind(Values, Class))
  dfCBP            = as.data.frame(MatCBP)
  colnames(dfCBP)  = ColNaming
  row.names(dfCBP) = NULL
  XTicks           = Names[1:tmpVar2[1]]
  XAxis            = 1:tmpVar2[1]
  
  p = ggplot(dfCBP, aes(x = NamesX, y = Values, group = Class, fill = Class)) +
    geom_bar(stat = 'identity', position = 'dodge', alpha = 0.5)
  
  if(!missing(Deviation)){
    p = p + geom_errorbar(aes(ymin = Values - Deviation, ymax = Values + Deviation),
                          position=position_dodge(0.9), color = "black", width=.2)
  }
  
  p = p + theme_bw() +
    theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    theme(axis.text.x = element_text(face = "bold", color = "black", 
                                     size = 8, angle = 0),
          axis.text.y = element_text(face = "bold", color = "black", 
                                     size = 14, angle = 0)) +
    scale_x_continuous(breaks = XAxis, labels = XTicks) +
    ylab(ylab) + xlab(xlab)
  
  if(isTRUE(PlotIt)){
    print(p)
  }
  
  return(list("ggplot2Object" = p))
}
