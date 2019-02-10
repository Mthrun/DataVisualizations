ClassMDplot  <- function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence,
                         ClassNames = NULL, PlotLegend = TRUE,
                         main = 'PDE Violin Plot for each Class',
                         xlab = 'Classes', ylab = 'PDE of Data per Class',
                         MinimalAmoutOfData=40) {
  # PlotHandle = ClassViolinplot(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend);
  # BoxPlot the data for all classes, weight the Plot with 1 (= maximum likelihood)
  # INPUT
  # Data                 Vector of the Data to be plotted
  # Cls                  Vector of class identifiers can be integers or
  #                     NaN's, need not be consecutive nor positive
  # OPTIONAL
  # ColorSequence        The sequence of colors used, if ==0 r not given: DefaultColorSequence
  # ClassNames           Vector of classnames to show correct legend
  # PlotLegend           Add a legend to plot. Default: TRUE
  # main                 Title of the Plot
  # xlab                 Title of the x axis
  # ylab                 Title of the y axis
  #
  # OUTPUT
  # ClassData                 Dataframe used to for the plot
  # ggobject                  ggplot2 plot
  #
  # author: MT12/2015
  # editor: FP12/2015
  #    library(reshape2)
  #    library(ggplot2)
  
  AnzData = length(Data)
  NoNanInd <- which(!is.nan(Data))
  Data <- Data[NoNanInd]
  Cls <- Cls[NoNanInd]
  
  #ClCou <- ClassCount(Cls)
  UniqueClasses = unique(Cls)#ClCou$UniqueClasses
  #CountPerClass = ClCou$countPerClass
  NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  # ClassPercentages = ClCou$classPercentages # KlassenZaehlen
  
  if (is.null(ClassNames)) {
    ClassNames = c(1:NrOfClasses)
    ClassNames <- paste("C", ClassNames, sep = "")
  }
  
  if (NrOfClasses != length(ClassNames))
    warning("Number of classes does not equal number of ClassNames!
              This might result in a wrong plot")
  
  ColorsUnique=ColorSequence[1:NrOfClasses]
  #print(ColorsUnique)
  Colors=rep(NaN,length(Cls))
  ClassNamesVec=rep(NaN,length(Cls))
  for(i in 1:NrOfClasses){
    Colors[Cls==UniqueClasses[i]]=ColorsUnique[i]
    ClassNamesVec[Cls==UniqueClasses[i]]=ClassNames[i]
  }
  
  ClassData = data.frame(cbind(data = Data, class = Cls))
  ClassData=cbind(ClassData,ClassColors=Colors,ClassNames=ClassNamesVec)
  sorted=order(ClassData$class,decreasing = FALSE,na.last = T)
  ClassData=ClassData[sorted,]
  ClassData$class=factor(ClassData$class)
  ClassData$ClassColors=factor(ClassData$ClassColors)
  ClassData$ClassNames=factor(ClassData$ClassNames) 

  Npervar=c()
  NUniquepervar=c()
  for(i in 1:length(UniqueClasses)){
    Npervar[i]=sum(Cls==UniqueClasses[i])
    NUniquepervar[i]=length(unique(Data[Cls==UniqueClasses[i]]))
  }
  
  if(any(Npervar<MinimalAmoutOfData) | any(NUniquepervar<MinimalAmoutOfData)){
    ClassData2=ClassData
    for(i in 1:length(UniqueClasses)){
      if(Npervar[i]<MinimalAmoutOfData | NUniquepervar[i]<MinimalAmoutOfData){
        ClassData2[Cls==UniqueClasses[i],]=NaN
      }
    }
  }else{
    ClassData2=ClassData
  }
  
  ggobject = ggplot2::ggplot(ClassData2,  aes_string(x = 'class', y = 'data')) +
    
  ggplot2::geom_violin(stat = "PDEdensity",aes_string(x='class',y='data',fill = 'class'),scale='width')+
  
  # ggobject <- ggplot(ClassData, aes(x = factor(class), y = data)) +
  #   
  #   geom_boxplot(aes(fill = factor(class)), notch = FALSE) +
  #   stat_summary(
  #     fun.y = "mean",
  #     geom = "point",
  #     shape = 23,
  #     size = 3,
  #     fill = "white"
  #   ) +
    ylab(ylab) + xlab(xlab) +
    scale_x_discrete(labels = ClassNames) +
    scale_fill_manual(values = ColorSequence,
                     labels = ClassNames,
                     name = "Classes") +
    ggtitle(main)+ theme(axis.text.x = element_text(angle = 45, hjust = 1,size = rel(1.2)))
  
  if(any(Npervar<MinimalAmoutOfData) | any(NUniquepervar<MinimalAmoutOfData)){
    DataJitter=ClassData
    for(i in 1:length(UniqueClasses)){
      if(Npervar[i]>MinimalAmoutOfData | NUniquepervar[i]>MinimalAmoutOfData){
        DataJitter[Cls==UniqueClasses[i],]=NaN
      }
    }
    ggobject=ggobject+geom_jitter(size=2,data =DataJitter,aes_string(x = 'class', y = 'data',fill = 'class'),position=position_jitter(0.15))
    
  }
  
  if (!PlotLegend)
    ggobject <- ggobject + theme(legend.position = "none")
  print(ggobject)
  return(invisible(list(ClassData = ClassData, ggobject = ggobject)))
  
}