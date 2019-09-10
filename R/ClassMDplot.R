ClassMDplot  <- function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence,
                         ClassNames = NULL, PlotLegend = TRUE,
                         main = 'PDE Violin Plot for each Class',
                         xlab = 'Classes', ylab = 'PDE of Data per Class',
                         MinimalAmoutOfData=40,SampleSize=1e+05) {
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
  
 
  NoNanInd <- which(!is.nan(Data))
  Data <- Data[NoNanInd]
  Cls <- Cls[NoNanInd]
  
  AnzData = length(Data)

  #split quoted
  TrainInd <- c()
  if(AnzData>SampleSize){
    UniqueClasses=unique(Cls)
    Percentage=round(SampleSize/AnzData,2)
    for(i in UniqueClasses){
      ClassInd <- which(Cls==i)
      nclass=round(length(ClassInd)* Percentage,0)
      if(nclass>SampleSize) #adjusted splited quoted (only for classes bigger than sample size)
        sampleInd <- sample(ClassInd,nclass)
      else
        sampleInd = ClassInd
      
      TrainInd=c(TrainInd,sampleInd)
    }
      Data=Data[TrainInd] #Data[TrainInd,,drop=FALSE]
      Cls=Cls[TrainInd]
    AnzData=length(Data)
  }
  
  if(AnzData<3e+03){
    Cls=checkCls(Cls,AnzData,Normalize=TRUE)
  }else{
    Cls=checkCls(Cls,AnzData,Normalize=FALSE)
  }
  #ClCou <- ClassCount(Cls)
  UniqueClasses = sort(unique(Cls))#ClCou$UniqueClasses
  #CountPerClass = ClCou$countPerClass
  NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  # ClassPercentages = ClCou$classPercentages # KlassenZaehlen
  
  if (is.null(ClassNames)) {
    ClassNames = unique(Cls)
    ClassNames <- paste("C", ClassNames, sep = "")
  }else{
    if(!is.character(ClassNames)){
      warning('ClassNames should be a character vector. Ignoring input and setting to default.')
      ClassNames = unique(Cls)
      ClassNames <- paste("C", ClassNames, sep = "")
    }
  }
  
  if (NrOfClasses != length(ClassNames))
    warning("Number of classes does not equal number of ClassNames!
              This might result in a wrong plot")
  
  ColorsUnique=ColorSequence[1:NrOfClasses]
  names(ClassNames)=UniqueClasses

  # hex_hlp = function(cname) {
  #   colMat <- grDevices::col2rgb(cname)
  #   grDevices::rgb(red = colMat[1,] / 255,
  #                  green = colMat[2,] / 255,
  #                  blue = colMat[3,] / 255)
  # }
  # ColorsUniqueHex=hex_hlp(ColorsUnique)

  Colors=rep(NaN,length(Cls))
  ClassNamesVec=rep(NaN,length(Cls))
  # ClassColorsHex=rep(NaN,length(Cls))
  for(i in 1:NrOfClasses){
    Colors[Cls==UniqueClasses[i]]=ColorsUnique[i]
    ClassNamesVec[Cls==UniqueClasses[i]]=ClassNames[i]
    # ClassColorsHex[Cls==UniqueClasses[i]]=ColorsUniqueHex[i]
  }

  ClassData = data.frame(cbind(data = Data, class = Cls))
  ClassData=cbind(ClassData,ClassColors=Colors,ClassNames=ClassNamesVec)#,ClassColorsHex=ClassColorsHex)
  sorted=order(ClassData$class,decreasing = FALSE,na.last = T)
  ClassData=ClassData[sorted,]


  ClassData$class=factor(ClassData$class)
  ClassData$ClassColors=factor(ClassData$ClassColors)
  ClassData$ClassNames=factor(ClassData$ClassNames) 
  #ClassData$ClassColorsHex=factor(ClassData$ClassColorsHex) 

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
        ClassData2[ClassData2$class==UniqueClasses[i],'data']=NaN
      }
    }
  }else{
    ClassData2=ClassData
  }

  Colors=as.vector(ClassData2$ClassColors)
  names(Colors)=as.vector(ClassData2$class)
  Colors=Colors[unique(names(Colors))]

  UniqueClassesPlot=ClassData2$class
  names(UniqueClassesPlot)=as.vector(ClassData2$ClassNames)
  UniqueClassesPlot=UniqueClassesPlot[unique(names(UniqueClassesPlot))]

  ggobject = ggplot2::ggplot(ClassData2,  aes_string(x = 'class', y = 'data',fill = 'class'),show.legend = PlotLegend) +
    
  ggplot2::geom_violin(stat = "PDEdensity",aes_string(x='class', y='data',fill = 'class'),scale='width',show.legend = PlotLegend)
    
    
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

  
  if(any(Npervar<MinimalAmoutOfData) | any(NUniquepervar<MinimalAmoutOfData)){
    DataJitter=ClassData
    for(i in 1:length(UniqueClasses)){
      if(Npervar[i]>MinimalAmoutOfData | NUniquepervar[i]>MinimalAmoutOfData){
        DataJitter[DataJitter$class==UniqueClasses[i],'data']=NaN
      }
    }
    ggobject=ggobject+geom_jitter(size=2,data =DataJitter,aes_string(x = 'class', y = 'data',fill='class'),position=position_jitter(0.15),show.legend = PlotLegend)
    
  }

  ggobject=ggobject+ylab(ylab) + xlab(xlab) +
    scale_x_discrete(limits=UniqueClassesPlot,labels = ClassNames) +
    scale_fill_manual(limits=UniqueClassesPlot,values = Colors,
                      name = "Classes") +
    ggtitle(main)+ theme(axis.text.x = element_text(angle = 45, hjust = 1,size = rel(1.2)))


  if (isFALSE(PlotLegend)){
    ggobject <- ggobject + theme(legend.position = "none",legend.title = NULL,legend.text = element_text(inherit.blank = T))
  }
  
  
  print(ggobject)
  return(invisible(list(ClassData = ClassData, ggobject = ggobject)))
  
}