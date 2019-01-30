ClassBoxplot <- ClassBoxPlot <- function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence, ClassNames = NULL, PlotLegend = TRUE, main = 'Boxplot per Class', xlab = 'Classes', ylab = 'Range of Data') {
    # PlotHandle = ClassBoxPlotMaxLikeli(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend);
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
    
    ClassData = data.frame(cbind(data = Data, class = as.numeric(Cls)))
    
    ggobject <- ggplot(ClassData, aes(x = factor(class), y = data)) +
      geom_boxplot(aes(fill = factor(class)), notch = FALSE) +
      stat_summary(
        fun.y = "mean",
        geom = "point",
        shape = 23,
        size = 3,
        fill = "white"
      ) +
      ylab(ylab) + xlab(xlab) +
      scale_fill_manual(values = ColorSequence,
                        labels = ClassNames,
                        name = "Classes") +
      scale_x_discrete(labels = ClassNames) +
      ggtitle(main)
    
    if (!PlotLegend)
      ggobject <- ggobject + theme(legend.position = "none")
    ggobject
    return(list(ClassData = ClassData, ggobject = ggobject))
    
  }