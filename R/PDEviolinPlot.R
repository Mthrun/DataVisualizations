PDEviolinPlot <- function(Data, Names,fill='darkblue',scale='width',size=0.01){
  #PDEviolinPlot(data, Names)
  # Plots a Boxplot for each column of the given data
  #
  # Input
  # data          Matrix containing data. Each column is one variable.
  # Names         Optional: Names of the variables. If missing the columnnames of data are used.
  # Means          TRUE: with mean, FALSE: Only median
  #
  # Output
  # ggplotObj     The ggplot object of the boxplots
  #
  # Author MT 2018: rewritten function of FP
  if (is.vector(Data)) {
    warning("This boxplot is for several features at once.Calling as.matrix()")
    Data = as.matrix(Data)
  }
  if (missing(Names)) {
    if (!is.null(colnames(Data))) {
      Names = colnames(Data)
    } else{
      Names = 1:ncol(Data)
    }
  } else{
    if (length(Names) != ncol(Data)) {
      warning('Length of Names does not equal number of columns of Data. Renaming variables...')
      Names = 1:ncol(Data)
    } else{
      colnames(Data) <- Names
    }
  }

  requireNamespace("reshape2")
  requireNamespace("ggExtra")
  dataframe = reshape2::melt(Data)
  colnames(dataframe) <- c('ID', 'Variables', 'Values')
  dataframe$Variables=as.character(dataframe$Variables)

    plot <-
      ggplot(data = dataframe,
             aes_string(x = "Variables", group = "Variables", y = "Values"))+ggExtra::rotateTextX() +
      geom_violin(stat = "PDEdensity",fill=fill,scale=scale,size=size)+ theme(axis.text.x = element_text(size=rel(1.2)))

  return(ggplotObj = plot)
} 