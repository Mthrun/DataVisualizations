BoxplotData <- function(Data, Names,Means=TRUE){
  #BoxplotData(data, Names)
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
  dataframe = reshape2::melt(Data)
  colnames(dataframe) <- c('ID', 'Variables', 'Values')
  dataframe$Variables=as.character(dataframe$Variables)
  if (isTRUE(Means)) {
    plot <-
      ggplot(data = dataframe,
             aes_string(x = "Variables", group = "Variables", y = "Values")) +
      geom_boxplot() +
      stat_summary(
        fun.y = 'mean',
        geom = 'point',
        shape = 23,
        size = 3,
        fill = "white"
      )
  } else{
    plot <-
      ggplot(data = dataframe,
             aes_string(x = "Variables", group = "Variables", y = "Values")) +
      geom_boxplot()
  }
  return(ggplotObj = plot+ theme(axis.text.x = element_text(angle = 90, hjust = 1)))
} 