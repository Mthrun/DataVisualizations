MDplot4multiplevectors = function(...,
                                  Names,
                                  Ordering = 'Columnwise',
                                  Scaling = "None",
                                  Fill = 'darkblue',
                                  RobustGaussian = TRUE,
                                  GaussianColor = 'magenta',
                                  Gaussian_lwd = 1.5,
                                  BoxPlot = FALSE,
                                  BoxColor = 'darkred',
                                  MDscaling = 'width',
                                  LineSize = 0.01,
                                  LineColor = 'black',
                                  QuantityThreshold = 40,
                                  UniqueValuesThreshold = 12,
                                  SampleSize = 5e+05,
                                  SizeOfJitteredPoints = 1,
                                  OnlyPlotOutput = TRUE) {
  #author: Michael Thrun


  
  inputs <- list(...)
  
  
  #addcol = function(...) {
 #   return(cbind_fill(..., fill = NaN))
 # }
  
  if (length(inputs) == 1) {
    if (is.list(inputs[[1]])) {
      #the list of input consists exactly of one list
      df = do.call(what = CombineCols, inputs[[1]])#for every element of this list do
    } else{
      #assumption: only one element ist given which is not a list, error catching if its not the case is done in base
      df = as.matrix(inputs[[1]])
    }
  } else{
    #several vectors are given, error catching if its not the case is done in (former) rowr
    df = CombineCols(...)
  }
  
  if (!missing(Names)) {
    if (length(Names) == ncol(df)) {
      colnames(df) = Names
    } else{
      warning('Length of Names is not equal to Number of vectors. Ignoring names.')
      colnames(df) = paste0('C', 1:ncol(df))
    }
    
  } else{
    colnames(df) = paste0('C', 1:ncol(df))
  }
  
  MDplot(
    Data = as.matrix(df),
    Ordering = Ordering,
    Scaling = Scaling,
    Fill = Fill,
    RobustGaussian = RobustGaussian,
    GaussianColor = GaussianColor,
    Gaussian_lwd = Gaussian_lwd,
    BoxPlot = BoxPlot,
    BoxColor = BoxColor,
    MDscaling = MDscaling,
    LineSize = LineSize,
    LineColor = LineColor,
    QuantityThreshold = QuantityThreshold,
    UniqueValuesThreshold = UniqueValuesThreshold,
    SampleSize = SampleSize,
    SizeOfJitteredPoints = SizeOfJitteredPoints,
    OnlyPlotOutput = OnlyPlotOutput
  )
}