ROC = function(Data, Cls, Names = NULL, Colors = NULL){
  # V = ROC(Data, Cls, Names)
  # 
  # INPUT
  # Data[1:n]               Vector of Data to be plotted
  # 
  # OPTIONAL
  # 
  # OUTPUT a list of
  # plot               plotly object
  #
  if(!requireNamespace("ROCit")){
    warning("ROC.R: Please install ROCit in order to use ROC().")
    return("ROC.R: Please install ROCit in order to use ROC().")
  }
  
  if(is.null(Data)){
    stop("Data is not given.")
  }else if(!is.numeric(Data)){
    stop("Data is not numeric.")
  }
  
  DIM = 1
  if(is.matrix(Data)){
    DIM = dim(Data)[2]
    N   = dim(Data)[1]
  }else{
    N   = length(Data)
  }
  
  if(is.null(Cls)){
    stop("Cls is not given.")
  }else if(!is.vector(Cls) | !is.numeric(Cls)){
    stop("Cls is no numeric vector.")
  }
  
  if(is.null(Names)){
    Names = c()
    for(i in 1:DIM){
      Names = c(Names, paste0("ROC", i))
    }
  }else if(!is.vector(Names) | !is.character(Names)){
    stop("Names is no character vector.")
  }
  
  if(is.null(Colors)){
    #Colors = c("blue", "red", "black", "cyan")
    Colors = DataVisualizations::DefaultColorSequence[1:DIM]
  }else if(!is.vector(Colors) | length(Colors) != DIM){
    stop("Colors is no vector with correct length.")
  }
  
  
  Res = NULL
  MyPlot = plotly::plot_ly(type = "scatter", mode = "lines")
  if(DIM > 1){
    Res = list()
    for(i in 1:dim(Data)[2]){
      Res[[i]]    = ROCit::rocit(Data[,i], Cls, negref = NULL,
                                 method = "empirical", step = FALSE)
      MyPlot      = plotly::add_trace(p = MyPlot,
                                      x = Res[[i]]$FPR,
                                      y = Res[[i]]$TPR,
                                      name = Names[i],
                                      line=list(color=Colors[i]))
    }
  }else{
    Res    = ROCit::rocit(Data, Cls, negref = NULL,
                          method = "empirical", step = FALSE)
    MyPlot = plotly::add_trace(p = MyPlot,
                               x = Res$FPR,
                               y = Res$TPR,
                               name = Names,
                               line=list(color=Colors))
  }
  
  MyPlot = plotly::layout(p      = MyPlot,
                          title  = "ROC",
                          xaxis = list(title = "TPR",
                                       range = c(0, 1),
                                       fixedrange = TRUE,
                                       #scaleanchor="y",
                                       #scaleratio=1,
                                       zeroline = TRUE,
                                       showline = TRUE,
                                       showticklabels = TRUE,
                                       showgrid = TRUE),
                          yaxis = list(title = "FPR",
                                       range = c(0,1),
                                       fixedrange = TRUE,
                                       zeroline = TRUE,
                                       showline = TRUE,
                                       showticklabels = TRUE,
                                       showgrid = TRUE),
                          plot_bgcolor = "rgb(254, 254, 254)",
                          paper_bgcolor = "rgb(254, 254, 254)")
  return(list("ROCit" = Res,
              "Plot"  = MyPlot))
}
