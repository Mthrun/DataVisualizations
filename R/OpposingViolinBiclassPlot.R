OpposingViolinBiclassPlot = function(ListData, Names, BoxPlots = FALSE,
                                     Subtitle = c("AttributeA", "AttributeB"),
                                     Title = "Opposing Violin Biclass Plot"){
  # INPUT
  # ListData    List of k matrices as elements where each element has shape [1:n, 1:2]
  # Names       Vector of character names for each element of ListData
  # 
  # OPTIONAL
  # BoxPlots    Boolean: BoxPlots = TRUE shows a box plot drawn into the violin
  #             plot. BoxPlots = FALSE shows no box plot. Default: BoxPlots = FALSE
  # Subtitle[1:2]    Vector of character names for two classes. The classes
  #                  are described as features contained in the matrix [1:n, 1:2]
  # Title       Character containing the title of the plot.
  # 
  # OUTPUT
  # plotly object
  # 
  # Author: QMS 31.10.2023
  
  if(!requireNamespace("plotly", quietly = TRUE)){
    stop("OpposingViolinBiclassPlot.R: requires R package 'plotly'!")
  }
  # TODO:
  # Subtitel beider Spalten fuer blau und rot
  # Assertions -----------------------------------------------------------------
  if(!is.list(ListData)){
    stop("ListData is not a list")
  }
  
  NumPlots = length(ListData)
  NumObs = c()
  for(i in 1:NumPlots){
    if(!is.matrix(ListData[[i]])){
      stop("Elements of ListData are not matrices")
    }
    if(dim(ListData[[i]])[2] != 2){
      stop("Elements of ListData have not all two features")
    }
    NumObs = c(NumObs, dim(ListData[[i]])[1])
  }
  if(!all(NumObs == rep(sum(NumObs)/length(NumObs), NumPlots))){
    stop("Not all matrices in ListData have the same number of observations")
  }
  # End Assertions -------------------------------------------------------------
  MyData1 = cbind()
  for(i in 1:NumPlots){
    MyData1 = cbind(MyData1, ListData[[i]][,1])
  }
  MyData2 = cbind()
  for(i in 1:NumPlots){
    MyData2 = cbind(MyData2, ListData[[i]][,2])
  }
  
  fig1 = plotly::plot_ly(type = "violin", spanmode = "hard", box = list(visible = BoxPlots))
  for(i in 1:NumPlots){
    fig1 = plotly::add_trace(p = fig1, x = MyData1[,i], showlegend = FALSE, name = Names[i], color = I("darkred"))
  }
  fig1 = plotly::layout(p = fig1, xaxis = list(autorange = "reversed", range = c(0, 100)))
  
  fig2 = plotly::plot_ly(type = "violin", spanmode = "hard", box = list(visible = BoxPlots))
  for(i in 1:NumPlots){
    fig2 = plotly::add_trace(p = fig2, x = MyData2[,i], showlegend = FALSE, name = Names[i], color = I("darkblue"))
  }
  fig2 = plotly::layout(p = fig2, xaxis = list(range = c(0, 100)))
  
  MyPlot = plotly::subplot(fig1, fig2, nrows = 1)
  MyPlot = plotly::layout(p = MyPlot,
                          title = list(text = Title, font = list(size = 24)),
                          yaxis2 = list(anchor = "x1", zeroline = TRUE),
                          margin = list("t" = -0.05),
                          annotations = list(
                            list(x = 0.15 , y = 1.05, text = Subtitle[1], showarrow = F, xref='paper', yref='paper'),
                            list(x = 0.85 , y = 1.05, text = Subtitle[2], showarrow = F, xref='paper', yref='paper')))
  return(MyPlot)
}

