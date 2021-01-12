DiagnosticAbility4Classifiers=function(TrueCondition_Cls,ManyPredictedCondition_Cls,NamesOfConditions=NULL,PlotType='PRC', xlab = "True Positive Rate", ylab = 'False Positive Rate', 
                                         main = "ROC Space", Colors,LineColor=NULL,Size=8,LineWidth=1,LineType=NULL,Showgrid=TRUE, SaveIt = FALSE){
  #https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0118432
  
  if(length(TrueCondition_Cls)!=nrow(ManyPredictedCondition_Cls)) stop(
    'unequal length of classifications'
  )

  
  
  X=c()
  Y=c()
  for(col in 1:ncol(ManyPredictedCondition_Cls)){
    PredictedCondition=ManyPredictedCondition_Cls[,col]
    cM=as.matrix(table(PredictedCondition,TrueCondition_Cls)) #confusion matrix
  #up/columns: TrueConditon_Cls
  #left/rows: PredictedConditon_Cls
  diag = diag(cM)
  rowsums = apply(cM, 1, sum)
  colsums = apply(cM, 2, sum)

  if(all(dim(cM)==2)){
    #precision,positive predictive power, positive predictive value 
    prec=cM[1,1]/rowsums[1] #true positive/Predicted condition positive=true ppsitive/(true positive+false positive)
    #FDR=1-prec#false discovery rate
  
    # recall #sensitivity,  true psotive rate
    rec=cM[1,1]/colsums[1]#true positive/condition positive=true positive/(true positive+false negative)
    #FNR=1-rec #false negative rate
  
    #False positive rate
    FPR=cM[1,2]/colsums[2]
    #true negative rate
    specifity=cM[2,2]/colsums[2]#true negative/(True negative+false positive)
  }else{
    print('Currently only for bi-classifications developed')
  #Generalization under development
  # n = sum(cM)
  # nc = nrow(cM)

  # p = rowsums/n
  # q = colsums/n
  # precision = diag/colsums #positive predictive power
  # specifity= #true negative rate
  # recall = diag/rowsums #sensitivity
  }
    switch (PlotType,
      'PRC' = {
          X[col]=prec
          Y[col]=rec
          xlab='Precision'
          ylab='Recall'
          main='Precision Recall Plot'
      },
      'ROC'={
          X[col]=FPR
          Y[col]=rec
          ylab='True Positive Rate'
          xlab='False Positive Rate'#
          main='ROC'
      },{
        'SenSpec'={
          X[col]=rec
          Y[col]=specifity
          ylab='Sensitivity'
          xlab='Specifity'
          main='Sensitivity-Specifity Plot'
        }
      }
    )

  }#end for condition cols
  if(missing(Colors)){
    Colors=DataVisualizations::DefaultColorSequence[1:length(X)]
  }
  Cls=1:length(X)
  fig=Classplot(X, Y,Cls,Names=NamesOfConditions, xlab = xlab, ylab = ylab, 
                     main = main, Colors,LineColor=LineColor,Size=Size,LineWidth=LineWidth,LineType=LineType,Showgrid=Showgrid, SaveIt = FALSE)
  
  if(PlotType=="ROC"){
    # initiate a line shape object
    line <- list(
      type = "line",
      dash = 'dot',
      line = list(color = "grey"),
      xref = "x",
      yref = "y"
    )
    line[["x0"]] <- 0
    line[["x1"]] <- 1
    line[["y0"]] <- 0
    line[["y1"]] <- 1
    lines <- c(lines, list(line))
    fig <- plotly::layout(fig, shapes = lines)
  }
  fig
  if (SaveIt) {
    if (!requireNamespace('htmlwidgets',quietly = TRUE)){
    
    message('Subordinate package (htmlwidgets) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
    
    return('Subordinate package (htmlwidgets) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
  }
    savename=paste0( PlotType,".html")
    htmlwidgets::saveWidget(fig, file =savename)
  }
  return(list(Plot=fig,X=X,Y=Y))
}