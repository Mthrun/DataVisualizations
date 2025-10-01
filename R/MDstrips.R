MDstrips = function(Data, palette = c("blue","green","yellow","orange","red"),ylab,main,BW=FALSE) {
  all_strips = list()
  if(missing(ylab)){
    ylab="Range of values in which PDE is estimated"
  }
  nbins=100
  for (var in colnames(Data)) {
    Feature = Data[, var]
    Feature = Feature[is.finite(Feature)]
    if (length(Feature) < 5) next
    
    par=ParetoRadius_fast(Feature)
    densV = ParetoDensityEstimation(Feature,MinAnzKernels = nbins,paretoRadius=par,Compute = "Cpp_exp")
    
    d_norm = (densV$paretoDensity - min(densV$paretoDensity)) / (max(densV$paretoDensity) - min(densV$paretoDensity))
    
    all_strips[[var]] = data.frame(Variable = var,
                                    x = densV$kernels,
                                    PDE = d_norm)
  }
  
  df_strips = do.call(rbind, all_strips)
  
  obj=ggplot2::ggplot(df_strips, ggplot2::aes(x = Variable, y = x, fill = PDE)) +
    ggplot2::geom_tile(height = (max(df_strips$x) - min(df_strips$x)) / nbins,
                       width = 1) +
    ggplot2::scale_fill_gradientn(colors = palette) +
    ggplot2::ylab(ylab) + ggplot2::xlab("Variables")+
    ggplot2::scale_x_discrete(expand = c(0, 0)) +     # no gaps between variables
    ggplot2::scale_y_continuous(expand = c(0, 0))   # no outer padding
  
  if(isTRUE(requireNamespace("ggExtra",quietly = TRUE))){
    obj=obj+ggExtra::rotateTextX()
  }else{
    warning('Package ggExtra is not installed. Labels of Variablenames are not rotated.')
  }
  if(!missing(main)){
    obj=obj+ggplot2::ggtitle(main)
  }
    if(isTRUE(BW))
      obj=obj+ggplot2::theme_bw()
    
  return(obj)
}
