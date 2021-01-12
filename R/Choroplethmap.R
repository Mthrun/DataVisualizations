Choroplethmap=plotChoroplethMap=function(Counts,PostalCodes,NumberOfBins=0,Breaks4Intervals,percentiles=c(0.5,0.95),digits=0,PostalCodesShapes,PlotIt=TRUE,DiscreteColors,HighColorContinuous='red',LowColorContinuous='deepskyblue1',NAcolor='grey',ReferenceMap=FALSE, main='Political Map of Germany',legend='Range of values',Silent=TRUE){
#plotChoroplethMap(Counts, PostalCodes, NumberOfBins = 0,                     Breaks4Intervals, percentiles = c(0.5, 0.95),                      digits = 0, PostalCodesShapes, PlotIt = TRUE,                     DiscreteColors, HighColorContinuous = "red",                     LowColorContinuous = "deepskyblue1", NAcolor = "grey",                     ReferenceMap = FALSE, main = "Political Map of Germany",                     legend = "Range of values", Silent = TRUE)

#   Plot the Choropleth Map
#   A thematic map with areas colored in proportion to the measurement of the statistical variable being displayed on the map. A political map geneated by this function was used in the conference talk of the publication [Thrun/Ultsch, 2018]}

#INPUT
#   Counts
#     vector [1:m], statistical variable being displayed
#   
#   PostalCodes
#     vector[1:n], currently german postal codes (zip codes), if \code{PostalCodesShapes} is not changed manually, does not need to be unique
#   
#   NumberOfBins
#     Default: 1; 1 or below continously changes the color as defined by the   package \code{choroplethr}.
#     A Number between 2 and 9 sets equally sized bins. Higher numbers are not allowed
#   
#   Breaks4Intervals
#     If NumberOfBins>1 you can set here the intervals of the bins manually
#   
#   percentiles
#     If NumberOfBins>1 and Breaks4Intervals not set, then the percentiles of min and max bin can be set here. See also \code{quantile}.
#   
#   digits
#     number of digits for \code{round}
#   
#   PostalCodesShapes
#     Specially prepared shape file with postal codes and geographic boundaries.
#     If you set this object, then you can use non german zip codes.
#     You can see the required structure in map.df,  github trulia choroplethr blob master r chloropleth. 
#     
#   
#   PlotIt
#     Either Plot the map directly or change the object manually before plotting it
#   
#   DiscreteColors
#     Set the discrete colors manually if NumberOfBins>1, else it is ignored
#   
#   HighColorContinuous
#     if NumberOfBins<=1: color of highest continuous value, else it is ignored
#   
#   LowColorContinuous
#     if NumberOfBins<=1: color of lowest continuous value, else it is ignored
#   
#   NAcolor
#     Color of NA values in the map (postal codes without any counts)
#   
#   ReferenceMap
#     TRUE: With Google map, FALSE: without Google map
#   
#   main:
#     title of plot
#   
#   legend
#     title of legend
#   
#   Silent
#     TRUE: disable warnings of \code{choroplethr} package
#     FALSE: enable  warnings of \code{choroplethr} package


# OUTPUT
#   List of
#   chorR6obj         An R6 object of the package \code{choroplethr}}
#   DataFrame   Transformed PostalCodes and Counts in a way that they can be used in the package \code{choroplethr}.}
# 
# details
#   The \code{choroplethr} package is not documented well. This wrapper enables  to visualize an map much more easy in the case of german zip codes. Other postal codes are in principle usable.
# 
# author:   Michael Thrun
# 
  
  if (!requireNamespace('choroplethr',quietly = TRUE)){
    
    message('Subordinate package (choroplethr) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
    
    return('Subordinate package (choroplethr) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
  }
  if(length(Counts)!=length(PostalCodes)) warning('Length of PostalCodes and Counts should match')
  if(max(percentiles)>1) stop('percentiles have to between zero and 1.')
  if(percentiles[1]>percentiles[2]) stop('first percentile has to be smaller then second.')
  
  if(NumberOfBins>8){
    NumberOfBins=8
    warning('choroplethr package does not support more than 8 bins.')
  }

  #map.df, see guthub trulia choroplethr blob master r chloropleth
  if(missing(PostalCodesShapes))
    PostalCodesShapes=DataVisualizations::GermanPostalCodesShapes

  df=data.frame(PLZ=PostalCodes,Counts=Counts)
  
  df2=stats::aggregate(Counts~PLZ,df,min)
  colnames(df2) = c("region", "value")
  df2$region=as.character(df2$region)

  #NumberOfBins=AdaptGauss::OptimalNoBins(Counts)
  #NumberOfBins = min(100,NumberOfBins) #
  if(NumberOfBins>1){
    if(missing(Breaks4Intervals)){
      MinD=quantile(df2$value,percentiles[1])
      MaxD=quantile(df2$value,percentiles[2])
      Breaks4Intervals <- seq(MinD, MaxD, abs(MinD-MaxD)/NumberOfBins) # bins haben alle die gleiche groesse
    }
    if(length(Breaks4Intervals)>9){
      warning('Breaks4Intervals is not supported of a length higher than 9, because choroplethr package does not support more than 8 bins. Setting equal length bins.')
      MinD=quantile(df2$value,percentiles[1])
      MaxD=quantile(df2$value,percentiles[2])
      Breaks4Intervals <- seq(MinD, MaxD, abs(MinD-MaxD)/NumberOfBins) # bins haben alle die gleiche groesse
    }
    
    df2$breaks =findInterval(df2$value,Breaks4Intervals)
    u=sort(unique(df2$breaks),decreasing = FALSE)
    Names=c()
    for(i in 1:length(Breaks4Intervals)-1)
     Names[i]=paste0('Bin',i,': ',round(Breaks4Intervals[i],digits),' - ',round(Breaks4Intervals[i+1],digits))
   
    df2$breaknames=""
    if(u[1]==0){ #werte kleiner erster grenze
      df2$breaknames[df2$breaks==0]=paste0('Bin',0,': ',round(min(df2$value),digits),' - ',round(Breaks4Intervals[1],digits))
      for(i in 2:(length(u)-1)){
        df2$breaknames[df2$breaks==u[i]]=Names[i]
      }
    }else{
      for(i in 1:(length(u))-1){
        df2$breaknames[df2$breaks==u[i]]=Names[i]
      }
    }
    df2$breaknames[df2$breaks==tail(u,1)]=paste0('Bin',length(Breaks4Intervals),': ',round(tail(Breaks4Intervals,1),digits),' - ',round(max(df2$value),digits))
    
    df2$valueReal=df2$value
    df2$value=df2$breaknames
  }
  if(Silent)
    chorR6obj=choroplethr::Choropleth$new(PostalCodesShapes,df2)
  else
    chorR6obj=choroplethr::Choropleth$new(PostalCodesShapes,df2)
  #plot the data
  chorR6obj$ggplot_polygon = geom_polygon(aes_string(fill = 'value'), color = NAcolor)
  chorR6obj$title = main
  chorR6obj$legend= legend

  if(NumberOfBins<=1){
    chorR6obj$set_num_colors(1)
    chorR6obj$ggplot_scale = scale_fill_continuous(low = LowColorContinuous,high = HighColorContinuous,na.value = NAcolor)
  }else{
    if(missing(DiscreteColors))
      DiscreteColors=DataVisualizations::DefaultColorSequence[1:length(unique(df2$value))]
    
    if(length(DiscreteColors)>length(unique(df2$value))){
      DiscreteColors=head(DiscreteColors,length(unique(df2$value)))
      warning(paste0('Too many DiscreteColors: ',length(DiscreteColors),' for number of bins: ',length(unique(df2$value))))                          
    }
    chorR6obj$set_num_colors(length(DiscreteColors))
    chorR6obj$ggplot_scale = scale_fill_manual(name=legend,values=DiscreteColors, drop=FALSE,na.value = NAcolor)
  }
  chorR6obj$discretize() 
  if(PlotIt){
    if(ReferenceMap){
      if(Silent)
        print(suppressWarnings(chorR6obj$render_with_reference_map()))
      else
       print(chorR6obj$render_with_reference_map())
    }else{
      if(Silent)
        print(suppressWarnings(chorR6obj$render()))
      else
        print(chorR6obj$render())
    }
  }  
  return(invisible(list(chorR6obj=chorR6obj,DataFrame=df2)))
}



