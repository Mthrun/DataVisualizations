Worldmap=plotWorldmap=function(CountryCodes,Cls,Colors,MissingCountryColor=grDevices::gray(.80),...){
  
  if (!requireNamespace('sp',quietly = TRUE)){
    
    message('Subordinate package (sp) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
    
    return('Subordinate package (sp) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
  }

  u=unique(Cls)
  LandList=c()
  if(missing(Colors))
    Colors=DataVisualizations::DefaultColorSequence[1:length(u)]

  world_country_polygons=DataVisualizations::world_country_polygons
  cols=rep(MissingCountryColor,length(world_country_polygons@data$ISO3))
  for(i in 1:length(u)){
      y=CountryCodes[Cls==u[i]]
      LandList=c(LandList,y)
      myCountries = world_country_polygons@data$ISO3 %in% y
      cols[myCountries]<- Colors[i]
      CountryCodes[Cls==u[i]]
  }
  
 
  #sp::plot.SpatialPolygons(world_country_polygons,col =cols,...) #a note in cran check is generated with this, thats not allowed. But the call works!
  


  #get('plot.SpatialPolygons', envir = asNamespace('sp'), inherits = FALSE)(world_country_polygons,col =cols,...)  #work around of Yihui -_-
  plot(world_country_polygons,col =cols,...) #https://stat.ethz.ch/pipermail/r-package-devel/2018q2/002694.html 
  return(invisible(list(Colors=cols,CountryCodeList=LandList,world_country_polygons=world_country_polygons)))
}