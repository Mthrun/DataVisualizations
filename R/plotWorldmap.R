plotWorldmap=function(CountryCodes,Cls,Colors,MissingCountryColor=grDevices::gray(.80),...){
  
  requireNamespace('sp')
  #requireNamespace('maps')
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

  sp:::plot.SpatialPolygons(world_country_polygons,col =cols,...)
  #plot.SpatialPolygons(world_country_polygons,col =cols,...)
  #plot(world_country_polygons,col =cols,...)
  return(invisible(list(Colors=cols,CountryCodeList=LandList,world_country_polygons=world_country_polygons)))
}