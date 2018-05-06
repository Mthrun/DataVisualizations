DrawWorldWithCls <- function(CountryCode, Cls, JoinCode = "UN", Title = "Default Title", 
                             Colors = DataVisualizations::DefaultColorSequence){
  # DrawWorldWithCls(CountryCode, Cls)
  # INPUT
  # CountryCode     Vector of Countrys belonging to the Cls
  # Cls             Classes belonging to the Countries from CountryCode
  # JoinCode        System that is used for the CountryCodes.
  #                 Possible are: "ISO3", "UN"
  # Title           will be plotted above worldmap
  # Colors          Vector that colors for classes will be selected from
  # 
  # OUTPUT
  # --
  # Author: FL
  requireNamespace("rworldmap")
  requireNamespace("grDevices")
  Cls=checkCls(Cls,length(CountryCode))
  # pruefe ob CountryCodes unique sind
  x <- unique(CountryCode)
  if(length(x) != length(CountryCode)) stop("Your CountryCodes are not unique!")
  if(length(CountryCode) != length(Cls)) stop("Length of CountryCode and Cls do not match!")
   
  # Datenframe zusammen bauen
  df = data.frame(CountryCode = CountryCode, Cls = Cls)
  
  NrOfClasses = length(unique(Cls))
  
  op <- Colors[1:NrOfClasses]
  
  sPDF <- rworldmap::joinCountryData2Map(df, joinCode = JoinCode, nameJoinColumn = "CountryCode")
  rworldmap::mapCountryData( sPDF, nameColumnToPlot="Cls", catMethod = 'categorical',
                  mapTitle = Title, colourPalette = op,addLegend=T)
}



