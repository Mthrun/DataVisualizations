GoogleMapsCoordinates=function(Longitude,Latitude,Cls=rep(1,length(Longitude)),zoom=3,location= c(mean(Longitude),mean(Latitude)),stroke=1.7,size=6,sequence){
#  GoogleMapsCoordinates(Longitude,Latitude,Cls)
#  Google Maps with marked coordinates 
# INPUT
# Longitude                 sphaerischer winkel der Kugeloberflaeche, coord 1
# Latitude                  sphaerischer winkel der Kugeloberflaeche,  coord 2
# OPTIONAL
# Cls                       Vorklassification/Clusterung
# zoom	                   map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city). 
#                           openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype. 
#                         "auto" automatically determines the zoom for bounding box specifications, and is defaulted to 10
#                           with center/zoom specifications. maps of the whole world currently not supported
# location                 default: c(mean(Longitude),mean(Latitude); an address, longitude/latitude pair (in that order), 
#                           or left/bottom/right/top bounding box
# stroke                    plotting parameter, dicke der linien der coordiantensymbole
# size                      plotting parameter, groesse der koordinatensymbole
# OUTPUT
# ggobject()              #falls ploz angepasst werden soll, ansosnten word nur geplottet
#author: MT 12/16
  
  if(!is.vector(Longitude)){
    Longitude=as.vector(Longitude)
    warning('Longitude is not a vector')
  }
  if(!is.vector(Latitude)){
    Longitude=as.vector(Latitude)
    warning('Latitude is not a vector')
  }
  if(!is.vector(Cls)){
  Longitude=as.vector(Cls)
  warning('Cls is not a vector')
  }
  if(!is.numeric(Cls)){
    Longitude=as.numeric(Cls)
    warning('Cls is not a numeric')
  }
  if(!is.numeric(Longitude)){
    Longitude=as.numeric(Longitude)
    warning('Longitude is not a numeric')
  }
  if(!is.numeric(Latitude)){
    Longitude=as.numeric(Latitude)
    warning('Latitude is not a numeric')
  }
  
  isnumber=function(x) return(is.numeric(x)&length(x)==1)  
  if(!isnumber(zoom))
    stop('"zoom" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(stroke))
    stop('"stroke" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(size))
    stop('"size" is not a numeric number of length 1. Please change Input.')
  
  Cls=checkCls(Cls,length(Longitude))
  
  requireNamespace('ggmap')
  requireNamespace('ggplot2')
  
   cpd=cbind(Longitude,Latitude,Cls)
  

  map <- ggmap::get_map(location = location, zoom = zoom)
  

  cpd = data.frame(cpd)
  #data(DefaultColorSequence)
  colnames(cpd) <- c('longitude','latitude','cluster')
  # a bad hack against no visible global binding:-(
  # variables exist in cpd
  longitude=NULL
  latitude=NULL
  cluster=NULL
  
  n=length(unique(Cls))
#sequenz=c(1:(n-1),n+13)
  if(missing(sequence))
	  sequenz=c(1:n)
  else
	  sequenz=sequence
  

  Colors=DataVisualizations::DefaultColorSequence[sequenz]
  map2=ggmap::ggmap(map) + ggplot2::geom_point(aes(x = longitude, y = latitude,color = factor(cluster),shape  =factor(cluster)),data=cpd, alpha = 0.8,size=size,stroke=stroke)+
    ggplot2::scale_shape_manual(values=sequenz, name='Clusters')+
    ggplot2::scale_color_manual(values=Colors, name='Clusters')+theme_bw()
  
  plot(map2)
  return(map2)
}