CleanColNames=function(DF){
  names=colnames(DF)
  names=gsub(' ','_',names)
  names=gsub('\\(','_',names)
  names=gsub('\\)','',names)
  names=gsub('\\[','_',names)
  names=gsub('\\]','',names)
  Euro = intToUtf8(8364)
  names=gsub(Euro,'_Eur',names)      #  € = intToUtf8(8364), utf8ToInt("€") <=> 8364
  ae = intToUtf8(228)
  names=gsub(ae,'ae',names)          # ae = intToUtf8(228), utf8ToInt("ä") <=> 228
  ue = intToUtf8(252)
  names=gsub(ue,'ue',names)
  oe = intToUtf8(246)
  names=gsub(oe,'oe',names)
  Ae = intToUtf8(196)
  names=gsub(Ae,'Ae',names)
  Ue = intToUtf8(220)
  names=gsub(Ue,'Ue',names)
  Oe = intToUtf8(214)
  names=gsub(Oe,'Oe',names)
  names=gsub('/','_Or_',names)
  names=gsub('-','_',names)
  names=gsub('\\.','',names)
  colnames(DF)=names
  return(DF)
}