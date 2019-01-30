CleanColNames=function(DF){
  names=colnames(DF)
  names=gsub(' ','',names)
  names=gsub('\\(','_',names)
  names=gsub('\\)','',names)
  names=gsub('€','_Eur',names)
  names=gsub('ä','ae',names)
  names=gsub('ü','ue',names)
  names=gsub('ö','oe',names)
  names=gsub('Ä','Ae',names)
  names=gsub('Ü','Ue',names)
  names=gsub('Ö','Oe',names)
  names=gsub('/','Or',names)
  names=gsub('-','_',names)
  names=gsub('\\.','',names)
  colnames(DF)=names
  return(DF)
}