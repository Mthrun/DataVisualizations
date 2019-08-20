 Heatmap=function(DataOrDistances,Cls,method='euclidean',LowLim=0,HiLim){
# Heatmap(DataOrDistances,Cls,Names,LowLim,HiLim) 
# Heatmap: Distances of DataOrDistances sorted by Cls
# INPUT
# DataOrDistances                    [1:n,1:d] data cases in rows, variables in columns oder [1:n,1:n] distances
# OPTIONAL
# Cls                   numeric vector, [1:n,1]  classified data 
# Distanz               see DistanceMatrix(...,method):
#
# LowLim,HiLim  limits for the color axis as in PlotPixMatrix

# author: MT 08/2016, edited 28.01.2018
   #2.Editor: MT 06/18
   requireNamespace('parallelDist')
   
   if(!is.matrix(DataOrDistances)){
     message('DataOrDistances is not a matrix. Calling as.matrix()')
     DataOrDistances=as.matrix(DataOrDistances)
   }
   if(!mode(DataOrDistances)=='numeric'){
     warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
     mode(DataOrDistances)='numeric'
   }
   AnzData = nrow(DataOrDistances)
   if (missing(Cls))
     Cls = rep(1, AnzData)
   
   Cls=checkCls(Cls,AnzData)
   #print(Cls)
   #MT: Reihenfolge muss unbedingt fest sein ab hier, sie unten
   SortOrder=FALSE
   ind = order(Cls,decreasing = SortOrder,na.last = T)
   
   if (isSymmetric(DataOrDistances)) {
     # nach Cls sortieren
     DataOrDistances = DataOrDistances[ind, ]
     DataOrDistances = DataOrDistances[, ind]
     AnzVar = ncol(DataOrDistances)
     DataDists = DataOrDistances
   }
   else{
      message('Distances are not in a symmetric matrix, Datamatrix is assumed and dist() ist called')
     
     AnzVar = ncol(DataOrDistances)
     
     # nach Cls sortieren
     DataOrDistances = DataOrDistances[ind, ]
     #DataDists = as.matrix(dist(DataOrDistances, method = method, diag =TRUE))
     DataDists=as.matrix(parallelDist::parDist(DataOrDistances[ind, ],method = method))
     #DataDists = DistanceMatrix(DataOrDistances, method = method)
   }

   if (missing(HiLim)){}
     HiLim = max(DataDists,na.rm=T)
     
  isnumber=function(x) return(is.numeric(x)&length(x)==1)  
   if(!isnumber(HiLim))
     stop('"HiLim" is not a numeric number of length 1. Please change Input.')
   
   if(!isnumber(LowLim))
     stop('"LowLim" is not a numeric number of length 1. Please change Input.')
   # Zeichnen
   #MT: sollte in der selbenreihenfolge sein wie anordnung der cls welche daten anordnet
   Vunique = sort(unique(Cls),decreasing = SortOrder,na.last = T)
   

   # Klassen Unterteilungslinien anbringen
   if (length(Vunique) > 1) {
       countPerClass <- rep(0, length(Vunique))
    for (i in 1:length(Vunique)) {
        inClassI <- sum(Cls == Vunique[i])
        countPerClass[i] = inClassI
    }
     ClassSepLines = cumsum(countPerClass) + 0.5

     #does not look good to change the color of the seperating lines of the clusters
     # and it is not applicable to color labels with multiple colors
    #  cols=c('black','coral','gray41','lightpink1','darksalmon','magenta','rosybrown2','thistle','wheat4','mistyrose1')
      cnn=length(ClassSepLines)
    #  if(cnn<=length(cols)){
    #   cols=cols[1:cnn]
    # }else{
    #    cols=rep('black',cnn)
    # }
      cols=rep('black',cnn)
   } 
     plt = Pixelmatrix(DataDists, c(), LowLim, HiLim) +
       ylab(paste('|Cls No', Vunique[order(Vunique, decreasing = !SortOrder)], '| ', collapse = '')) +
       xlab(paste('|Cls No', Vunique, '| ', collapse = '')) +
       ggtitle('Distances of DataOrDistances sorted by Cls')+
       theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_blank())
     
     if (length(Vunique) > 1) {
      plt = plt + geom_hline(yintercept = ClassSepLines,color=cols) + geom_vline(xintercept = ClassSepLines,color=cols)
     }
   print(plt)
   return(invisible(plt))
}                    