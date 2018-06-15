InspectDistances=function(DataOrDistances,method= "euclidean",sampleSize = 50000,...){
  # InspectDistance(DataOrDistances,method)
  # visualizes the distances between objects in the data matrix 
  # using the method specified by distance, which can be any of the following character strings
  #
  # INPUT
  # DataOrDistances[d,n]     Daten bestehend aus d Datensaetzen/Werten/Zeilen von n Vektoren/Variablen/Spalten ohne NaN
  #                Distanz wird jeweils zwischen zwei Zeilen berechnet
  #           
  # Optional         
  # method          method specified by distance string: 
  #                 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev'=max(abs(x-y)),'jaccard','minkowski','manhattan','binary', 'canberra'=sum abs(x-y)/sum(abs(x)-abs(y)), 'maximum', 'braycur'=sum abs(x -y)/abs(x+y)
  #author: MT 06/16
  #1.Editor: MT 06/18
  requireNamespace('parallelDist')
  
  if(!is.matrix(DataOrDistances)){
    warning('DataOrDistances is not a matrix. Calling as.matrix()')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  if(!mode(DataOrDistances)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
    mode(DataOrDistances)='numeric'
  }
  if (isSymmetric(DataOrDistances)) {
    # nach Cls sortieren
    InputDistances = DataOrDistances
  }
  else{
    print('Distances are not in a symmetric matrix, Datamatrix is assumed and dist() ist called')
    #InputDistances = as.matrix(dist(DataOrDistances, method = method, diag =TRUE))
    InputDistances=as.matrix(parallelDist::parDist(DataOrDistances,method = method))
    #DataDists = DistanceMatrix(DataOrDistances, method = method)
  }
  
  vecdist=InputDistances[upper.tri(InputDistances,F)]
  # if(method!='euclidean')
  InspectVariable(vecdist,N = paste(method,'distance'),sampleSize=sampleSize,...)
  # else
  #   InspectVariable(vecdist,sampleSize = 10000,N = paste('Euclidean','distance'),main=main)
  # 
}