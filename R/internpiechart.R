internpiechart=function(Datavector,Names,Labels,MaxNumberOfSlices,col){
#    internpiechart(Datavector,Names,Labels,main='',col,MaxPercentage=FALSE,ShrinkPies=0.05,Rline=1.1)
# 
#     intern function
#   
# 
# INPUT
#      \item{Datavector   [1:n] a vector of n non unique values
#      \item{Names   names to search for in Datavector, if not set \code{unique} of Datavector is calculated.
#      Labels   [1:k] Labels if they are specially named, if not Names are used.
#      MaxNumberOfSlices    integer, how many slices should be presented at maximum?
#      \item{main}{
#        title below the fan pie, see \code{plot}
#      }
#      \item{col}{default as other colors in this packages, else the same as in \code{plot}
#      }
# 
#      A normal pie plot is dificult to interpret for a human observer, because humans are not trained well to observe angles [Gohil, 2015, p. 102]. Therefore, the fan plot is used. As proposed in [Gohil 2015] the \code{fan.plot}() of the \code{plotrix} package is used to solve this problem.
# 
# # OUTPUT
#    silent output by calling \code{invisible} of a list with
#      \item{Percentages}{
#        [1:k] percent values visualized in fanplot
#      }
#      \item{Labels}{
#        [1:k] see input \code{Labels}, only relevant ones
#    Cols[1:k]
#   Names[1:k]

  
   #Datavector= checkFeature(Datavector,'Datavector')
  
  nas=which(is.na(Datavector))
  if(length(nas)>0){
    Datavector[nas]='Missing (NA)'
    print('Note: NA values found.')
  }
  if(is.numeric(Datavector)){
    nas=which(!is.finite(Datavector))
    if(length(nas)>0){
      Datavector=as.character(Datavector)
      Datavector[nas]='NaN'
      print('Note: Infinitive and/or NaN values found.')
    }
  }
  
  if(missing(Names))
    Names=sort(unique(Datavector),na.last = T)
  
  n=length(Datavector)

  m=length(Names)
  if(missing(Labels))
    Labels=as.character(Names)

  k=length(Labels)
  
  if(missing(MaxNumberOfSlices)){
    MaxNumberOfSlices=k
  }else{
    if(identical(FALSE,MaxNumberOfSlices)){
      MaxNumberOfSlices=k
    }
    if(!isTRUE(MaxNumberOfSlices)&!is.numeric(MaxNumberOfSlices)){
       warning('MaxNumberOfSlices could not be interpreted because it is not numeric or TRUE, using MaxNumberOfSlices=TRUE.')
      MaxNumberOfSlices=TRUE
    }
    if(length(MaxNumberOfSlices)<1|length(MaxNumberOfSlices)>1){
      MaxNumberOfSlices=TRUE
      warning('MaxNumberOfSlices could not be interpreted because it is a of length unequal 1, using MaxNumberOfSlices=TRUE.')
    }
    
    if(MaxNumberOfSlices<1){
      MaxNumberOfSlices=TRUE
      warning('MaxNumberOfSlices value below 1, using MaxNumberOfSlices=TRUE.')
    } 
    if(MaxNumberOfSlices>k){
      warning(paste('Setting MaxNumberOfSlices at number of unique values',k,'because its too high.'))
    }
  }
  if(m!=k){
    warning('Length if Names does not equal length of Labels, using names as Labels')
    Labels=as.character(Names)
  }

  allu=unique(Datavector)

  tmp=setdiff(allu,Names)
  if(length(tmp)>0){
    print('These Names where additionally in the Datavector;')
    print(tmp)
    indnonex=Datavector %in% tmp
    Datavector[indnonex]='other'
    Names=c(Names,'other')
    Labels=c(Labels,'other')  
    m=m+1
  }
  
  count=c()
  
  for(i in 1:m){
    tempind=Datavector==Names[i]
    count[i]=sum(tempind,na.rm = T)
  }

 
  indmissing=which(count==0)

  if(length(indmissing)>0){
    warning(paste0(length(indmissing),' Names could not be found and will not be shown in the fan plot.'))
    ind=which(count>0)
    if(length(ind)>0){
      count=count[ind]
      Labels=Labels[ind]
      Names=Names[ind]
    }
  }
  names(count)=Names



  pct=round((count/sum(count))*100,2)
  if(length(pct)>MaxNumberOfSlices){
	requireNamespace('ABCanalysis')
    abc=ABCanalysis::ABCanalysis(pct)
    if(!isTRUE(MaxNumberOfSlices)){
    if(length(abc$Aind)<MaxNumberOfSlices){#kein abc sondern einfach nuer die hoechsten
      print('Not enough slices in group A of ABCanalysis comparing to MaxNumberOfSlices. Adding slices of highest percentage following after group A.')
      pct2=sort(pct,decreasing = T,na.last = T)
      tempind=seq(from=(MaxNumberOfSlices+1),to=length(pct),by=1)
      pct_tmp=sum(pct2[tempind])
      pct2=pct2[seq(from=1,to=MaxNumberOfSlices,by=1)]
      uuu=unique(names(pct2))
      pct=c(pct2,pct_tmp)

    }
    
    if(length(abc$Aind)==MaxNumberOfSlices){#kein abc sondern einfach nuer die hoechsten
      pct2=pct[abc$Aind]
      uuu=unique(names(pct2))
      pct=c(pct2,(sum(pct[abc$Bind])+sum(pct[abc$Cind])))
    }
    
    if(length(abc$Aind)>MaxNumberOfSlices){ #abc gruppe zu gross, reduziere
      pct2=pct[abc$Aind]
      
      print('Too many slices in group A of ABCanalysis comparing to MaxNumberOfSlices. Selecting the most frequent subgoup out of group A.')
      
      pct2=sort(pct2,decreasing = T)
      pct_tmp=sum(pct2[seq(from=MaxNumberOfSlices+1,to=length(pct2),by=1)],na.rm = T)
      pct2=pct2[seq(from=1,to=MaxNumberOfSlices,by=1)]
      uuu=unique(names(pct2))
      pct=c(pct2,(sum(pct[abc$Bind])+sum(pct[abc$Cind])+pct_tmp))
    }
    }else{
      pct2=pct[abc$Aind]
      uuu=unique(names(pct2))
      pct=c(pct2,(sum(pct[abc$Bind])+sum(pct[abc$Cind])))
    }
    Labels=c(Labels[match(uuu,Names)],'Other')
  }
  LabelsOut=Labels
  knew=length(Labels)
  indother=which(Labels=='Other')
  if(length(indother)==1){
    names(pct)[indother]='Other'
  }
  Labels=paste0(Labels,': ',pct,"%")
  Labels=gsub('0%','<0.01%',Labels)
  if(missing(col)){
    colors=DataVisualizations::DefaultColorSequence[1:knew]
  }else{
    if(length(col)==knew)
      colors=col
    else{
      warning('Length of colors doesnt match found names defined as labels.')
      colors=DataVisualizations::DefaultColorSequence[1:knew]
    }
  }

   return(list(Percents=pct,Labels=Labels,Names=LabelsOut,Cols=colors))
}