JitterUniqueValues=function(Data,Npoints=20){
  
  vals=unique(Data)
  nu=length(vals)
  n=length(Data)
  Flag=FALSE
  if(is.null(Npoints)){#generate vector of length data specifically
    Npoints=round(n/nu,0)
    Flag=TRUE
  }else{
    if(Npoints<1) Npoints=1
  }
  
  DataJitter=c()
  for(v in vals){
    if(v!=0){
      x <- c(v, v * runif(Npoints-1, 0.99999, 1.00001))
     
    }else{
      x <- c(v, v + runif(Npoints-1, -0.00001, 0.00001))
    }
    DataJitter=c(DataJitter,x)
  }
  if(Flag){#check if rounding had some effect
    if(n!=length(DataJitter)){#
      DataJitter=sample(DataJitter,size = n,replace = T)
    }
  }
  return(DataJitter)
}