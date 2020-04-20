SignedLog=function (Data, Base="Ten") 
{
  #author:MT 11/2018
  Absolut = abs(Data)
  Signed = sign(Data)
  
  #Catch numeric in switch
  if(is.numeric(Base)){
    Base=as.character(Base)
  }
  switch(Base,
         "Two"={
           LogedData = Signed * log2(Absolut + 1)
         },
         "Zero"={
           LogedData = Signed * log1p(Absolut)
         },
         "Ten"={
           LogedData = Signed * log10(Absolut + 1)
         },
         "2"={
           LogedData = Signed * log2(Absolut + 1)
         },
         "0"={
           LogedData = Signed * log1p(Absolut)
         },
         "10"={
           LogedData = Signed * log10(Absolut + 1)
         },
         {
           if(!is.finite(as.numeric(Base))) Base=10
           
           LogedData = Signed * log(Absolut,as.numeric(Base))*log(as.numeric(Base))
         }
    
  )

  return(LogedData)
}