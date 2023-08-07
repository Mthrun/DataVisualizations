ShepardDensityScatter = ShepardDensityPlot = function(InputDists,
                                                      OutputDists,
                                                      Plotter = "native",
                                                      Type = "DDCAL",
                                                      DensityEstimation="SDH",
                                                      Marginals = FALSE,
                                                      xlab='Input Distances',
                                                      ylab='Output Distances',
                                                      main='ProjectionMethod',
                                                      sampleSize=500000){
  # INPUT
  #    \item{InputDists}{[1:n,1:n] with n cases of data in d variables/features:
  #      Matrix containing the distances of the inputspace.
  #    }
  #    \item{OutputDists}{[1:n,1:n] with n cases of data in d dimensionalites of the projection method variables/features: 
  #    Matrix containing the distances of the outputspace.
  #    }
  #    \item{Plotter}{
  #    Optional, either \code{"native"} or \code{"plotly"}
  #    }
  #    \item{Type}{
  #    Optional, either \code{"DDCAL"} which creates a special hard color transition sensitive to density-based structures or \code{"Standard"} which creates a standard continuous color transition which is proven to be not very sensitive for complex density-based structures.
  #    }
  #    \item{DensityEstimation}{
  #    Optional, use either \code{"SDH"} or \code{"PDE"} for data density estimation.
  #    }
  #    \item{Marginals}{
  #    Optional, either TRUE (draw Marginals) or FALSE (do not draw Marginals)
  #    }
  #    \item{xlab}{
  #    Label of the x axis in the resulting Plot.
  #    }
  #    \item{ylab}{
  #    Label of the y axis in the resulting Plot.
  #    }
  #    \item{main}{
  #    Title of the Shepard diagram
  #    }
  #    \item{sampleSize}{Optional, default(500000),  reduces a.ount of data for density estimation, if too many distances given}
  #    }
  # 
  # OUTPUT
  # ggobject    ggplot2 or plotly object, if said plotter is chosen, NULL if native
  # Author: MT, 2017

  if(!is.matrix(InputDists)){
    warning('InputDists is not a matrix. Calling as.matrix()')
    InputDists=as.matrix(InputDists)
  }
  if(!is.matrix(OutputDists)){
    warning('OutputDists is not a matrix. Calling as.matrix()')
    OutputDists=as.matrix(OutputDists)
  }
  if(!mode(InputDists)=='numeric'){
    warning('InputDists is not a numeric matrix. Calling mode(InputDists)="numeric"')
    mode(InputDists)='numeric'
  }
  if(!mode(OutputDists)=='numeric'){
    warning('OutputDists is not a numeric matrix. Calling mode(OutputDists)="numeric"')
    mode(OutputDists)='numeric'
  }
  
  
  x=InputDists[lower.tri(InputDists, diag = FALSE)]
  y=OutputDists[lower.tri(OutputDists, diag = FALSE)]
  xn=length(x)
  yn=length(y)

  if(xn!=yn) stop('Number of distances is not equal')
  
 
  if(xn>sampleSize){
    ind=sample(size = sampleSize,replace = F,x = 1:xn)
    print('Too many distances. Drawing sample.')
		plot(x[ind],y[ind],xlab='Input',ylab='Output',main='shepard diagram')
    ggobject=DensityScatter(x[ind], y[ind],
                            DensityEstimation = DensityEstimation,
                            Plotter=Plotter,
                            Type = Type,
                            Marginals = Marginals,
                            xlab=xlab, ylab=ylab, main=main)
  }else{
    ggobject=DensityScatter(x, y,
                            DensityEstimation = DensityEstimation,
                            Plotter = Plotter,
                            Type = Type,
                            Marginals = Marginals,
                            xlab=xlab, ylab=ylab, main=main)
  }
  # ggobject=PDEscatter(x,y)
  # ggobject=ggobject+ylab('Output Distances')+xlab('Input Distances')+ggtitle(label)#+
  # if(fancy){
  #   ggobject=ggobject+
  #     theme(panel.background = element_blank(), legend.key = element_blank(),axis.line =element_line(colour='black'),
  #           axis.title.y = element_text(size = rel(2), angle = 90),
  #           axis.title.x = element_text(size = rel(2), angle = 00),
  #           axis.text.x = element_text(size = rel(2)),
  #           axis.text.y = element_text(size = rel(2)),
  #           plot.title =  element_text(size = rel(2))
  #     )+#coord_fixed(ratio=max(df$InDist)/max(df$OutDist))+
  #     coord_fixed(ratio=1)+
  #     # geom_line(data = df, aes(x = InDist, y = InDist),color='red',size=1.5)+
  #     coord_cartesian(expand=FALSE)
  # }
  return(ggobject)
}