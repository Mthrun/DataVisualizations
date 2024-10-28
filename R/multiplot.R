
Multiplot <- function(..., Plotlist=NULL, ColNo=1, LayoutMat,Plotter="native") {

  if (!requireNamespace('grid', quietly = TRUE)) {
    message(
      'Subordinate package (grid) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Plotlist = "Subordinate package (grid) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), Plotlist)

  numPlots = length(plots)
#do nothing special if only one sublpo
  if (numPlots==1) {
    print(plots[[1]])
    return(plots[[1]])
  } 
  

  if(tolower(Plotter)=="ggplot2"){
    if (!requireNamespace('gridExtra', quietly = TRUE)) {
      message(
        'Subordinate package (gridExtra) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
      )
      return(
        list(
          Plotlist = "Subordinate package (gridExtra) is missing.
                Please install the package which is defined in 'Suggests'."
        )
      )
    }
    if(missing(LayoutMat)){
      p <- do.call(gridExtra::grid.arrange, c(plots, list(ncol = ColNo)))
    }else{
      p <- do.call(gridExtra::grid.arrange, c(plots, list(layout_matrix = LayoutMat)))
    }
 
    #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
    
    #end for plotte ggplot2
  }else if(tolower(Plotter)=="native"){
    

  # If LayoutMat is missing,  define the panel estimating int via ColNo
  if (missing(LayoutMat)) {
    LayoutMat <- matrix(seq(1, ColNo * ceiling(numPlots/ColNo)),
                    ncol = ColNo, nrow = ceiling(numPlots/ColNo))
  }

    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(LayoutMat), ncol(LayoutMat))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # identify the i,j matrix positions of the area that contain this subplot
      matchidx <- as.data.frame(which(LayoutMat == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
      
    }

  return(invisible(list(Plotlist=plots)))
    #end ploter native
  }else{
    #do nothing special if only one sublpo
    warning("Multiplot: Please select either native or ggplot2 as Plotter")
      return(NULL)
  } 
  
}