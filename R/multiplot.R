
Multiplot <- function(..., Plotlist=NULL, ColNo=1, LayoutMat) {

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
}