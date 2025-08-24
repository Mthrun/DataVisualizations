Multiplot <- function(..., Plotlist=NULL, ColNo=1, LayoutMat, Plotter="native", main,main_fontsize=16) {
  
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
  numPlots <- length(plots)
  
#do nothing special if only one sublpo
  if (numPlots == 1) {
    print(plots[[1]])
    return(plots[[1]])
  }
  
  if (tolower(Plotter) == "ggplot2") {
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
    if (missing(main)) {
      if (missing(LayoutMat)) {
        p <- do.call(gridExtra::grid.arrange, c(plots, list(ncol = ColNo)))
      } else {
        p <- do.call(gridExtra::grid.arrange, c(plots, list(layout_matrix = LayoutMat)))
      }
    }else{
    
    # --- ONLY WHEN TITLE IS GIVEN: add a centered top title ---
    title_grob <- grid::textGrob(main, x = 0.5, hjust = 0.5,
                                 gp = grid::gpar(fontsize = main_fontsize, fontface = "bold"))
    if (missing(LayoutMat)) {
      p <- do.call(gridExtra::grid.arrange, c(plots, list(ncol = ColNo, top = title_grob)))
    } else {
      p <- do.call(gridExtra::grid.arrange, c(plots, list(layout_matrix = LayoutMat, top = title_grob)))
    }

    }
    return(invisible(p))
    
  } else if (tolower(Plotter) == "native") {
    # If LayoutMat is missing, define the panel estimating int via ColNo
    if (missing(LayoutMat)) {
      LayoutMat <- matrix(seq(1, ColNo * ceiling(numPlots/ColNo)),
                          ncol = ColNo, nrow = ceiling(numPlots/ColNo))
    }
    grid::grid.newpage()
    
    nrows <- nrow(LayoutMat)
    ncols <- ncol(LayoutMat)
    
    if (missing(main)) {
      
      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(LayoutMat == i, arr.ind = TRUE))
        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
      }
    
    }else{
    
    # --- ONLY WHEN TITLE IS GIVEN: insert a title row that spans all columns ---
    # Build a default layout matrix if not provided
    
    # Create a layout that adds one top row for the title
    grid::pushViewport(grid::viewport(
      layout = grid::grid.layout(
        nrow = nrows + 1,
        ncol = ncols,
        heights = grid::unit.c(
          grid::unit(1.8, "lines"),
          grid::unit(rep(1, nrows), "null")
        )
      )
    ))
      grid::grid.text(main, y = unit(0.98, "npc"), gp = grid::gpar(fontsize=main_fontsize, fontface="bold"))
    
    # Print each plot at its (row, col), offset by the title row (+1)
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(LayoutMat == i, arr.ind = TRUE))
      print(plots[[i]],
            vp = grid::viewport(layout.pos.row = matchidx$row + 1,
                                layout.pos.col = matchidx$col))
    }
  }#end ifmissing main
    return(invisible(list(Plotlist = plots)))
    
  } else {
    warning("Multiplot: Please select either native or ggplot2 as Plotter")
    return(NULL)
  }
}