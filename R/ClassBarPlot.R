ClassBarPlot = function(Values, Cls, Deviation, Names, ClassColors,
                        ylab = "Values", xlab = "Instances", PlotIt = TRUE){
  # Classbarplot(Values, Class)
  # Classbarplot(Values, Class, Deviation, Names)
  # 
  # DESCRIPTION
  # Represent values for each class and instance as bar plot with optional
  # error deviation, e.g., mean values of features depending on class with
  # standard deviation.
  # 
  # INPUT
  # Values[1:n]        Numeric vector with values (y-axis) in matching order to 
  #                    Class, Deviation and Names
  # Cls[1:n]           Numeric vector of classes in matching order to Values
  #                    and Deviation and Names
  # 
  # OPTIONAL
  # Deviation[1:n]     Numeric vector with deviation in matching order to Values
  #                    and Class and Names
  # Names[1:n]         Character or numeric vector of instances (x-axis) in
  #                    matching order to Values and Class and Deviation
  # ylab               Character stating y label.
  # xlab               Character stating x label.
  # PlotIt             Logical value indicating visual output
  #                    TRUE => create visual output
  #                    FALSE => do not create visual output
  #                    (Default: Boolean=TRUE)
  # OUTPUT
  # ggplot2 object for saving or further manipulation.
  # 
  # Author: QMS October 2024
  # 1. Editor: MCT, 2026 (several logical bug fixes)
  

  if (length(Values) != length(Cls)) {
    stop("Length of vectors Values and Cls must equal.")
  }
  
  if (!is.null(Deviation) && length(Deviation) != length(Values)) {
    stop("Length of vectors Values and Deviation must equal.")
  }
  
  NumCls <- length(unique(Cls))
  
  # same number of values per class required
  tmpDF <- data.frame(Values = Values, Cls = Cls)
  n_per_class <- aggregate(Values ~ Cls, data = tmpDF, FUN = length)$Values
  
  if (!all(n_per_class == n_per_class[1])) {
    stop("Provide values for each class and each instance on the x-axis.")
  }
  
  # If Names are given, use them directly for the x-axis.
  # That avoids the ordering bug completely.
  if (is.null(Names)) {
    Names <- rep(seq_len(n_per_class[1]), each = NumCls)
  } else {
    if (length(Names) != length(Values)) {
      stop("Length of vectors Values and Names must equal.")
    }
  }
  
  dfCBP <- data.frame(
    Values = as.numeric(Values),
    Names  = factor(Names, levels = unique(Names)),
    Class  = factor(Cls, levels = unique(Cls))
  )
  
  if (!is.null(Deviation)) {
    dfCBP$Deviation <- as.numeric(Deviation)
  }
  
  if (is.null(ClassColors)) {
    ClassColors <- DataVisualizations::DefaultColorSequence[seq_len(nlevels(dfCBP$Class))]
  }
  
  if (length(ClassColors) != nlevels(dfCBP$Class)) {
    stop("ClassColors must have one color per class.")
  }
  
  if (is.null(names(ClassColors))) {
    names(ClassColors) <- levels(dfCBP$Class)
  }
  
  p <- ggplot(dfCBP, aes(x = Names, y = Values, fill = Class)) +
    geom_col(position = position_dodge(width = 0.9), alpha = 0.5)
  
  if (!is.null(Deviation)) {
    p <- p + geom_errorbar(
      aes(ymin = Values - Deviation, ymax = Values + Deviation),
      position = position_dodge(width = 0.9),
      color = "black",
      width = 0.2
    )
  }
  
  p <- p +
    scale_fill_manual(values = ClassColors) +
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
      axis.text.y = element_text(face = "bold", color = "black", size = 14, angle = 0)
    ) +
    ylab(ylab) + xlab(xlab)
  
  if (isTRUE(PlotIt)) {
    print(p)
  }
  
  return(list(ggplot2Object = p))
}
