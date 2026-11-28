DiagnosticAbility4Classifiers = function(TrueCondition_Cls, ManyPredictedCondition_Cls,
                                         NamesOfConditions = NULL, PlotType = "PRC",
                                         xlab = NULL, ylab = NULL, main = NULL,
                                         Colors = NULL, LineColor = NULL, Size = 8,
                                         LineWidth = 1, LineType = NULL, Showgrid = TRUE,
                                         SaveIt = FALSE, PositiveClass = NULL) {
  # Compare the diagnostic ability of binary classifiers or classifier conditions.
  # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0118432
  #
  # INPUT
  # TrueCondition_Cls              Vector of observed reference labels, preferably
  #                                from a test set; exactly two classes are required.
  # ManyPredictedCondition_Cls     Matrix or data frame with one prediction per row
  #                                and one classifier/condition per column.
  #
  # OPTIONAL
  # NamesOfConditions             One name per column; defaults to column names,
  #                                or consecutive numbers if column names are absent.
  # PlotType                      "PRC": x=precision, y=recall; "ROC": x=false
  #                                positive rate, y=recall; "SenSpec": x=sensitivity,
  #                                y=specificity. Each column gives one point.
  # xlab, ylab, main               NULL selects labels appropriate to PlotType.
  # Colors                        One R color per column; NULL uses
  #                                DefaultColorSequence, repeated when necessary.
  # LineColor                     NULL draws points only; otherwise a single R
  #                                color connects defined points in column order.
  # Size, LineWidth               Positive marker size and non-negative line width.
  # LineType                      Plotly dash style; NULL means "solid".
  # Showgrid                      Logical flag controlling both axis grids.
  # SaveIt                        TRUE saves <PlotType>.html in the working directory.
  # PositiveClass                 Positive label. NULL retains the first observed
  #                                factor level of the reference classification;
  #                                for non-factors, the first level of factor().
  #                                Set explicitly, e.g. PositiveClass = 1 for 0/1 data.
  #
  # OUTPUT
  # Plot                          Plotly widget, with a diagonal reference for ROC.
  # X, Y                          Coordinates in prediction-column order. Precision
  #                                is NA if no positives are predicted; these PRC
  #                                points are omitted with a warning, but retained
  #                                in the returned coordinates.
  #
  # DETAILS
  # Missing/non-finite labels, unknown predicted classes and multiclass reference
  # data are rejected. Constant predictions within the two reference classes are
  # supported. Inputs are class labels, not scores; no thresholds or AUC are computed.
  # The existing PRC axis order (precision on x, recall on y) is retained.
  # See Saito and Rehmsmeier (2015), DOI:10.1371/journal.pone.0118432, and
  # Hoffmann et al. (2020), DOI:10.1002/cyto.b.21936.
  # Author: Michael Thrun

  if (!is.null(dim(TrueCondition_Cls)) || is.complex(TrueCondition_Cls) ||
      !(is.numeric(TrueCondition_Cls) || is.character(TrueCondition_Cls) ||
        is.logical(TrueCondition_Cls) || is.factor(TrueCondition_Cls)) ||
      anyNA(TrueCondition_Cls) ||
      (is.numeric(TrueCondition_Cls) && any(!is.finite(TrueCondition_Cls))))
    stop("TrueCondition_Cls must be a vector of non-missing, finite class labels.")
  Classes = levels(factor(TrueCondition_Cls))
  if (length(Classes) != 2L)
    stop("TrueCondition_Cls must contain exactly two observed classes.")
  if (!(is.matrix(ManyPredictedCondition_Cls) || is.data.frame(ManyPredictedCondition_Cls)) ||
      ncol(ManyPredictedCondition_Cls) < 1L)
    stop("ManyPredictedCondition_Cls must be a matrix or data frame with at least one column.")
  if (length(TrueCondition_Cls) != nrow(ManyPredictedCondition_Cls))
    stop("Unequal length of classifications: one prediction per reference observation is required.")
  if (is.null(PositiveClass)) PositiveClass = Classes[1L]
  if (length(PositiveClass) != 1L || anyNA(PositiveClass) ||
      !as.character(PositiveClass) %in% Classes)
    stop("PositiveClass must be one of the two observed reference labels.")
  PositiveClass = as.character(PositiveClass)
  Classes = c(PositiveClass, Classes[Classes != PositiveClass])
  Truth = factor(as.character(TrueCondition_Cls), levels = Classes)
  PlotType = match.arg(PlotType, c("PRC", "ROC", "SenSpec"))
  NrConditions = ncol(ManyPredictedCondition_Cls)
  if (is.null(NamesOfConditions)) NamesOfConditions = colnames(ManyPredictedCondition_Cls)
  if (is.null(NamesOfConditions)) NamesOfConditions = as.character(seq_len(NrConditions))
  if (length(NamesOfConditions) != NrConditions || anyNA(NamesOfConditions))
    stop("NamesOfConditions must contain one non-missing name per prediction column.")
  NamesOfConditions = as.character(NamesOfConditions)
  if (!is.logical(Showgrid) || length(Showgrid) != 1L || is.na(Showgrid) ||
      !is.logical(SaveIt) || length(SaveIt) != 1L || is.na(SaveIt))
    stop("Showgrid and SaveIt must each be one non-missing logical value.")
  if (!is.numeric(Size) || is.complex(Size) || length(Size) != 1L || !is.finite(Size) || Size <= 0 ||
      !is.numeric(LineWidth) || is.complex(LineWidth) || length(LineWidth) != 1L || !is.finite(LineWidth) || LineWidth < 0)
    stop("Size must be positive and LineWidth non-negative; both must be finite numbers.")
  if (is.null(Colors))
    Colors = rep(DataVisualizations::DefaultColorSequence, length.out = NrConditions)
  if (!is.character(Colors) || length(Colors) < NrConditions || anyNA(Colors))
    stop("Colors must supply at least one non-missing R color per prediction column.")
  Colors = unname(Colors[seq_len(NrConditions)])
  grDevices::col2rgb(Colors)
  if (!is.null(LineColor)) {
    if (!is.character(LineColor) || length(LineColor) != 1L || is.na(LineColor))
      stop("LineColor must be NULL or one R color.")
    grDevices::col2rgb(LineColor)
  }
  if (is.null(LineType)) LineType = "solid"
  LineType = match.arg(LineType, c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))

  X = Y = numeric(NrConditions)
  for (col in seq_len(NrConditions)) {
    PredictedCondition = ManyPredictedCondition_Cls[, col]
    if (!is.null(dim(PredictedCondition)) || is.complex(PredictedCondition) ||
        !(is.numeric(PredictedCondition) || is.character(PredictedCondition) ||
          is.logical(PredictedCondition) || is.factor(PredictedCondition)) ||
        anyNA(PredictedCondition) ||
        (is.numeric(PredictedCondition) && any(!is.finite(PredictedCondition))) ||
        any(!as.character(PredictedCondition) %in% Classes))
      stop("Prediction column ", col, " must contain only the two non-missing reference labels.")
    # Rows: predicted labels; columns: reference labels. Keep both classes even
    # when a classifier predicts only one of them, and use the same level order.
    cM = table(factor(as.character(PredictedCondition), levels = Classes), Truth)
    rowsums = rowSums(cM)
    colsums = colSums(cM)
    prec = if (rowsums[1L] == 0L) NA_real_ else cM[1L, 1L] / rowsums[1L]
    rec = cM[1L, 1L] / colsums[1L]
    FPR = cM[1L, 2L] / colsums[2L]
    specificity = cM[2L, 2L] / colsums[2L]
    switch(PlotType,
           PRC = { X[col] = prec; Y[col] = rec },
           ROC = { X[col] = FPR; Y[col] = rec },
           SenSpec = { X[col] = rec; Y[col] = specificity })
  }
  Labels = switch(PlotType,
                  PRC = c("Precision", "Recall", "Precision Recall Plot"),
                  ROC = c("False Positive Rate", "True Positive Rate", "ROC"),
                  SenSpec = c("Sensitivity", "Specificity", "Sensitivity-Specificity Plot"))
  if (is.null(xlab)) xlab = Labels[1L]
  if (is.null(ylab)) ylab = Labels[2L]
  if (is.null(main)) main = Labels[3L]
  for (Label in list(xlab, ylab, main))
    if (!is.character(Label) || length(Label) != 1L || is.na(Label))
      stop("xlab, ylab and main must each be NULL or one non-missing character string.")
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required for DiagnosticAbility4Classifiers().")
  if (SaveIt && !requireNamespace("htmlwidgets", quietly = TRUE))
    stop("Package 'htmlwidgets' is required when SaveIt = TRUE.")

  Plotted = which(is.finite(X) & is.finite(Y))
  if (length(Plotted) < NrConditions)
    warning("Precision is undefined when no positives are predicted; PRC points omitted for: ",
            paste(NamesOfConditions[!is.finite(X)], collapse = ", "), call. = FALSE)
  # Build the Plotly widget directly: Classplot also supports other backends
  # and warns about one or two unique values, which are valid operating points here.
  if (length(Plotted) == 0L) {
    fig = plotly::plot_ly(x = numeric(0), y = numeric(0), type = "scatter",
                          mode = "markers", showlegend = FALSE)
  } else {
    fig = plotly::plot_ly()
    if (!is.null(LineColor))
      fig = plotly::add_trace(fig, x = X[Plotted], y = Y[Plotted],
                             type = "scatter", mode = "lines",
                             line = list(color = plotly::toRGB(LineColor),
                                         width = LineWidth, dash = LineType),
                             name = "Line", showlegend = FALSE)
    for (col in Plotted)
      fig = plotly::add_markers(fig, x = X[col], y = Y[col],
                               name = NamesOfConditions[col], text = NamesOfConditions[col],
                               hoverinfo = "text+x+y", showlegend = TRUE,
                               marker = list(size = Size, color = plotly::toRGB(Colors[col]),
                                             line = list(color = "black", width = 1)))
  }
  fig = plotly::layout(fig, title = main,
                       xaxis = list(title = xlab, range = c(0, 1), showgrid = Showgrid),
                       yaxis = list(title = ylab, range = c(0, 1), showgrid = Showgrid))
  if (PlotType == "ROC") {
    line = list(type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
                xref = "x", yref = "y", line = list(color = "grey", dash = "dot"))
    fig = plotly::layout(fig, shapes = list(line))
  }
  if (SaveIt)
    htmlwidgets::saveWidget(fig, file = paste0(PlotType, ".html"))
  return(list(Plot = fig, X = X, Y = Y))
}
