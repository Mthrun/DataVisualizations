ECDFplot = function(Curves, Names = NULL, xlab = "Value",
                    ylab = "Cumulative probability",
                    main = "Empirical cumulative distributions",
                    col = c("#555555", "#2166ac", "#16845b", "#c36720"),
                    lty = seq_len(6L), lwd = 2,
                    LegendPosition = "bottomright", xlim = NULL,
                    ReferenceLines = c(0.5, 0.8, 0.9), PlotIt = TRUE, ...) {
# Result = ECDFplot(Curves, Names, xlab, ylab, main, col, lty, lwd,
#                   LegendPosition, xlim, ReferenceLines, PlotIt, ...)
# Compare empirical cumulative distribution functions in a single plot.
# Each curve shows the fraction of its observations less than or equal to x.
#
# INPUT
# Curves               Numeric vector, list of numeric vectors (lengths may
#                      differ), or numeric matrix/data frame (one curve/column).
#
# OPTIONAL
# Names                Legend labels; defaults to list/column names, with
#                      "Curve 1", "Curve 2", ... for unnamed entries.
# xlab, ylab, main      Axis labels and title, with general-purpose defaults.
# col, lty, lwd        Curve colors, line types and widths; recycled as needed.
# LegendPosition       Position accepted by legend(), or NULL for no legend.
# xlim                 Finite increasing x-axis limits; defaults to the pooled
#                      data range with a small margin, also for constant data.
# ReferenceLines       Probabilities for horizontal guide lines; NULL disables.
# PlotIt               TRUE draws the plot; FALSE only computes the curves.
# ...                  Further graphical arguments to plot(), e.g. cex.main.
#                      type and ylim are fixed by this function.
#
# OUTPUT (invisibly)
# Curves               Named list of data frames with X and Y at unique data
#                      values. Y is the right-continuous ECDF, including ties.
# N                    Number of finite observations used for each curve.
# Removed              Number of NA, NaN and infinite values omitted per curve.
# xlim                 The x-axis limits used for the plot.
#
# DETAILS
# Nonfinite observations are omitted with a warning. Each curve is normalized
# by its own number of finite observations, not a shared sample size. Thus a
# failed benchmark run must not be encoded as NA/Inf if it should count against
# an algorithm: handle failures separately. No smoothing or interpolation is
# applied. Existing multi-panel layouts and graphical parameters are preserved.

  if (is.matrix(Curves)) {
    ColumnNames = colnames(Curves)
    Curves = lapply(seq_len(ncol(Curves)), function(i) Curves[, i])
    names(Curves) = ColumnNames
  } else if (is.numeric(Curves) && is.null(dim(Curves))) {
    Curves = list(Curves)
  }
  if (!is.list(Curves) || !length(Curves) ||
      !all(vapply(Curves, function(x) is.numeric(x) && !is.complex(x) &&
                  is.null(dim(x)), logical(1))))
    stop("Curves must contain one or more numeric vectors.")

  NrCurves = length(Curves)
  if (is.null(Names)) {
    Names = names(Curves)
    if (is.null(Names)) Names = rep("", NrCurves)
    Unnamed = is.na(Names) | !nzchar(Names)
    Names[Unnamed] = paste("Curve", which(Unnamed))
  }
  if (length(Names) != NrCurves || anyNA(Names))
    stop("Names must provide one non-missing label per curve.")
  Names = as.character(Names)
  Removed = vapply(Curves, function(x) sum(!is.finite(x)), integer(1))
  Curves = lapply(Curves, function(x) x[is.finite(x)])
  N = lengths(Curves)
  if (any(N == 0L)) stop("Every curve needs at least one finite observation.")
  if (any(Removed > 0L))
    warning("Nonfinite observations omitted; ECDFs use finite observations only: ",
            paste(paste0(Names[Removed > 0L], " (", Removed[Removed > 0L], ")"),
                  collapse = ", "), call. = FALSE)
  names(N) = names(Removed) = Names

  ECDFs = lapply(Curves, function(x) {
    X = sort(unique(x))
    F = stats::ecdf(x)
    data.frame(X = X, Y = F(X))
  })
  names(ECDFs) = Names
  if (is.null(xlim)) {
    xlim = range(unlist(Curves, use.names = FALSE))
    Span = diff(xlim)
    if (Span == 0) Span = max(1, abs(xlim[1L]))
    xlim = xlim + c(-0.04, 0.04) * Span
  }
  if (!is.numeric(xlim) || length(xlim) != 2L ||
      any(!is.finite(xlim)) || xlim[1L] >= xlim[2L])
    stop("xlim must contain two finite, strictly increasing values.")
  if (!is.null(ReferenceLines) && (!is.numeric(ReferenceLines) ||
      any(!is.finite(ReferenceLines)) || any(ReferenceLines < 0 | ReferenceLines > 1)))
    stop("ReferenceLines must contain probabilities between zero and one.")
  if (!length(col) || !length(lty) || !length(lwd))
    stop("col, lty and lwd must not be empty.")

  if (isTRUE(PlotIt)) {
    col = rep(col, length.out = NrCurves)
    lty = rep(lty, length.out = NrCurves)
    lwd = rep(lwd, length.out = NrCurves)
    graphics::plot(xlim, c(0, 1), type = "n", xlim = xlim, ylim = c(0, 1),
                   xlab = xlab, ylab = ylab, main = main, ...)
    if (length(ReferenceLines))
      graphics::abline(h = ReferenceLines, lty = 3, col = "grey85")
    # Extend each step function to both plotting boundaries. Clipped xlim
    # retains the actual ECDF level rather than restarting it at zero.
    Bounds = graphics::par("usr")[1:2]
    if (graphics::par("xlog")) Bounds = 10^Bounds
    for (i in seq_len(NrCurves)) {
      X = sort(unique(c(Bounds, ECDFs[[i]]$X)))
      X = X[X >= Bounds[1L] & X <= Bounds[2L]]
      F = stats::ecdf(Curves[[i]])
      graphics::lines(X, F(X), type = "s", col = col[i], lty = lty[i], lwd = lwd[i])
    }
    if (!is.null(LegendPosition))
      graphics::legend(LegendPosition, legend = Names, col = col, lty = lty,
                       lwd = lwd, bty = "n")
  }
  return(invisible(list(Curves = ECDFs, N = N, Removed = Removed, xlim = xlim)))
}
