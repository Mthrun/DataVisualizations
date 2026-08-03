ParetoDensityEstimation = function(Data,paretoRadius,kernels = NULL,MinAnzKernels = 100,PlotIt = FALSE,Compute = "Cpp",Silent = FALSE,EvaluationMode = c("strict_2026", "compatible_2020")) {
  #  V = ParetoDensityEstimation(Data,paretoRadius,kernels)
  #  V = ParetoDensityEstimation(Data)
  #  ParetoDensity = V$paretoDensity
  #  Kernels = V$kernels
  #  Estimates the Pareto density for a one-dimensional distribution.
  #  After boundary mirroring, observations in
  #  [kernel-paretoRadius,kernel+paretoRadius] are counted.
  #
  #  INPUT
  #  Data				One-dimensional data. Non-finite values are removed.
  #
  #  OPTIONAL
  #  paretoRadius			Positive finite Pareto radius. If missing, NULL,
  #				numeric(0), NA, NaN, or zero, the radius is estimated
  #				from Data.
  #  kernels			Numeric positions requested by the user. NULL,
  #				numeric(0), or scalar zero requests an automatically
  #				generated grid. Supplied kernels must be finite and
  #				strictly increasing.
  #  MinAnzKernels		Minimum number of kernels for an automatically generated
  #				grid. Zero restores the default value 100. Automatic
  #				grids are limited to 10000 points.
  #  PlotIt			One logical value. If TRUE, the returned density is plotted.
  #  Compute			Counting backend: "r", "cpp", or "cpp_exp". This affects
  #				only interval counting, not radius estimation, grid
  #				construction, or normalization.
  #  Silent			One logical value. If TRUE, warnings generated directly by
  #				this function are suppressed. Errors are never suppressed.
  #  EvaluationMode		"strict_2026" (default): If the user supplies kernels,
  #				the mirrored-data counts are evaluated directly at these
  #				points. min(Data) and/or max(Data) are inserted when
  #				required for complete range coverage. The resulting grid
  #				may therefore be non-equidistant. No interpolation is
  #				performed.
  #				"compatible_2020": If the user supplies kernels, the
  #				mirrored-data counts are first evaluated and normalized on
  #				the established automatically generated internal grid. The
  #				normalized internal PDE is then linearly interpolated to the
  #				user-supplied kernels.
  #				In every other respect the modes are identical. With no
  #				user-supplied kernels, EvaluationMode has no effect.
  #
  #  SPECIAL CASE: ONE OR TWO DISTINCT VALUES
  #  A normalized piecewise-linear numerical representation of the empirical
  #  Dirac measure(s) is returned by the separate helper function
  #  makeDiracRepresentation(). Each triangular impulse has an area equal to
  #  the empirical relative frequency of its support value; total trapezoidal
  #  area is one. paretoRadius, kernels, MinAnzKernels, Compute, and
  #  EvaluationMode are ignored in this case.
  #
  #  OUTPUT
  #  List with
  #  kernels			Data values at which paretoDensity is returned. In
  #				strict_2026 with user-supplied kernels, min(Data) and/or
  #				max(Data) may be added.
  #  paretoDensity		Pareto-density values or the numerical Dirac
  #				representation.
  #  paretoRadius		Supplied or estimated Pareto radius; zero in the Dirac
  #				special case.
  #  Fit			Minimal counting context for
  #				ParetoDensityEstimationFromFit(); NULL in the Dirac special
  #				case. It contains only Data, unnormalized paretoCounts, and
  #				countingKernels when the counting grid differs from the returned
  #				kernels, and interpolationUsed to preserve the exact evaluation
  #				path. 
  #
  #  REFERENCES
  # Ultsch, A. (2005). Pareto density estimation: A density estimation for
  # knowledge discovery. In D. Baier and K. D. Wernecke (eds.),
  # \emph{Innovations in Classification, Data Science, and Information Systems},
  # pp. 91--100. Springer, Berlin.
  #  Stier, Q., Hoffmann, J. & Thrun, M. C. (2026): Classifying with the Fine
  #  Structure of Distributions: Leveraging Distributional Information for
  #  Robust and Plausible Naive Bayes. Machine Learning and Knowledge
  #  Extraction, 8(1), 13. DOI: 10.3390/make8010013.
  #
  #  Thrun, M. C., Gehlert, T. & Ultsch, A. (2020): Analyzing the Fine
  #  Structure of Distributions. PLoS ONE, 15(10), e0238835.
  #  DOI: 10.1371/journal.pone.0238835.
  #
  #  Original author: MT, 2019
  #  Revised implementation: 2026

  radiusArgumentMissing = missing(paretoRadius)

  # Named helpers are retained only when they are called more than once below.
  # makeDiracRepresentation() is the intentional Dirac-specific exception.
  validateFlag = function(x, name) {
  #  validateFlag(x,name)
  #  Validates a logical control argument.
  #
  #  INPUT
  #  x				Value to be validated.
  #  name				Name of the argument used in the error message.
  #
  #  OUTPUT
  #  invisible(TRUE)		Returned if x is one non-missing logical value.
  #
  #  Revision: 2026

    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop(name, " must be one non-missing logical value.", call. = FALSE)
    }
    invisible(TRUE)
  }
  validateFlag(Silent, "Silent")
  validateFlag(PlotIt, "PlotIt")

  if(isTRUE(PlotIt))   
    xlab = paste(deparse(substitute(Data)), collapse = " ")
  
  warnIfNeeded = function(message, silent) {
  #  warnIfNeeded(message,silent)
  #  Issues a warning unless silent is TRUE.
  #
  #  INPUT
  #  message			Warning message passed to warning().
  #  silent			One logical value controlling warning suppression.
  #
  #  OUTPUT
  #  invisible(NULL)		No warning is issued if silent is TRUE.
  #
  #  Revision: 2026

    if (!silent) {
      warning(message, call. = FALSE)
    }
    invisible(NULL)
  }

  normalizeByTrapezoids = function(x, y) {
  #  normalizeByTrapezoids(x,y)
  #  Normalizes non-negative ordinates by trapezoidal integration.
  #
  #  INPUT
  #  x				Strictly increasing finite evaluation positions.
  #  y				Finite non-negative ordinates at x.
  #
  #  OUTPUT
  #  normalizedY			Numeric vector with trapezoidal integral equal to one.
  #
  #  Revision: 2026

    if (length(x) != length(y) || length(x) < 2L ||  any(!is.finite(x)) || any(!is.finite(y)) || any(y < 0)) {
      stop("Invalid Pareto-density grid or ordinates.", call. = FALSE)
    }

    dx = diff(x)
    if (any(!is.finite(dx)) || any(dx <= 0)) {
      stop(
        "The Pareto-density grid must be finite and strictly increasing.",
        call. = FALSE
      )
    }

    area = sum(dx * (head(y, -1L) + tail(y, -1L)) / 2)
    if (!is.finite(area) || area <= 0) {
      stop(
        "The Pareto density could not be normalized on the selected grid.",
        call. = FALSE
      )
    }

    return(y / area)
  }

  makeDiracRepresentation = function(x, support) {
  #  D = makeDiracRepresentation(x,support)
  #  Constructs a normalized numerical representation of one or two
  #  empirical Dirac measures by non-overlapping triangular impulses.
  #
  #  INPUT
  #  x				Finite numeric observations.
  #  support			Sorted distinct support values of x; length one or two.
  #
  #  OUTPUT
  #  List with
  #  kernels			Strictly increasing positions of the triangular impulses.
  #  density			Non-negative ordinates whose trapezoidal integral is one;
  #				each impulse area equals its empirical relative frequency.
  #
  #  Revision: 2026

    support = sort(support)
    probabilities = tabulate( match(x, support),   nbins = length(support) ) / length(x)

    if (length(support) == 1L) {
      halfWidth = 1e-4 * max(1, abs(support))
    } else {
      gap = diff(support)
      supportScale = max(1, max(abs(support)))
      minimumHalfWidth = 128 * .Machine$double.eps * supportScale
      halfWidth = min( gap / 4, max(gap * 1e-3, minimumHalfWidth))
    }

    if (!is.finite(halfWidth) || halfWidth <= 0) {
      stop( "A finite numerical Dirac representation could not be constructed.", call. = FALSE )
    }

    diracKernels = unlist(
      lapply(
        seq_along(support),
        function(i) {
          c( support[i] - halfWidth, support[i], support[i] + halfWidth)
        }
      ), use.names = FALSE
    )
    diracDensity = unlist(
      lapply(seq_along(support),
        function(i) {
          c(0, probabilities[i] / halfWidth, 0)
        }
      ),use.names = FALSE
    )

    orderIndex = order(diracKernels)
    diracKernels = diracKernels[orderIndex]
    diracDensity = diracDensity[orderIndex]

    if (any(!is.finite(diracKernels)) || any(diff(diracKernels) <= 0)) {
      stop(paste0(
          "The support values are too close to each other, or too close to ",
          "machine limits, for a stable numerical Dirac representation."
        ), call. = FALSE)
    }

    return(list(
      kernels = diracKernels,
      density = normalizeByTrapezoids(diracKernels, diracDensity)
    ))
  }

  addCoveragePoints = function(grid, minX, maxX, radius) {
  #  grid = addCoveragePoints(grid,minX,maxX,radius)
  #  Adds data-boundary positions when the first or last Pareto interval does
  #  not cover the complete data range.
  #
  #  INPUT
  #  grid				Finite strictly increasing evaluation positions.
  #  minX				Minimum of the finite observations.
  #  maxX				Maximum of the finite observations.
  #  radius				Positive finite Pareto radius.
  #
  #  OUTPUT
  #  grid				Sorted unique grid, optionally extended by minX and/or maxX.
  #
  #  Revision: 2026

    if ((grid[1L] - radius) > minX) {
      grid = c(minX, grid)
    }
    if ((tail(grid, 1L) + radius) < maxX) {
      grid = c(grid, maxX)
    }
    return(sort(unique(as.numeric(grid))))
  }

  # Convert Data once and remove non-finite values once.
  if (!is.null(dim(Data)) || is.list(Data)) {
    warnIfNeeded("Data is not a plain univariate vector and is flattened.",Silent)
    Data = unlist(Data, recursive = TRUE, use.names = FALSE)
  } else {
    Data = as.vector(Data)
  }

  if (is.complex(Data)) {
    stop("Data must contain real values.", call. = FALSE)
  }
  if (!is.numeric(Data)) {
    Data = tryCatch(
      suppressWarnings(as.numeric(Data)),
      error = function(e) NULL
    )
    if (is.null(Data)) {
      stop("Data cannot be converted to numeric.", call. = FALSE)
    }
    warnIfNeeded( "Non-numeric Data was converted to numeric.", Silent )
  } else {
    Data = as.numeric(Data)
  }

  finiteValues = is.finite(Data)
  if (!all(finiteValues)) {
    warnIfNeeded("Non-finite observations were removed from Data.", Silent )
    Data = Data[finiteValues]
  }
  if (length(Data) == 0L) {
    stop("Data contains no finite numeric observations.", call. = FALSE)
  }

  values = sort(unique(Data))
  if (length(values) > 2L && length(values) < 5L) {
    warnIfNeeded("Fewer than five distinct values were supplied; results may be unstable.",Silent)
  }

  if (length(values) < 3L) {
    warnIfNeeded(
      paste0(
        "Only one or two distinct values were supplied. A normalized ",
        "numerical representation of the empirical Dirac measure(s) is ",
        "returned; other arguments are ignored."
      ), Silent )

    dirac = makeDiracRepresentation(Data, values)

    if (PlotIt) {
      maximumDensity = max(dirac$density)
      plot(dirac$kernels,dirac$density,
        type = "l", main = "Numerical Dirac representation",
        xaxs = "i",yaxs = "i",xlab = xlab,
        ylab = "Density",ylim = c(0, 1.1 * maximumDensity),xlim=c(0.9*min(dirac$kernels),1.1*max(dirac$kernels))
      )
    }

    return(list(
      kernels = dirac$kernels,
      paretoDensity = dirac$density,
      paretoRadius = 0,
      Fit = NULL
    ))
  }

  if (length(Data) < 10L) {
    warnIfNeeded( "Fewer than ten observations were supplied; radius estimation may be unstable.", Silent)
  }

  if (missing(EvaluationMode)) {
    EvaluationMode = "strict_2026"
  }
  if (!is.character(EvaluationMode) || length(EvaluationMode) != 1L || is.na(EvaluationMode)) {
    stop("EvaluationMode must be one character value.", call. = TRUE)
  }
  EvaluationMode = match.arg( tolower(EvaluationMode), c("strict_2026", "compatible_2020") )

  if (!is.character(Compute) || length(Compute) != 1L || is.na(Compute)) {
    stop("Compute must be one character value.", call. = TRUE)
  }
  Compute = match.arg(tolower(Compute),c("r", "cpp", "cpp_exp") )

  if (!is.numeric(MinAnzKernels) || is.complex(MinAnzKernels) || length(MinAnzKernels) != 1L || !is.finite(MinAnzKernels) || MinAnzKernels < 0) {
    stop("MinAnzKernels must be one finite non-negative real value.",call. = TRUE)
  }
  if (MinAnzKernels == 0) {
    MinAnzKernels = 100
  }
  MinAnzKernels = ceiling(MinAnzKernels)
  if (MinAnzKernels > 10000) {
    warnIfNeeded("MinAnzKernels is capped at 10000.",Silent)
    MinAnzKernels = 10000
  }
  MinAnzKernels = as.integer(max(2, MinAnzKernels))

  kernelsAutomatic = is.null(kernels) || length(kernels) == 0L || (
      is.numeric(kernels) &&!is.complex(kernels) &&length(kernels) == 1L && !is.na(kernels) && is.finite(kernels) && kernels == 0
    )
  userKernels = !kernelsAutomatic

  if (userKernels) {
    if (!is.numeric(kernels) || is.complex(kernels) || anyNA(kernels) || any(!is.finite(kernels))) {
      stop( "kernels must contain finite real numeric values.", call. = TRUE)
    }
    kernels = as.numeric(kernels)
    if (length(kernels) > 1L && any(diff(kernels) <= 0)) {
      stop("User-supplied kernels must be strictly increasing.", call. = TRUE)
    }
  }

  estimateRadius = radiusArgumentMissing || is.null(paretoRadius) || length(paretoRadius) == 0L

  if (!estimateRadius) {
    if (length(paretoRadius) != 1L) {
      stop( "paretoRadius must be one scalar or a missing-like value.", call. = TRUE)
    }
    if (is.na(paretoRadius)) {
      estimateRadius = TRUE
    } else if (!is.numeric(paretoRadius) || is.complex(paretoRadius)) {
      stop("paretoRadius must be real numeric.", call. = FALSE)
    } else if (paretoRadius == 0) {
      estimateRadius = TRUE
    } else if (!is.finite(paretoRadius) || paretoRadius < 0) {
      stop( "paretoRadius must be finite and positive, or zero/NA for estimation.", call. = TRUE)
    }
  }

  if (estimateRadius) {
    numberOfObservations = length(Data)

    if (numberOfObservations < 5000L) {
      paretoRadius = ParetoRadius(Data)
    } else if (numberOfObservations <= 100000L) {
      paretoRadius = ParetoRadius_fast(Data)
    } else {
      radiusEstimates = replicate( 100L, ParetoRadius_fast(Data, maximumNrSamples = 10000L))
      radiusEstimates = radiusEstimates[is.finite(radiusEstimates) & radiusEstimates > 0 ]
      if (length(radiusEstimates) == 0L) {
        paretoRadius =   NA_real_
      } else {
        paretoRadius = mean(radiusEstimates)
      }
    }
  }
  if (!is.numeric(paretoRadius) || is.complex(paretoRadius) || length(paretoRadius) != 1L || !is.finite(paretoRadius) || paretoRadius <= 0) {
    stop("A positive finite Pareto radius could not be determined.",call. = TRUE)
  }
  paretoRadius = as.numeric(paretoRadius)

  minData = min(Data)
  maxData = max(Data)
  if (!is.finite(maxData - minData) || maxData <= minData) {
    stop( "The finite data range cannot be represented numerically.", call. = TRUE )
  }

  automaticGridNeeded = !userKernels || EvaluationMode == "compatible_2020"

  automaticGrid = NULL
  if (automaticGridNeeded) {
    maximumNumberOfKernels = 10000L
    requiredBins = ceiling((maxData - minData) / paretoRadius) + 1

    if (!is.finite(requiredBins) || requiredBins > maximumNumberOfKernels) {
      stop( paste0(   "paretoRadius requires more than ", maximumNumberOfKernels,   " automatic grid points. The radius was not changed." ),  call. = TRUE)
    }

    estimatedBins = tryCatch(
      suppressWarnings(OptimalNoBins(Data)),
      error = function(e) NA_real_
    )
    if (!is.numeric(estimatedBins) || is.complex(estimatedBins) ||  length(estimatedBins) != 1L || !is.finite(estimatedBins) || estimatedBins <= 0) {
      warnIfNeeded( "OptimalNoBins() failed; MinAnzKernels is used instead.", Silent)
      estimatedBins = MinAnzKernels
    }

    nBins = max(MinAnzKernels, ceiling(estimatedBins))
    if (nBins > 100) {
      nBins = 3 * nBins + 1
    }
    if (!is.finite(nBins) || nBins > maximumNumberOfKernels) {
      warnIfNeeded("The initial grid-size estimate exceeds 10000 and is capped.", Silent)
      nBins = maximumNumberOfKernels
    }
    nBins = as.integer(max(nBins, requiredBins, 2))

    repeat {
      breaks = pretty(c(minData, maxData), n = nBins, min.n = 1)
      if (length(breaks) < 3L || any(!is.finite(breaks))) {
        stop("A valid automatic kernel grid could not be generated.",call. = TRUE)
      }

      numberOfBreaks = length(breaks)
      midpointGrid = 0.5 * ( breaks[-1L] + breaks[-numberOfBreaks] )
      midpointGrid = sort(unique(as.numeric(midpointGrid)))

      if (length(midpointGrid) < 2L || any(diff(midpointGrid) <= 0)) {
        stop("The automatic kernel grid is degenerate.", call. = FALSE)
      }

      spacing = max(diff(midpointGrid))
      tolerance = max(  100 * .Machine$double.eps *max(abs(spacing), abs(paretoRadius)), .Machine$double.xmin)

      if (is.finite(spacing) && spacing <= paretoRadius + tolerance) {
        automaticGrid = addCoveragePoints(midpointGrid, minData, maxData,  paretoRadius)
        if (length(automaticGrid) > maximumNumberOfKernels) {
          stop(paste0("The automatic grid exceeds ", maximumNumberOfKernels, " points after boundary insertion."),call. = TRUE )
        }
        break
      }

      if (nBins >= maximumNumberOfKernels) {
        stop(paste0("The automatic grid cannot resolve paretoRadius without ", "exceeding ", maximumNumberOfKernels, " points. The radius was not changed."),call. = TRUE)
      }

      nextBins = min( maximumNumberOfKernels, max(nBins + 10L, ceiling(1.25 * nBins), requiredBins))
      if (!is.finite(nextBins) || nextBins <= nBins) {
        stop( "Automatic kernel-grid refinement did not progress.", call. = TRUE)
      }
      nBins = as.integer(nextBins)
    }
  }

  if (userKernels && EvaluationMode == "strict_2026") {
    evaluationKernels = addCoveragePoints( kernels, minData, maxData, paretoRadius )
  } else {
    evaluationKernels = automaticGrid
  }

  if (length(evaluationKernels) < 2L) {
    stop(paste0( "At least two evaluation points are required for trapezoidal ", "normalization."),call. = TRUE)
  }

  # Boundary approximation by mirroring. Mirrored observations are counted
  # normally; half weighting applies only to the first and last ordinates in
  # trapezoidal integration.
  lowerReflections = 2 * minData - Data[ Data < minData + paretoRadius]
  upperReflections = 2 * maxData - Data[ Data > maxData - paretoRadius]
  DataPlus = c(Data, lowerReflections, upperReflections)

  numberOfKernels = length(evaluationKernels)
  paretoCounts = switch(
    Compute,
    r = vapply(
      evaluationKernels,
      function(center) {
        sum( DataPlus >= center - paretoRadius &DataPlus <= center + paretoRadius)
      },numeric(1L)),
    cpp = c_pde(evaluationKernels,numberOfKernels,paretoRadius,DataPlus ),
    cpp_exp = c_pde_fast(evaluationKernels,numberOfKernels,paretoRadius,DataPlus)
  )
  paretoCounts = as.numeric(paretoCounts)
  if (length(paretoCounts) != numberOfKernels ||any(!is.finite(paretoCounts)) || any(paretoCounts < 0)) {
    stop( "The Pareto-density counting backend returned invalid values.", call. = TRUE)
  }
  densityOnEvaluationGrid = normalizeByTrapezoids(evaluationKernels, paretoCounts )

  interpolationUsed = userKernels && EvaluationMode == "compatible_2020"

  if (interpolationUsed) {
    paretoDensity = stats::approx(
      x = evaluationKernels,
      y = densityOnEvaluationGrid,
      xout = kernels,
      rule = 1,
      ties = "ordered"
    )$y
    paretoDensity[!is.finite(paretoDensity)] = 0

    sameCountingAndOutputGrid =
      length(kernels) == length(evaluationKernels) &&
      all(kernels == evaluationKernels)

    if (sameCountingAndOutputGrid) {
      countingKernels = NULL
    } else {
      countingKernels = evaluationKernels
    }
  } else {
    kernels = evaluationKernels
    paretoDensity = densityOnEvaluationGrid
    countingKernels = NULL
  }

  if (PlotIt) {
    maximumDensity = suppressWarnings(max(paretoDensity, na.rm = TRUE))
    if (!is.finite(maximumDensity) || maximumDensity <= 0) {
      maximumDensity = 1
    }
    plot(
      kernels,paretoDensity,
      type = "l", main = "Raw Pareto Density Estimation",
      xaxs = "i", yaxs = "i", xlab = xlab, ylab = "PDE",ylim = c(0, 1.1 * maximumDensity)
    )
  }

  Fit = list(
    Data = Data,
    paretoCounts = paretoCounts,
    countingKernels = countingKernels,
    interpolationUsed = interpolationUsed
  )

  return(list(
    kernels = kernels,
    paretoDensity = paretoDensity,
    paretoRadius = paretoRadius,
    Fit = Fit
  ))
}
