BimodalityAmplitude = function(
    x,
    PlotIt = FALSE,    SampleSize = Inf,
    SampleSeed = NULL,
    na.rm = FALSE,...){
  # BimodalityAmplitude(x, PlotIt)
  #
  # Computes the bimodality amplitude after Zhang et al. (2003). The public
  # function retains its historical name and first two arguments.
  #
  # Historical implementation notes retained here:
  # dens = density(x) # crude but fast estimation
  # If no turning point can be identified, zero is returned. For every
  # antimode, the nearest lower-index and higher-index maxima are compared.
  #
  # Density estimation remains in stats::density(), while the search for
  # adjacent peaks, antimodes, and the maximum amplitude is implemented in
  # C++. Optional reproducible subsampling bounds runtime for large vectors.

#later to be mabye added


  BimodalityAmplitudeCpp(
    x = x,
    PlotIt = PlotIt,
    SampleSize = SampleSize,
    SampleSeed = SampleSeed,
    na.rm = na.rm,
    ...
  )
}

BimodalityAmplitudeCpp = function(
    x,
    PlotIt = FALSE,
    SampleSize = Inf,
    SampleSeed = NULL,
    na.rm = FALSE,
    ...){
  if(!is.numeric(x) || is.complex(x) || !is.null(dim(x))){
    stop("'x' must be one numeric vector.", call. = FALSE)
  }
  x = as.numeric(x)

  if(!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt)){
    stop("'PlotIt' must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)){
    stop("'na.rm' must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.numeric(SampleSize) || is.complex(SampleSize) ||
     length(SampleSize) != 1L || is.na(SampleSize) ||
     (!is.finite(SampleSize) &&
      !identical(as.numeric(SampleSize), Inf)) ||
     (is.finite(SampleSize) &&
      (SampleSize < 3 || SampleSize != floor(SampleSize) ||
       SampleSize > .Machine$integer.max))){
    stop(
      "'SampleSize' must be one integer of at least three or Inf.",
      call. = FALSE
    )
  }
  if(!is.null(SampleSeed)){
    if(!is.numeric(SampleSeed) || is.complex(SampleSeed) ||
       length(SampleSeed) != 1L || is.na(SampleSeed) ||
       !is.finite(SampleSeed) || SampleSeed < 0 ||
       SampleSeed != floor(SampleSeed) ||
       SampleSeed > .Machine$integer.max){
      stop(
        "'SampleSeed' must be NULL or one non-negative integer.",
        call. = FALSE
      )
    }
    SampleSeed = as.integer(SampleSeed)
  }

  if(isTRUE(na.rm)){
    x = x[is.finite(x)]
  }else if(any(!is.finite(x))){
    stop("'x' contains non-finite values.", call. = FALSE)
  }

  if(length(x) < 3L || length(unique(x)) < 2L){
    return(0)
  }

  if(is.finite(SampleSize) && length(x) > SampleSize){
    x = .BimodalityAmplitudeLocalSeed(
      SampleSeed,
      function(){
        sample(
          x,
          size = as.integer(SampleSize),
          replace = FALSE
        )
      }
    )
  }
  if(length(x) < 3L || length(unique(x)) < 2L){
    return(0)
  }

  dens = stats::density(x, ...)
  Result = BimodalityAmplitudeCoreCpp(
    DensityX = dens$x,
    DensityY = dens$y
  )

  B = as.numeric(Result$Amplitude)
  if(length(B) != 1L || !is.finite(B) || B <= 0){
    return(0)
  }
  B = max(0, min(1, B))

  if(isTRUE(PlotIt) &&
     length(Result$MaximumIndices) == 2L &&
     length(Result$AntimodeIndex) == 1L &&
     !is.na(Result$AntimodeIndex)){
    maxima = dens$x[Result$MaximumIndices]
    minima = dens$x[Result$AntimodeIndex]

    mm = min(dens$x)
    if(abs(mm) < 1e-2) mm = -0.02

    xMaximum = max(dens$x) * 1.02
    if(!is.finite(xMaximum) || xMaximum <= mm){
      Width = diff(range(dens$x))
      if(!is.finite(Width) || Width <= 0) Width = 1
      xMaximum = max(dens$x) + 0.02 * Width
    }

    graphics::plot(
      dens$x,
      dens$y,
      type = 'l',
      main = "Kernel density estimate with modes and antimode 'A'",
      ylim = c(0, max(dens$y) * 1.1),
      xlim = c(mm, xMaximum),
      xlab = 'Observed values',
      ylab = 'Estimated density'
    )
	#Left mode
    graphics::abline(v = maxima[1], col = 'blue', lwd = 2)
	#Right mode
    graphics::abline(v = maxima[2], col = 'blue', lwd = 2)
	#Antimode
    graphics::abline(v = minima, col = 'darkgreen', lwd = 2)
    graphics::text(
      max(maxima) + 0.06 * diff(range(dens$x)),
      1.009 * max(range(dens$y)),
      'Max 1',
      col = 'red'
    )
    graphics::text(
      min(maxima) - 0.06 * diff(range(dens$x)),
      1.009 * max(range(dens$y)),
      'Max 2',
      col = 'red'
    )
    graphics::text(
      minima + 0.06 * diff(range(dens$x)),
      0,
      'A',
      col = 'red'
    )
  }

  return(B)
}

.BimodalityAmplitudeLocalSeed = function(Seed, FUN){
  if(is.null(Seed)) return(FUN())

  HadSeed = exists(
    '.Random.seed',
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if(HadSeed){
    OldSeed = get(
      '.Random.seed',
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }
  on.exit({
    if(HadSeed){
      assign(
        '.Random.seed',
        OldSeed,
        envir = .GlobalEnv
      )
    }else if(exists(
      '.Random.seed',
      envir = .GlobalEnv,
      inherits = FALSE
    )){
      rm('.Random.seed', envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(Seed)
  FUN()
}
