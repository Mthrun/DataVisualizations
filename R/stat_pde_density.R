stat_pde_density <- function(mapping = NULL,
                             data = NULL,
                             geom = "violin",
                             position = "dodge",
                             ...,
                             trim = TRUE, #enden des violins werden korrekt angezeigt und nicht ueber den wertebereich fortgesetzt
                             scale = "area",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Construct a ggplot2 layer using StatPDEdensity; returns the layer object.
  # compute_pdedensity() calls ParetoDensityEstimation(..., Silent = TRUE).
 
  # VIOLIN AND OPTIONAL SPIKE VISIBILITY MARKS
  # The default geom = "violin" draws the standard violin without extra ticks.
  # With geom = GeomPDEviolin (as selected by MDplot), the standard violin is
  # still drawn first. Fill-colored ticks are then overlaid only on eligible,
  # visually compressed density components. This is not a switch between two
  # density estimators or a replacement of the violin by spikes.
  #
  # DECISION A: SPARSE-GRID ELIGIBILITY (compute_pdedensity)
  # The plotting-only pde_sparse flag is TRUE when !Flag and either:
  #   - dens$paretoRadius == 0, or
  #   - any consecutive kernel gap exceeds 4 * dens$paretoRadius.
  # Flag is the existing single-unique-value workaround; it excludes that
  # result from the additional ticks. 
  #
  # DECISION B: DISPLAYED COMPONENT SIZE (GeomPDEviolin$draw_group)
  # The ordinary violin is returned without ticks if pde_sparse is not TRUE,
  # spike_linewidth <= 0, or the coordinate system is nonlinear.
  # Otherwise, consecutive finite positive-density rows form components.
  # For each component, its extent includes the bordering zero-density rows,
  # where present, and is measured along the transformed panel's value axis.
  # A tick is added only when this extent is finite and <= spike_fraction.
  # The default spike_fraction = 0.001 means 0.1 percent of the displayed
  # value-axis span, not 0.1 percent of the raw data range. Panel coordinates
  # account for Cartesian zooms, facets and coord_flip(). A component can
  # therefore stop receiving a tick when a zoom makes it sufficiently wide.
  # Broad and compressed components can coexist in the same violin.
  #
  # TICK POSITION, COLOR AND CONTROLS
  # One tick is placed at a maximum of each qualifying component and spans
  # the existing left and right violin edges at that position. Ticks do not
  # bridge the gaps between separate components. They are visibility marks,
  # not widened density estimates or probability-area representations.
  # Each tick uses its violin's resolved fill: ticks$colour <- ticks$fill.
  # LineColor controls only the ordinary outline, not the tick color; there
  # is no fallback to black when the fill is missing. Existing alpha is kept.
  # Tick linewidth is max(the existing linewidth, spike_linewidth).
  # MDplot's SpikeLineSize is passed as spike_linewidth (default 0.4); it
  # controls thickness, not component selection. Set it to 0 to disable ticks.
  # spike_fraction controls selection (default 0.001); it is a GeomPDEviolin
  # parameter passed through ..., not an MDplot argument. Tick parameters
  # apply only when using GeomPDEviolin, not the default geom = "violin".
  # Ineligible groups keep
  # the ordinary violin; eligible groups keep it too, with ticks added only
  # to their compressed components.
  #
  # Example within this package (GeomPDEviolin is an internal geometry):
  # stat_pde_density(geom = GeomPDEviolin, fill = "steelblue", colour = "black",
  #                  linewidth = 0.01, spike_linewidth = 0.4,
  #                  spike_fraction = 0.001)
  scale <- match.arg(scale, c("area", "count", "width"))
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPDEdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


compute_pdedensity <- function(x) {
  nx <- length(x)
  
  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    warning("stat_pde_density: Groups with fewer than two data points have been dropped.",
            call. = FALSE)
    return(
      data.frame(
        x = NA_real_,
        density = NA_real_,
        scaled = NA_real_,
        count = NA_real_,
        n = NA_integer_
      )
    )
  }
  
  ##MT: chatch error of one unique value
  Flag <- FALSE
  if (length(unique(x)) ==1) {
    warning('stat_pde_density: Only one unique value in Data.')
    if(unique(x)!=0)
      x <- c(unique(x), head(x, 1) * runif(1, 0.999, 1.001))
    else
      x <- c(unique(x), head(x, 1) + runif(1, 0.999, 1.001))

    Flag <- TRUE
  }
  
  dens <- ParetoDensityEstimation(Data = x,Compute = "Cpp_exp",Silent = TRUE)
  
  # Density cannot be estiamted, set density to value equal 1
  if (Flag) {
    # scatter kernels a little to visualize several features if given
    dens$kernels <- dens$kernels * runif(length(dens$kernels), 0.998, 1.002)
    x <- max(dens$kernels) - min(dens$kernels)
    dens$paretoDensity[1:length(dens$paretoDensity)] <- 1 / x # integral over pdf should be 1
  }
  data.frame(
    x = dens$kernels,
    density = dens$paretoDensity,
    scaled =  dens$paretoDensity / max(dens$paretoDensity, na.rm = TRUE),
    count =   dens$paretoDensity * nx,
    # Decision A: grid-spacing eligibility only; see stat_pde_density above.
    # GeomPDEviolin still checks each component's displayed size before drawing.
    pde_sparse = !Flag && (isTRUE(dens$paretoRadius == 0) ||
      isTRUE(any(diff(dens$kernels) > 4 * dens$paretoRadius))),
    n = nx
  )
  
}

StatPDEdensity <- ggproto("StatPDEdensity",
  Stat,
  required_aes = c("x", "y"),
  
  compute_group = function(data,
                           scales,
                           width = NULL,
                           trim = TRUE,
                           na.rm = FALSE) {
    if (nrow(data) < 3)
      return(data.frame())
    range <- range(data$y, na.rm = TRUE)
    modifier <- if (trim) 0 else 3
    dens <- compute_pdedensity(data$y)
    
    dens$y <- dens$x
    dens$x <- mean(range(data$x))
    
    # Compute width if x has multiple values
    if (length(unique(data$x)) > 1) {
      width <- diff(range(data$x)) * 0.9
    }
    dens$width <- width
    
    dens
  },
  
  compute_panel = function(self,
                           data,
                           scales,
                           width = NULL,
                           trim = TRUE,
                           na.rm = FALSE,
                           scale = "area") {
    data <- ggproto_parent(Stat, self)$compute_panel(
      data,
      scales,
      width = width,
      trim = trim,
      na.rm = na.rm
    )
    
    # choose how violins are scaled relative to each other
    data$violinwidth <- switch(
      scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = data$density / max(data$density) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )
    data
  }
  
)

# Keep the normal violin and add visible ticks only to compressed sparse peaks.
# Neither the estimated density nor its scaled horizontal width is changed.
GeomPDEviolin <- ggplot2::ggproto("GeomPDEviolin", ggplot2::GeomViolin,
  parameters = function(self, extra = FALSE) {
    union(ggplot2::GeomViolin$parameters(extra),
          c("spike_linewidth", "spike_fraction"))
  },
  draw_group = function(data, panel_params, coord, ...,
                        spike_linewidth = 0.4, spike_fraction = 0.001) {
    violin <- ggplot2::GeomViolin$draw_group(data, panel_params, coord, ...)
    # Decision B: preserve the standard violin when extra marks are ineligible.
    if (!isTRUE(data$pde_sparse[1L]) || spike_linewidth <= 0 ||
        !coord$is_linear()) return(violin)

    data <- data[order(data$y), , drop = FALSE]
    positive <- is.finite(data$density) & data$density > 0
    starts <- which(positive & !c(FALSE, head(positive, -1L)))
    ends <- which(positive & !c(tail(positive, -1L), FALSE))
    if (!length(starts)) return(violin)

    # Include the bordering zero-density points when measuring each component.
    # Transformed coordinates make the threshold relative to the displayed panel,
    # including zooms, facets and coord_flip(), rather than the raw data range.
    lower <- pmax(starts - 1L, 1L)
    upper <- pmin(ends + 1L, nrow(data))
    transformed <- coord$transform(data, panel_params)
    value_axis <- if (inherits(coord, "CoordFlip")) "x" else "y"
    height <- abs(transformed[[value_axis]][upper] -
                  transformed[[value_axis]][lower])
    # Select only components <= 0.1 percent of the displayed value axis by default.
    thin <- which(is.finite(height) & height <= spike_fraction)
    if (!length(thin)) return(violin)

    # One tick at the maximum of each compressed positive-density component.
    peaks <- vapply(thin, function(i) {
      rows <- seq.int(starts[i], ends[i])
      rows[which.max(data$density[rows])]
    }, integer(1))
    ticks <- data[peaks, , drop = FALSE]
    ticks$xend <- ticks$x + ticks$violinwidth * (ticks$xmax - ticks$x)
    ticks$x <- ticks$x - ticks$violinwidth * (ticks$x - ticks$xmin)
    ticks$yend <- ticks$y
    ticks$linewidth <- pmax(ticks$linewidth, spike_linewidth)
    # Match each violin's fill; LineColor still controls its ordinary outline.
    ticks$colour <- ticks$fill
    segments <- ggplot2::GeomSegment$draw_panel(ticks, panel_params, coord,
                                                lineend = "butt")
    segments$name <- "pde_spike_ticks"
    grid::grobTree(violin, segments, name = "geom_pde_violin")
  }
)
