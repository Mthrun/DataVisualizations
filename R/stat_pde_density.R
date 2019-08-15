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
  if (length(unique(x)) == 1) {
    warning('stat_pde_density: Only one unique value in Data.')
    x <- c(unique(x), head(x, 1) * runif(1, 0.999, 1.001))
    Flag <- TRUE
  }
  
  dens <- ParetoDensityEstimationV2(Data = x)
  
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