Choroplethmap=plotChoroplethMap=function(Counts,PostalCodes,NumberOfBins=0,Breaks4Intervals,percentiles=c(0.5,0.95),digits=0,PostalCodesShapes,PlotIt=TRUE,DiscreteColors,HighColorContinuous='red',LowColorContinuous='deepskyblue1',NAcolor='grey',ReferenceMap=FALSE, main='Political Map of Germany',legend='Range of values',Silent=TRUE,
    Interactive=FALSE,Categorical=is.factor(Counts)||is.character(Counts),
    ClassLabels=NULL,Aggregate=NULL,Labels=NULL,ShapeCRS=4326,SimplifyKeep=1){
#   Plot the Choropleth Map
#   Connect measurements or class codes to polygon regions by postal code.
#   Return a static map and, if requested, an interactive Leaflet map.

#INPUT
#   Counts
#     Vector of measurements or class codes. Keep one value for each postal code.
#   
#   PostalCodes
#     Vector of postal codes. The function keeps leading zeros.
#   
#   NumberOfBins
#     Zero or one selects a continuous scale.
#     A larger value sets the maximum number of automatic bins.
#   
#   Breaks4Intervals
#     Optional finite boundaries in strictly increasing order.
#     The bins also include values below and above these boundaries.
#   
#   percentiles
#     Two ordered probabilities between zero and one for automatic boundaries.
#   
#   digits
#     Number of decimal places in bin labels.
#   
#   PostalCodesShapes
#     An sf polygon object with region identifiers and a coordinate reference system.
#     You can also supply the historical polygon table.
#   
#   PlotIt
#     Set TRUE to show the selected map.
#   
#   DiscreteColors
#     One color for each class or bin. Include empty classes and bins.
#   
#   HighColorContinuous
#     Color for the highest value on a continuous scale.
#   
#   LowColorContinuous
#     Color for the lowest value on a continuous scale.
#   
#   NAcolor
#     Color for a region with a missing value.
#   
#   ReferenceMap
#     Keep FALSE. The old Google interface is no longer available.
#   
#   main:
#     Plot title.
#   
#   legend
#     Legend title.
#   
#   Silent
#     Keep this argument for older calls. It does not suppress data warnings.
#   
#   Interactive
#     Set TRUE to create a Leaflet map with labels, popups, and zoom.
#   
#   Categorical
#     Set TRUE to use class codes without numeric bins.
#   
#   ClassLabels
#     Named vector of labels. Names identify classes. Order sets the legend.
#   
#   Aggregate
#     Optional function that combines numeric values for the same postal code.
#     Without this function, conflicting values cause an error.
#   
#   Labels
#     Optional text for each postal code. The function converts special HTML characters.
#   
#   ShapeCRS
#     Coordinate reference system of the historical table. Default: EPSG:4326.
#   
#   SimplifyKeep
#     Fraction of vertices to keep. Use a value greater than zero and at most one.

# OUTPUT
#   List of
#   chorR6obj   Environment with a render() function for older calls.
#   DataFrame   One input row for each postal code after aggregation.
#   Plot        Static ggplot object.
#   Widget      Leaflet map, or NULL.
#   MapData     sf polygons with values and labels.
#
# details
#   Static and interactive maps use the same postal codes and values.
#   External data files require an explicit download.
#
# author:   Michael Thrun
#

  .dv_map_require("sf")
  for (flag in list(PlotIt, ReferenceMap, Silent, Interactive, Categorical)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
      stop("PlotIt, ReferenceMap, Silent, Interactive and Categorical must be TRUE or FALSE.")
  }
  if (ReferenceMap)
    stop("The old Google interface is no longer available. Use ReferenceMap = FALSE. Add a reference layer to the returned Leaflet Widget.")
  if (!is.numeric(NumberOfBins) || length(NumberOfBins) != 1L ||
      !is.finite(NumberOfBins) || NumberOfBins < 0 || NumberOfBins != floor(NumberOfBins))
    stop("NumberOfBins must be a non-negative integer.")
  if (!is.numeric(percentiles) || length(percentiles) != 2L ||
      any(!is.finite(percentiles)) || any(percentiles < 0 | percentiles > 1) ||
      percentiles[1L] > percentiles[2L])
    stop("percentiles must contain two increasing probabilities between zero and one.")
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits))
    stop("digits must be one finite number.")
  if (!is.numeric(SimplifyKeep) || length(SimplifyKeep) != 1L ||
      !is.finite(SimplifyKeep) || SimplifyKeep <= 0 || SimplifyKeep > 1)
    stop("SimplifyKeep must be greater than zero and at most one.")
  codes <- .dv_map_codes(PostalCodes)
  if (!length(Counts) || length(Counts) != length(codes) || !is.null(dim(Counts)))
    stop("Counts and PostalCodes must be non-empty vectors of equal length.")
  if (!Categorical && (!is.numeric(Counts) || any(is.infinite(Counts))))
    stop("Continuous Counts must be numeric and contain no infinite values.")
  if (Categorical && !is.null(Aggregate))
    stop("Use Aggregate only for numeric measurements. Do not use it for class codes.")
  if (!is.null(Aggregate) && !is.function(Aggregate))
    stop("Aggregate must be a function or NULL.")
  if (!is.null(Labels) && (length(Labels) != length(codes) || !is.character(Labels)))
    stop("Labels must be a character vector with one element for each postal code.")
  values <- if (Categorical) as.character(Counts) else as.numeric(Counts)
  values[is.na(Counts)] <- NA
  regions <- unique(codes)
  groups <- split(seq_along(codes), factor(codes, levels = regions))
  # Accept identical duplicate values. Require an explicit rule when values differ.
  grouped <- lapply(groups, function(i) {
    if (length(unique(values[i])) > 1L && is.null(Aggregate))
      stop(paste0("Conflicting values for postal code ", codes[i[1L]],
                  ". Supply an explicit Aggregate function for numeric measurements."))
    v <- if (is.null(Aggregate)) values[i[1L]] else Aggregate(values[i])
    if (length(v) != 1L || (!Categorical && (!is.numeric(v) || is.infinite(v))))
      stop("Aggregate must return one finite numeric value or NA per postal code.")
    if (!is.null(Labels) && length(unique(Labels[i])) > 1L)
      stop(paste("Conflicting Labels for postal code", codes[i[1L]]))
    v
  })
  df <- data.frame(region = regions, value = unlist(grouped, use.names = FALSE),
                   stringsAsFactors = FALSE)
  if (!is.null(Labels)) df$label <- Labels[match(regions, codes)]
  discrete <- Categorical || NumberOfBins > 1
  if (Categorical) {
    if (!is.null(ClassLabels)) {
      if (!is.character(ClassLabels) || is.null(names(ClassLabels)) ||
          anyNA(ClassLabels) || anyNA(names(ClassLabels)) ||
          any(!nzchar(names(ClassLabels))) || anyDuplicated(names(ClassLabels)) ||
          anyDuplicated(ClassLabels))
        stop("ClassLabels must be a named character vector with unique codes and labels.")
      if (any(!is.na(df$value) & !df$value %in% names(ClassLabels)))
        stop("ClassLabels does not cover every observed class.")
      lev <- names(ClassLabels)
      display <- unname(ClassLabels)
    } else {
      lev <- if (is.factor(Counts)) levels(Counts) else sort(unique(values[!is.na(values)]))
      display <- lev
    }
    df$valueReal <- df$value
    df$value <- factor(df$value, levels = lev, labels = display)
  } else if (NumberOfBins > 1) {
    real <- df$value
    finite <- real[is.finite(real)]
    if (missing(Breaks4Intervals)) {
      breaks <- if (length(unique(finite)) > 1L) unique(as.numeric(stats::quantile(
        finite, probs = seq(percentiles[1L], percentiles[2L], length.out = NumberOfBins - 1L)))) else numeric()
    } else {
      breaks <- Breaks4Intervals
      if (!is.numeric(breaks) || !length(breaks) || any(!is.finite(breaks)) ||
          is.unsorted(breaks, strictly = TRUE))
        stop("Breaks4Intervals must be finite, strictly increasing numeric boundaries.")
    }
    # Include values outside the supplied boundaries. Do not lose either tail.
    edges <- c(-Inf, breaks, Inf)
    boundary <- function(x) ifelse(is.infinite(x), ifelse(x < 0, "-Inf", "Inf"),
                                   format(round(x, digits), trim = TRUE, scientific = FALSE))
    labels <- paste0("Bin ", seq_len(length(edges) - 1L), ": [",
                     boundary(head(edges, -1L)), ", ", boundary(tail(edges, -1L)), ")")
    df$valueReal <- real
    df$value <- cut(real, breaks = edges, labels = labels, right = FALSE)
    lev <- levels(df$value)
    display <- lev
  }
  if (discrete) {
    if (missing(DiscreteColors)) {
      colors <- grDevices::hcl(h = seq(15, 375, length.out = length(lev) + 1L)[seq_along(lev)], c = 100, l = 65)
    } else {
      colors <- DiscreteColors
      if (!is.null(names(colors))) {
        if (!all(lev %in% names(colors))) stop("Named DiscreteColors must cover all class codes or bin labels.")
        colors <- colors[lev]
      } else if (length(colors) != length(lev)) {
        stop("Supply one DiscreteColors entry per class or bin (including empty levels).")
      }
    }
    names(colors) <- display
    grDevices::col2rgb(c(colors, NAcolor))
  } else grDevices::col2rgb(c(LowColorContinuous, HighColorContinuous, NAcolor))
  if (missing(PostalCodesShapes)) {
    env <- new.env(parent = emptyenv())
    suppressWarnings(utils::data("GermanPostalCodesShapes", package = "DataVisualizations", envir = env))
    if (!exists("GermanPostalCodesShapes", envir = env, inherits = FALSE))
      stop("Supply PostalCodesShapes. Download GermanPostalCodesShapes separately from GitHub. This function does not download data.")
    PostalCodesShapes <- env$GermanPostalCodesShapes
  }
  map <- .dv_map_geometry(PostalCodesShapes, ShapeCRS)
  unknown <- setdiff(df$region, map$region)
  if (length(unknown)) warning(paste("Postal codes absent from shapes:", paste(unknown, collapse = ", ")))
  if (SimplifyKeep < 1) map <- .dv_map_simplify(map, SimplifyKeep)
  # Match postal codes before assigning values. Geometry order can differ from input order.
  j <- match(map$region, df$region)
  map$value <- df$value[j]
  if ("valueReal" %in% names(df)) map$valueReal <- df$valueReal[j]
  if (!is.null(Labels)) map$label <- df$label[j]
  plot <- ggplot2::ggplot(map) + ggplot2::geom_sf(ggplot2::aes(fill = .data[["value"]]),
    color = "#555555", linewidth = 0.1) + ggplot2::theme_void() + ggplot2::labs(title = main, fill = legend)
  if (discrete) {
    plot <- plot + ggplot2::scale_fill_manual(values = colors, drop = FALSE, na.value = NAcolor)
  } else {
    plot <- plot + ggplot2::scale_fill_gradient(low = LowColorContinuous,
        high = HighColorContinuous, na.value = NAcolor)
  }
  widget <- NULL
  if (Interactive) {
    .dv_map_require("leaflet")
    .dv_map_require("htmltools")
    webmap <- sf::st_transform(map, 4326)
    val <- ifelse(is.na(webmap$value), "No data / unresolved mapping", as.character(webmap$value))
    extra <- if (is.null(Labels)) rep("", nrow(webmap)) else ifelse(is.na(webmap$label), "", paste0(" | ", webmap$label))
    text <- paste0("Postal code: ", webmap$region, " | ", val, extra)
    if (discrete) {
      fill <- unname(colors[as.character(webmap$value)])
      fill[is.na(fill)] <- NAcolor
    } else {
      domain <- webmap$value[is.finite(webmap$value)]
      if (!length(domain)) domain <- c(0, 1)
      pal <- leaflet::colorNumeric(c(LowColorContinuous, HighColorContinuous), domain = domain, na.color = NAcolor)
      fill <- pal(webmap$value)
    }
    # Some color names have different values in R and CSS.
    # Convert colors to hexadecimal values to keep the map colors identical.
    web_color <- function(x) {
      rgba <- grDevices::col2rgb(x, alpha = TRUE)
      grDevices::rgb(rgba[1L, ], rgba[2L, ], rgba[3L, ], rgba[4L, ], maxColorValue = 255)
    }
    fill <- web_color(fill)
    widget <- leaflet::leaflet(webmap, options = leaflet::leafletOptions(preferCanvas = TRUE, zoomSnap = 0.1))
    widget <- leaflet::addPolygons(widget, layerId = webmap$region, fillColor = fill,
      fillOpacity = 1, color = "#555555", weight = 0.3, smoothFactor = 0,
      label = text, popup = htmltools::htmlEscape(text),
      labelOptions = leaflet::labelOptions(style = list("white-space" = "normal", "max-width" = "320px")),
      highlightOptions = leaflet::highlightOptions(weight = 2, color = "#222222", bringToFront = TRUE))
    if (discrete) {
      widget <- leaflet::addLegend(widget, opacity = 1, colors = web_color(c(unname(colors), NAcolor)),
        labels = c(names(colors), "No data / unresolved mapping"), title = htmltools::htmlEscape(legend))
    } else if (!any(is.finite(webmap$value))) {
      widget <- leaflet::addLegend(widget, opacity = 1, colors = web_color(NAcolor), labels = "No data",
        title = htmltools::htmlEscape(legend))
    } else {
      widget <- leaflet::addLegend(widget, opacity = 1, pal = pal, values = webmap$value,
        title = htmltools::htmlEscape(legend), na.label = "No data")
    }
    box <- sf::st_bbox(webmap)
    widget <- leaflet::fitBounds(widget, box[["xmin"]], box[["ymin"]], box[["xmax"]], box[["ymax"]])
  }
  # Keep the old render() call without creating a choroplethr R6 object.
  compatibility <- new.env(parent = emptyenv())
  compatibility$render <- local({ p <- plot; function() p })
  compatibility$render_with_reference_map <- function()
    stop("The old Google interface is no longer available. Use the Leaflet Widget.")
  class(compatibility) <- "ChoroplethmapCompatibility"
  result <- list(Plot = plot, Widget = widget, MapData = map, DataFrame = df, chorR6obj = compatibility)
  if (PlotIt) print(if (Interactive) widget else plot)
  invisible(result)
}

.dv_map_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    stop(paste0("Package '", package, "' is required for this operation. Install it explicitly."), call. = FALSE)
}

.dv_map_codes <- function(x) {
  if (!is.atomic(x) || !is.null(dim(x))) stop("Postal codes must be a vector.")
  if (is.numeric(x)) {
    if (any(!is.finite(x) | x < 0 | x != floor(x))) stop("Numeric postal codes must be non-negative integers.")
    x <- format(x, scientific = FALSE, trim = TRUE)
  }
  x <- trimws(as.character(x))
  if (anyNA(x) || any(!nzchar(x))) stop("Postal codes must not be missing or empty.")
  small <- grepl("^[0-9]{1,4}$", x)
  x[small] <- sprintf("%05d", as.integer(x[small]))
  x
}

.dv_map_geometry <- function(shapes, crs) {
  if (inherits(shapes, "sf")) {
    if (!"region" %in% names(shapes)) stop("sf shapes must have a region column.")
    if (is.na(sf::st_crs(shapes))) stop("sf shapes must have a declared coordinate reference system.")
    geometry <- sf::st_geometry(shapes)
    regions <- .dv_map_codes(shapes$region)
  } else {
    needed <- c("long", "lat", "order", "hole", "id", "group", "region")
    if (!is.data.frame(shapes) || !all(needed %in% names(shapes)))
      stop("Supply sf polygons with a region column. Alternatively, supply a historical table with long, lat, order, hole, id, group, and region.")
    if (!nrow(shapes) || anyNA(shapes[needed]) ||
        any(!is.finite(shapes$long)) || any(!is.finite(shapes$lat)))
      stop("Historical shape coordinates and identifiers must be complete and finite.")
    groups <- split(seq_len(nrow(shapes)), as.character(shapes$group))
    rings <- lapply(groups, function(i) {
      if (length(unique(shapes$id[i])) != 1L || length(unique(shapes$region[i])) != 1L ||
          length(unique(shapes$hole[i])) != 1L) stop("Each polygon group must identify one ring, region and hole flag.")
      i <- i[order(shapes$order[i])]
      xy <- as.matrix(shapes[i, c("long", "lat")])
      if (!all(xy[1L, ] == xy[nrow(xy), ])) xy <- rbind(xy, xy[1L, ])
      if (nrow(unique(xy)) < 3L) stop("A polygon ring needs at least three distinct points.")
      list(xy = xy, hole = shapes$hole[i[1L]], id = as.character(shapes$id[i[1L]]),
           region = as.character(shapes$region[i[1L]]))
    })
    by_id <- split(seq_along(rings), vapply(rings, `[[`, "", "id"))
    polygons <- lapply(by_id, function(i) {
      r <- rings[i]
      if (length(unique(vapply(r, `[[`, "", "region"))) != 1L) stop("Each shape id must belong to one region.")
      hole <- vapply(r, `[[`, FALSE, "hole")
      if (all(hole)) stop("A polygon has holes but no exterior ring.")
      outer <- lapply(r[!hole], function(x) list(x$xy))
      if (any(hole)) {
        shells <- sf::st_sfc(lapply(outer, sf::st_polygon))
        holes <- sf::st_sfc(lapply(r[hole], function(x) sf::st_polygon(list(x$xy))))
        owners <- sf::st_within(holes, shells)
        areas <- as.numeric(sf::st_area(shells))
        for (j in seq_along(owners)) {
          candidates <- owners[[j]]
          if (!length(candidates)) stop("A hole is not contained in an exterior ring.")
          # Use the smallest containing shell when shells are nested.
          owner <- candidates[which.min(areas[candidates])]
          outer[[owner]] <- c(outer[[owner]], list(r[hole][[j]]$xy))
        }
      }
      sf::st_multipolygon(outer)
    })
    regions <- .dv_map_codes(vapply(by_id, function(i) rings[[i[1L]]]$region, ""))
    geometry <- sf::st_sfc(polygons, crs = crs)
  }
  if (!length(geometry) || any(sf::st_is_empty(geometry)) ||
      !all(sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")))
    stop("Shapes must contain non-empty polygon or multipolygon geometries.")
  # Check ring topology in the coordinate plane.
  # Restore the CRS after repair. Do not change global sf/s2 settings.
  original_crs <- sf::st_crs(geometry)
  flat <- sf::st_set_crs(geometry, NA)
  valid <- sf::st_is_valid(flat)
  if (anyNA(valid)) stop("Cannot determine shape validity.")
  if (any(!valid)) {
    warning(paste(sum(!valid), "invalid geometries repaired using sf::st_make_valid."))
    flat[!valid] <- sf::st_make_valid(flat[!valid])
  }
  unique_regions <- unique(regions)
  by_region <- split(seq_along(regions), factor(regions, levels = unique_regions))
  geometry <- sf::st_sfc(lapply(by_region, function(i) {
    if (length(i) == 1L) flat[[i]] else sf::st_union(flat[i])[[1L]]
  }), crs = original_crs)
  if (any(sf::st_is_empty(geometry)) || !all(sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")))
    stop("Geometry repair did not retain all polygon regions.")
  sf::st_sf(region = unique_regions, geometry = sf::st_cast(geometry, "MULTIPOLYGON"))
}

.dv_map_simplify <- function(map, keep) {
  .dv_map_require("rmapshaper")
  # Protect each separate polygon and each hole as a feature.
  # Simplify shared boundaries together. Then remove the extra hole features.
  parts <- sf::st_cast(map[, "region", drop = FALSE], "POLYGON", warn = FALSE)
  parts$auxiliary <- FALSE
  holes <- list()
  for (g in sf::st_geometry(parts)) {
    if (length(g) > 1L) for (h in g[-1L])
      holes[[length(holes) + 1L]] <- sf::st_polygon(list(h))
  }
  protected <- parts
  if (length(holes)) {
    protected <- rbind(parts, sf::st_sf(region = rep("auxiliary", length(holes)),
      auxiliary = TRUE, geometry = sf::st_sfc(holes, crs = sf::st_crs(map))))
  }
  small <- rmapshaper::ms_simplify(protected, keep = keep,
    keep_shapes = TRUE, explode = FALSE)
  small <- small[!small$auxiliary, "region", drop = FALSE]
  valid <- sf::st_is_valid(sf::st_set_crs(sf::st_geometry(small), NA))
  if (anyNA(valid) || any(!valid))
    stop("Simplification damaged polygon topology. Increase SimplifyKeep or use 1 for full geometry.")
  small <- .dv_map_geometry(small, sf::st_crs(map))
  signature <- function(x) vapply(sf::st_geometry(x),
    function(g) c(parts = length(g), rings = sum(lengths(g))), c(parts = 0L, rings = 0L))
  if (!identical(map$region, small$region) || any(sf::st_is_empty(small)) ||
      !identical(signature(map), signature(small)))
    stop("Simplification changed region coverage, disconnected parts or holes. Increase SimplifyKeep or use 1.")
  small
}
