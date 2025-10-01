PDEstrip = function(Feature,
                          palette = c("blue","green","yellow","orange","red")) {
#PDEstrip(Feature)
  # PDEstrip: 1D density strip based on Pareto Density Estimation (PDE)
  # Renders a single variable’s probability density as a horizontal “heat strip”
  # where color encodes local density (blue/green = low, yellow = medium,
  # orange/red = high). Useful as a compact alternative to a violin/box plot for
  # one feature.
  #
  # Input
  # Feature              Numeric vector. Finite values are used for Pareto Density
  #                      Estimation (PDE). Non-finite values are ignored.
  # palette              Character vector of colors defining the low→high density
  #                      gradient (passed to ggplot2::scale_fill_gradientn()).
  #
  # Output
  # ggplotObj            A ggplot object showing a 1D density strip for Feature.
  #
  # Details
  # - Density is estimated via Pareto Density Estimation (PDE), which is robust
  #   and adaptive (Ultsch, 2005). The returned PDE values are min–max normalized
  #   to [0,1] and mapped to colors along the strip.
  # - The strip is drawn with geom_tile() at y = 1 to form a single band.
  # - The feature name in the x-axis label is captured from the call if possible.
  #
  # Notes
  # - To match multi-variable “strip” plots visually, you can adjust theme and
  #   color palette for consistency.
  #
  # Author
  # MT, 10/2025
  
  Name=deparse1(substitute(Feature))
  # Estimate density
  densV = ParetoDensityEstimation(Feature)
  
  # Normalize density to [0,1]
  y_scaled = (densV$paretoDensity - min(densV$paretoDensity)) / (max(densV$paretoDensity) - min(densV$paretoDensity))
  
  # Build data frame
  d = data.frame(x = densV$kernels,
                  y_scaled = y_scaled,
                  ypos = 1)
  
  # Plot as 1D heat strip
  dense_strip=ggplot2::ggplot(d, ggplot2::aes(x = x, y = ypos, fill = y_scaled)) +
    ggplot2::geom_tile(height = 0.9) +
    ggplot2::scale_fill_gradientn(colors = palette) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = Name, y = "") +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank())
  
  return(dense_strip)
}