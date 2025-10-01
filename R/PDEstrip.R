PDEstrip = function(Feature,
                          palette = c("blue","green","yellow","orange","red")) {

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