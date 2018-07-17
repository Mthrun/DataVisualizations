DualAxisLineChart=function(X,Y1,Y2,xlab='X',y1lab='Y1',y2lab='Y2',main='Dual Axis Line Chart',cols=c('black','blue'),SaveIt=FALSE){
  requireNamespace('plotly')

  
  p <- plotly::plot_ly() 
    p <- plotly::add_lines(p,x = ~X, y = ~Y1, name = y1lab, line = list(color=cols[1])) 
    p <-  plotly::add_lines(p,x = ~X, y = ~Y2, name = y2lab, yaxis = "y2", line = list(color=cols[2])) 
    p <-  plotly::layout(p,
      title = main, 
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = y2lab,
        showgrid=FALSE
      ),
      xaxis = list(title=xlab,
                   showgrid=FALSE),
      yaxis= list(
        title=y1lab,
        showgrid=FALSE
        )
    )
	p
  if(SaveIt){
    requireNamespace('htmlwidgets')
    htmlwidgets::saveWidget(p, file = "DualAxisLineChart.html")
  }
  return(p)
}