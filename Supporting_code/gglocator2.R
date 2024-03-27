



gglocator <- function(ggobj,nrofpoints) {
  # Extract coordinates
  xr <- ggplot_build(ggobj)$layout$panel_scales_x[[1]]$range
  yr <- ggplot_build(ggobj)$layout$panel_scales_y[[1]]$range
  
  # Variable for selected points
  selection <- data.frame(x = as.numeric(), y = as.numeric())
  colnames(selection) <- c(ggobj$plot$mapping$x, ggobj$plot$mapping$y)
  
  # Detect and move to plot area viewport
  suppressWarnings(print(ggobj$plot))
  panels <- unlist(current.vpTree()) %>%
    grep("panel", ., fixed = TRUE, value = TRUE)
  
  n_panels <- length(panels)
  if(n_panels == 0){
    stop("ggmap plot not detected in current device")
  }
  
  seekViewport(panels, recording=TRUE)
  pushViewport(viewport(width=1, height=1))
  
  # Select point, plot, store and repeat
  for (i in 1:nrofpoints){
    tmp <- grid.locator('native')
    if (is.null(tmp)) break
    grid.points(tmp$x,tmp$y, pch = 16, gp=gpar(cex=0.5, col="darkred"))
    selection[i, ] <- as.numeric(tmp)
  }
  grid.polygon(x= unit(selection[,1], "native"), y= unit(selection[,2], "native"), gp=gpar(fill=NA))
  #return a data frame with the coordinates of the selection
  return(selection)
}

