# Panel fusion function
# by Yorick Minnebo
# versio 2021

### Input of function
#ggplot object without any formatting 


### Arguments of function
# only the input object
Fusetoplayer <- function(ggplotobject, Type = "most"){
require(gtable); require(grid)
  
  if(Type == "most"){
## Combine top label rows
### Get the ggplot grob
pg = ggplotGrob(ggplotobject)

index = which(pg$layout$name == "strip-t-1"); 
strip1 = pg$grobs[[index]] # Get the left most strip
# grid.newpage(); grid.draw(strip1) # Draw the strip
# strip1$layout; gtable_show_layout(strip1) # Examine its layout

# Get a list of strips from the original plot
strip <- lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})

# Construct gtable to contain the new strip
newStrip  <- gtable(widths = unit(rep(1, 6), "null"), heights = strip[[1]]$heights) # rep(1,#panels below)

## Populate the gtable    
cols <- seq(1, by = 2, length.out = 3) # seq(start, by = panels under this row, length.out = panel amount)
newStrip <- gtable_add_grob(newStrip, 
                            lapply(strip[cols], `[`, 1), t = 1, l = cols, r = cols + 1) #r = cols + panels under row

# Put the strip into the plot
pgNew <- gtable_add_grob(pg, newStrip, t = 7, l = 5, r = 15)


  } else{
  if(Type == "DNA"){ 
    ## Combine top label rows
    pg <- ggplotGrob(plot)
    ### Get a list of strips from the original plot
    strip <- lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
    
    ### Construct gtable to contain the new strip
    newStrip <- gtable(widths = unit(rep(1, 18), "null"), heights = strip[[1]]$heights)
    
    ## Populate the gtable Top row
    cols <- seq(1, by = 3, length.out = 6)  # seq(start, by = panels under this row, length.out = panel amount)
    newStrip <- gtable_add_grob(newStrip, lapply(strip[cols], `[`, 1), 
                                t = 1, l = cols, r = cols + 2)
    # t = row, l = amount of cols, r = # of panels under this row Put the strip
    # into the plot
    pgNew <- gtable_add_grob(pg, newStrip, t = 7, l = 5, r = 39)
  } else {
    if(Type == "DNA_FS"){ 
      ## Combine top label rows
      pg <- ggplotGrob(plot)
      ### Get a list of strips from the original plot
      strip <- lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
      
      ### Construct gtable to contain the new strip
      newStrip <- gtable(widths = unit(rep(1, 24), "null"), heights = strip[[1]]$heights)
      
      ## Populate the gtable Top row
      cols <- seq(1, by = 4, length.out = 6)  # seq(start, by = panels under this row, length.out = panel amount)
      newStrip <- gtable_add_grob(newStrip, lapply(strip[cols], `[`, 1), 
                                  t = 1, l = cols, r = cols + 3)
      # t = row, l = amount of cols, r = # of panels under this row Put the strip
      # into the plot
      pgNew <- gtable_add_grob(pg, newStrip, t = 7, l = 5, r = 51)
    }
    }
  } 
  # Draw the plot
  # p<- grid.newpage();
  ggnewplotobject <- ggdraw() + draw_grob(pgNew)
return(ggnewplotobject)} 
