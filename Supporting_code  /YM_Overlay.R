# Title: Nutrient colour overlay
# Author: Yorick Minnebo 
# Date: April 2019

### Input of function
# ggplot object

### WHAT THIS FUNCTION DOES
# Adds a nutrient overlay on the graphs

### Arguments of function
# Only the input object

nutrientoverlay <- function(ggplotobject, ymx, ymn, size, color = "blue", y0 = 0, x0 = 0){
  library(ggplot2) 
   
if (color == "BW") {
  ggplotnewobject <- ggplotobject + 
   geom_rect(mapping=aes(xmin=x0 + 1, xmax=x0, ymin=ymn, ymax=ymx), fill="#005800") 
   
} else { 
  if (color == "colours") {
    ggplotnewobject <- ggplotobject + 
      annotate("rect", xmin=x0 + 0.7, xmax=x0 + 5.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#d18fa9")+
      annotate("rect", xmin=x0 + 5.5, xmax=x0 + 10.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#d1b78f")+
      annotate("rect", xmin=x0 + 10.5, xmax=x0 + 15.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#8fd1b7")+
      annotate("rect", xmin=x0 + 15.5, xmax=x0 + 20.3, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#8fa9d1")+
      annotate("text", x =x0 + 3, y = ymx-(ymx-ymn)/2, label = "100%", size = size, family='sans') +
      annotate("text", x =x0 + 8, y = ymx-(ymx-ymn)/2, label = "33%", size = size, family='sans') +
      annotate("text", x =x0 + 13, y = ymx-(ymx-ymn)/2, label = "20%", size = size, family = 'sans') +
      annotate("text", x =x0 + 18, y = ymx-(ymx-ymn)/2, label = "10%", size = size, family = 'sans')
  } else {
    if (color == "colours2") {
      ggplotnewobject <- ggplotobject + 
        annotate("rect", xmin=x0 + 0.7, xmax=x0 + 5.5, ymin=0, ymax=Inf, alpha=0.2, fill="#d18fa9")+
        annotate("rect", xmin=x0 + 5.5, xmax=x0 + 10.5, ymin=0, ymax=Inf, alpha=0.2, fill="#d1b78f")+
        annotate("rect", xmin=x0 + 10.5, xmax=x0 + 15.5, ymin=0, ymax=Inf, alpha=0.2, fill="#8fd1b7")+
        annotate("rect", xmin=x0 + 15.5, xmax=x0 + 20.3, ymin=0, ymax=Inf, alpha=0.2, fill="#8fa9d1")+
        annotate("text", x =x0 + 3, y = ymx-(ymx-ymn)/2, label = "100%", size = size, family='sans') +
        annotate("text", x =x0 + 8, y = ymx-(ymx-ymn)/2, label = "33%", size = size, family='sans') +
        annotate("text", x =x0 + 13, y = ymx-(ymx-ymn)/2, label = "20%", size = size, family = 'sans') +
        annotate("text", x =x0 + 18, y = ymx-(ymx-ymn)/2, label = "10%", size = size, family = 'sans')
    } else{
  # color  #3264C8
  ggplotnewobject <- ggplotobject + 
    geom_rect(mapping=aes(xmin=x0 + 1, xmax=x0 + 5.5, ymin=ymn, ymax=ymx), fill="#5A83D3") +
    geom_rect(mapping=aes(xmin=x0 + 5.5, xmax=x0 + 10.5, ymin=ymn, ymax=ymx), fill="#84A2DE") +
    geom_rect(mapping=aes(xmin=x0 + 10.5, xmax=x0 + 15.5, ymin=ymn, ymax=ymx), fill="#ADC1E9") +
    geom_rect(mapping=aes(xmin=x0 + 15.5, xmax=x0 + 20, ymin=ymn, ymax=ymx), fill="#D6E0F4") +
    annotate("text", x =x0 + 3, y = ymx-(ymx-ymn)/2, label = "control", size = size, family='sans') +
    annotate("text", x =x0 + 8, y = ymx-(ymx-ymn)/2, label = "33%", size = size, family='sans') +
    annotate("text", x =x0 + 13, y = ymx-(ymx-ymn)/2, label = "20%", size = size, family = 'sans') +
    annotate("text", x =x0 + 18, y = ymx-(ymx-ymn)/2, label = "10%", size = size, family = 'sans') 
    # geom_segment(aes(x = 1, y = y0, xend = 1, yend = ymx, colour = "blue"))
            
}}}
  
return(ggplotnewobject)  }
