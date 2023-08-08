# Panel fusion function
# by Yorick Minnebo
# version 2021

### Input of function
#ggplot object without any formatting 


# Functions function
## Get effectsize
## Effect size The interpretation values commonly in published literature are: 
## 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate    effect) and >= 0.14 (large effect).
Effectsize <- function(data, formula = Abundance ~ Transit){
  effsize <- data %>% rstatix::kruskal_effsize(formula = formula)
    ## If it bugs
  if(is.nan(effsize$effsize) == TRUE){effsize <- paste0(" Presence of the genus is too low!")
  }else{
    if(effsize$effsize>0.13){
    effsize <- paste0(", effsize= ", signif(effsize$effsize, digits = 5)," (large effect size is detected)")
    }else{if(effsize$effsize>0.06){
      effsize <- paste0(", effsize= ", signif(effsize$effsize, digits = 5)," (moderate effect size is detected)")
      }else{effsize <- paste0(", effsize= ", signif(effsize$effsize, digits = 5)," (WARNING: low effect size is detected)")}}}
  return(effsize)
}

PossibleObjects <- function(){message("Objects are: \nplotprox: Proximal colon plot \nplotdist: Distal colon plot \ngglist: List of individidual plots\ngglist2: List of proximal and distal colon plot")}

statplot <- function(dataframe = Top, AboveZero = T, Legend = F, yaxis = F, title = F, plot = "Fancy", ymax = 1, samey = T, showstat = T){
  Top1 <- subset(dataframe, Genus == gen)
  if(AboveZero == T){Top1 <- subset(Top1, Abundance > 0); Top1$Abundance <- as.numeric(Top1$Abundance)}else{Top1$Abundance <- as.numeric(Top1$Abundance)}
  
  ## Proximal colon
  my_data <- subset(Top1, PD_Colon == "P"); unique(my_data$Transit); my_data$Transit <- droplevels(my_data$Transit)
  my_data2 <- my_data
  
  ### Will bug if only 1 transit: try to catch it
  skip_to_next <- FALSE; pvaluep <- 1
  tryCatch(res.kruskal <- my_data2 %>% rstatix::kruskal_test(Abundance ~ Transit), error = function(e) {skip_to_next <<- TRUE
    message(crayon::red("Only 1 transit time has", gen,  "in the proximal colon!
Skipped to next")) })
  
  if(skip_to_next == FALSE){
  ### Perform kruskal-wallis
  my_data2 <- my_data; res.kruskal <- my_data2 %>% rstatix::kruskal_test(Abundance ~ Transit)
  
  ### Pairwise comparisons using Wilcoxon’s test, minimal sample size = 6!!:
  pwc <- wilcox_test(my_data2, Abundance ~ Transit, p.adjust.method = correction, paired = F)
  if(ncol(pwc) == 7){pwc <- subset(pwc, p != "ns"); pwc$Vessel <- "P"; df.pwc2 <- rbind(df.pwc2, pwc)
  }else{pwc <- subset(pwc, p.adj.signif != "ns"); pwc$Vessel <- "P"; df.pwc <- rbind(df.pwc, pwc)}
  pvaluep <- res.kruskal$p
  ### print(pwc) # print pairwise comparisons
  if(showstat == T){
  if(res.kruskal$p < 0.05){
    message(crayon::green(paste0("Kruskal-Wallis: Genus = ",gen, ", ", "Proximal colon", ", p = ",res.kruskal$p, Effectsize(my_data2))))
  }else{
    message(crayon::red(paste0("Kruskal-Wallis: Genus = ",gen, ", ","Proximal colon", ", p = ",res.kruskal$p, Effectsize(my_data2))))}}
  
  ### Visualization: box plots with p-values
  pwc1 <- pwc %>% add_xy_position(x = "Transit"); my_data3 <- my_data2
  p<- papertheme(ggboxplot(my_data2, x = "Transit", y = "Abundance", fill = "Transit") +
                   stat_pvalue_manual(pwc1, hide.ns = TRUE) + labs(
                     # title = paste0(gen, ", ","Proximal colon"), 
                     subtitle = get_test_label(res.kruskal, detailed = TRUE),
                     caption = get_pwc_label(pwc1)) + scale_fill_manual(values = transitcolors))+
    facet_grid(rows = vars(PD_Colon), labeller = labeller(PD_Colon = Vessel.labs)) + 
    theme(strip.background = element_rect(fill = "#D6E0F4"), axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
  if(Legend == F){p <- p + theme(legend.position = "none")}
  if(yaxis == F){p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())}
  if(title == F){p <- p + theme(legend.title = element_blank())}
   gglist[[y]]<-p
  }else{
    my_data3 <- my_data2; gglist[[y]]<- ggplot() + theme_void() + ggtitle("Single transit time detected in the proximal colon")}
  y = y +1
  
   
  ## Distal colon
  my_data <- subset(Top1, PD_Colon == "D"); unique(my_data$Transit); my_data$Transit <- droplevels(my_data$Transit)
  my_data2 <- my_data
  
  ### Will bug if only 1 transit: try to catch it
  skip_to_next <- FALSE; pvalued <- 1
  tryCatch(res.kruskal <- my_data2 %>% rstatix::kruskal_test(Abundance ~ Transit), error = function(e) {
    message(crayon::red("Only 1 transit time has", gen,  "in the distal colon!
Skipped to next"))
    skip_to_next <<- TRUE})
  
  if(skip_to_next == FALSE){
  ### Perform kruskal-wallis
  res.kruskal <- my_data2 %>% rstatix::kruskal_test(Abundance ~ Transit)
  
  ### Pairwise comparisons using Wilcoxon’s test, minimal sample size = 6!!:
  pwc <- wilcox_test(my_data2, Abundance ~ Transit, p.adjust.method = correction, paired = F)
  if(ncol(pwc) == 7){pwc <- subset(pwc, p != "ns"); pwc$Vessel <- "D"; df.pwc2 <- rbind(df.pwc2, pwc)
  }else{pwc <- subset(pwc, p.adj.signif != "ns"); pwc$Vessel <- "D"; df.pwc <- rbind(df.pwc, pwc)}
  
  ### print(pwc) # print pairwise comparisons
  pvalued <- res.kruskal$p
  if(showstat == T){
  if(res.kruskal$p < 0.05){
    message(crayon::green(paste0("Kruskal-Wallis: Genus = ",gen, ", ", "Distal colon", ", p = ",res.kruskal$p, Effectsize(my_data2))))
  }else{
    message(crayon::red(paste0("Kruskal-Wallis: Genus = ",gen, ", ","Distal colon", ", p = ",res.kruskal$p, Effectsize(my_data2))))}}
  
  ### Visualization: box plots with p-values
  pwc <- pwc %>% add_xy_position(x = "Transit")
  p2<- papertheme(ggboxplot(my_data2, x = "Transit", y = "Abundance", fill = "Transit") +
                    stat_pvalue_manual(pwc, hide.ns = TRUE) + labs(
                      # title = paste0(gen, ", ","Distal colon"), 
                      subtitle = get_test_label(res.kruskal, detailed = TRUE),
                      caption = get_pwc_label(pwc)) + scale_fill_manual(values = transitcolors)) + 
    facet_grid(rows = vars(PD_Colon), labeller = labeller(PD_Colon = Vessel.labs)) + 
    theme(strip.background = element_rect(fill = "#D6E0F4"))
  if(Legend == F){p2 <- p2 + theme(legend.position = "none")}
  if(yaxis == F){p2 <- p2 + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())}
  if(title == F){p2 <- p2 + theme(legend.title = element_blank())}
  gglist[[y]]<-p2
  }else{gglist[[y]]<- ggplot() + theme_void() + ggtitle("Single transit time detected in the distal colon")}
  y = y +1
  
  if(samey == T){maxstat <- max(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range, ggplot_build(p2)$layout$panel_scales_y[[1]]$range$range)
  minstat <- min(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range, ggplot_build(p2)$layout$panel_scales_y[[1]]$range$range)
  p <- p + scale_y_continuous(limits = c(minstat, maxstat)); p2 <- p2 + scale_y_continuous(limits = c(minstat, maxstat))
  }
  plotstatistics <- ggarrange(p, p2, common.legend = T, legend = "bottom", nrow = 2)
  plotstatistics <- annotate_figure(plotstatistics, top = text_grob(gen, face = "italic", size = 20))
  
  
  # Fancy alternative
  if(plot == "Fancy"){my_data4 <- rbind(my_data3, my_data2)
  if(nrow(my_data4) == 0){test <- ggplot() + theme_void() + ggtitle(paste0("No abundances present for genus ", gen))}else{
  test <- papertheme(ggboxplot(my_data4, x = "Transit", y = "Abundance", fill = "Transit"))+ 
                       facet_grid(rows = vars(PD_Colon), labeller = labeller(PD_Colon = Vessel.labs))
  maximal <- max(ggplot_build(test)$layout$panel_scales_y[[1]]$range$range)
  if(pvaluep>0.05 & pvalued>0.05){test <- test
  }else{test2 <- test + stat_compare_means(comparisons = list(c("ST", "MT"), c("MT", "LT"), c("ST", "LT")), 
                                           label = "p.signif", hide.ns = T, hide.NS = T)
  mx <- max(ggplot_build(test2)$data[[2]]$y); mn <- min(ggplot_build(test2)$data[[2]]$y)
   
       test <- test + stat_compare_means(comparisons = list(c("ST", "MT"), c("MT", "LT"), c("ST", "LT")), 
                                         label = "p.signif", hide.ns = T, hide.NS = T,
                                     # label.y = max(pwc1$y.position, pwc$y.position),
                                     label.y = c(mn, mn, (mn+(mx-mn)/2))
                                     # label.y = maximal,
                                     )}
  test <- test + scale_fill_manual(values = transitcolors, labels = c("Short", "Medium", "Long")) + 
    xlab("Transit time") + 
    # scale_y_discrete(labels=c("ST"="Short","MT"="Medium", "LT"="Long")) +
    theme(
      # legend.position = "none",
      axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
      axis.text.y = element_blank(), strip.background = element_rect(fill = "#D6E0F4"),
      legend.title = element_text(face="bold"))
  }}
  
  
  if(plot == "Fancy"){return(test)}else{return(plotstatistics)}
}



rename_dates <- function (dataframe, FS = FALSE, type = "Top15") {
  # dataframe =functdatanettrt
  Top <- subset(dataframe, Donor == 1); Top <- subset(Top, Transit == "ST")
  Top2 <- Top[1,]; z <- 1
  Top3 <- Top2
  if(Top2$Day == min(Top$Day)){Top3$Day = 1}; if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
  if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}; if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
  if(Top2$Day == max(Top$Day)){Top3$Day = 5}; Top4 <- Top3
  for (y in c(2:nrow(Top))) {
    Top2 <- Top[y,]
    Top3 <- Top2
    if(Top2$Day == min(Top$Day)){Top3$Day = 1}
    if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
    if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}
    if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
    if(Top2$Day == max(Top$Day)){Top3$Day = 5}
    Top4 <- rbind(Top4, Top3); z <- z+1
  }
  for (i in c(1:6)) {
    if(FS == T){transit = c("ST", "MT", "LT", "FS")}else{transit = c("ST", "MT", "LT")}
    for(x in transit){
      
      if(type == "Top15"){
        Top <- subset(Top15, Donor == i); Top <- subset(Top, Transit == x); total <- nrow(Top15)
      }else{Top <- subset(dataframe, Donor == i); Top <- subset(Top, Transit == x); total <- nrow(dataframe)}
      Combo <- paste0(i, x)
      if((Combo!="1ST")){
        for (y in c(1:nrow(Top))) {
          
          Top2 <- Top[y,]
          Top3 <- Top2
          if(Top2$Day == min(Top$Day)){Top3$Day = 1}
          if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
          if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}
          if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
          if(Top2$Day == max(Top$Day)){Top3$Day = 5}
          Top4 <- rbind(Top4, Top3)
          z <- z+1
        }
      }
    }
    message(paste0(z,"/",total))
    if(z == total){ message("Done!")}
  }
  return(Top4)
}




vegan_otu <- function(physeq_object) {
  OTU <- phyloseq::otu_table(physeq_object)
  if (phyloseq::taxa_are_rows(OTU)) {
    OTU <- t(OTU)
  }
  return(methods::as(OTU, "matrix"))
}




# Aggregate the top taxa 
aggregate_top_taxa <- function (x, top, level) {
  x <- aggregate_taxa(x, level)
  tops <- top_taxa(x, top)
  tax <- tax_table(x)
  inds <- which(!rownames(tax) %in% tops)
  tax[inds, level] <- "Other"
  tax_table(x) <- tax
  tt <- tax_table(x)[, level]
  tax_table(x) <- tax_table(tt)
  aggregate_taxa(x, level)}


# Rename the dates in the data frame to 1, 2, 3, 4 or 5
rename_dates <- function (dataframe, FS = FALSE) {
  Top <- subset(dataframe, Donor == 1); Top <- subset(Top, Transit == "ST")
  Top2 <- Top[1,]; z <- 1
  Top3 <- Top2
  if(Top2$Day == min(Top$Day)){Top3$Day = 1}; if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
  if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}; if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
  if(Top2$Day == max(Top$Day)){Top3$Day = 5}; Top4 <- Top3
  for (y in c(2:nrow(Top))) {
    Top2 <- Top[y,]
    Top3 <- Top2
    if(Top2$Day == min(Top$Day)){Top3$Day = 1}
    if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
    if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}
    if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
    if(Top2$Day == max(Top$Day)){Top3$Day = 5}
    Top4 <- rbind(Top4, Top3); z <- z+1
  }
  for (i in c(1:6)) {
    if(FS == T){transit = c("ST", "MT", "LT", "FS")}else{transit = c("ST", "MT", "LT")}
    for(x in transit){
      Top <- subset(Top15, Donor == i); Top <- subset(Top, Transit == x)
      Combo <- paste0(i, x)
      if((Combo!="1ST")){
        for (y in c(1:nrow(Top))) {
          
          Top2 <- Top[y,]
          Top3 <- Top2
          if(Top2$Day == min(Top$Day)){Top3$Day = 1}
          if(Top2$Day == min(Top$Day)+1){Top3$Day = 2}
          if(Top2$Day == min(Top$Day)+2){Top3$Day = 3}
          if(Top2$Day == min(Top$Day)+3){Top3$Day = 4}
          if(Top2$Day == max(Top$Day)){Top3$Day = 5}
          Top4 <- rbind(Top4, Top3)
          z <- z+1
        }
      }
    }
    message(paste0(z,"/",nrow(Top15)))
    if(z == nrow(Top15)){ message("Done!")}
  }
  return(Top4)
}


eq <- function(x, y, type = "linear") {
  if (type == "linear") {
    m <- lm(y ~ x)
    as.character(as.expression(substitute(italic(y) == a + b %.% italic(x) * "," ~
                                            ~italic(r)^2 ~ "=" ~ r2, 
                                          list(a = format(coef(m)[[1]], scientific = T, digits = 2),
                                               b = format(coef(m)[[2]], scientific = T, digits = 2), 
                                               r2 = format(summary(m)$r.squared, digits = 3)))))
  } else {
    if (type == "poly") {
      # x = df$TT; y = df$TotalpermL
      m <- lm(y ~ x + I(x^2), data = df)
      as.character(as.expression(substitute(italic(y) == a + b %.% italic(x) -
                                              c %.% italic(x^2) * "," ~ ~italic(r)^2 ~ "=" ~ r2, 
                                            list(a = format(coef(m)[[1]], scientific = T, digits = 2),
                                                 b = format(coef(m)[[2]], scientific = T, digits = 2), 
                                                 c = format(abs(coef(m)[[3]]), scientific = T, digits = 2),
                                                 r2 = format(summary(m)$r.squared, digits = 3)))))
    } else {
      if (type == "log") {
        m <- lm(y ~ x)
        as.character(as.expression(substitute(italic(y) == a + b %.% italic(log(x)) *
                                                "," ~ ~italic(r)^2 ~ "=" ~ r2, 
                                              list(a = format(coef(m)[[1]], scientific = T, digits = 2), 
                                                   b = format(coef(m)[[2]], scientific = T, digits = 2),
                                                   r2 = format(summary(m)$r.squared, digits = 3)))))
      }
    }
  }
}



ig2ggplot <- function(g, dfOnly = TRUE, labels=FALSE, metab = TRUE, ...){
  . = 'shutup'
  x=NULL
  y=NULL
  name = NULL
  type=NULL
  group=NULL
  label=NULL
  layoutDF    = setNames(data.frame(layout.norm(g$layout, xmax=1, xmin=0, ymin=0, ymax=1)), c("x", "y"))
  vertexDF    = data.frame(id = unlist(V(g)$name), name = unlist(V(g)$label))
  edgeDF      = get.edgelist(g)
  edgeDF = as.matrix(data.frame(edgeDF[,1] %>% unlist, edgeDF[,2] %>% unlist))
  edgeDF = edgeDF[!duplicated(1:nrow(edgeDF) %>% lapply(function(x) paste(sort(edgeDF[x,]), collapse="")) %>% do.call(c,.)),]
  vertexDF2   = 1:nrow(edgeDF) %>% lapply(function(row){
    v1 = edgeDF[row,1]
    v2 = edgeDF[row,2]
    before = data.frame(layoutDF[which(V(g)$name == v1),], row    =  row, name =  v1, label  =  subset(vertexDF, id == v1)$name)
    after = data.frame(layoutDF[which(V(g)$name == v2),], row    =  row, name =  v2, label  =  subset(vertexDF, id == v2)$name)
    rbind(before, after)
  }) %>% do.call(rbind,.)
  
  vertexDF2$id = 1:nrow(vertexDF2)
  if(metab){
    vertexDF2$type = do.call(c,lapply(strsplit(x=as.character(vertexDF2$name), split=":"), function(x) x[[1]]))
  }else{
    vertexDF2$type = 1
    vertexDF2$type = as.factor(vertexDF2$type)
  }
  edgelists = 1:nrow(edgeDF) %>% lapply(function(row) {
    rbind(
      vertexDF2 %>% dplyr::select(x,y,name) %>% unique %>% dplyr::filter(name == edgeDF[row,1]) %>% dplyr::select(x,y) %>% dplyr::mutate(group=row),
      vertexDF2 %>% dplyr::select(x,y,name) %>% unique %>% dplyr::filter(name == edgeDF[row,2]) %>% dplyr::select(x,y) %>% dplyr::mutate(group=row)
    )
  }) %>% do.call(rbind,.)
  if(dfOnly){
    list(vertexDF2, edgelists)
  }else{
    p = ggplot(vertexDF2, aes(x, y))                                                                     +
      geom_line(data=edgelists, aes(x,y, group=group), color="grey", alpha=0.5)                         +
      geom_point(data=vertexDF2, aes(color=type, size=type, text=label)) +
      theme_void()
    if(labels){
      p + geom_text(aes(label=label))
    }else{
      p
    }
  }
}







# Make dataframe to add statistics with numbers to plot
stats_dataframe <- function(data, i, profile = NULL, dividerPC = 4, dividerDC = 4) {
  df1 <- data; i <- i
  # dividerPC <- dividerDC <- 4
  
  
  if(i == "Acidaminococcus" | i == "Otu00025" | i == "Otu00144" |i == "Otu00016" | i == "Otu00261"){
    dividerDC <- 2
  }
  
  if(i == "Otu00255"|i == "Akkermansia"|i == "Sutterella" | i == "Otu00029" | i == "Otu00306"){
    dividerPC <- 2
  }
  
  if(i == "Alistipes"|i == "Otu00018"| i == "Otu00037"|i == "Otu00055" | i == "Otu00092"| i == "Otu00119"| i == "Otu00129" | i == "Otu00135" | i == "Otu00147" | i == "Otu00228"){
    dividerPC <- 1
  }
  
  
  res.kruskal25P <- subset(df1, PD_Colon == "P") %>% rstatix::kruskal_test(Abundance ~ Transit)
  res.kruskal25D <- subset(df1, PD_Colon == "D") %>% rstatix::kruskal_test(Abundance ~ Transit)
  
  if (is.na(res.kruskal25P$p)) {res.kruskal25P$p <- 1}
  if (is.na(res.kruskal25D$p)) {res.kruskal25D$p <- 1}
  
  df1$Abundance[df1$Abundance == 0] <- runif(nrow(subset(df1, Abundance ==0)), 0, 0.1)
  
  if (res.kruskal25P$p >= 0.05 & res.kruskal25D$p >= 0.05) {
    correlations <- data.frame(
      label = c("a", "a", "a", "a", "a", "a"),
      PD_Colon = c("P", "P", "P", "D", "D", "D"),
      Abundance = c(
        max(subset(df1, PD_Colon == "P")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "P")$Abundance),2)[1])/dividerPC,
        max(subset(df1, PD_Colon == "P")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "P")$Abundance),2)[1])/dividerPC,
        max(subset(df1, PD_Colon == "P")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "P")$Abundance), 2)[1])/dividerPC,
        max(subset(df1, PD_Colon == "D")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC,
        max(subset(df1, PD_Colon == "D")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC,
        max(subset(df1, PD_Colon == "D")$Abundance) +
          (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC),
      xpos = c(xInf, xInf),
      ypos = c(yInf, yInf),
      TT1 = c(8, 16, 24, 8, 16, 24)
    )
    correlations$PD_Colon <- as.ordered(factor(correlations$PD_Colon, levels = c("P", "D")))
    TESTY <- T
  } else {
    if (res.kruskal25P$p < 0.05 & res.kruskal25D$p >= 0.05) {
      
      wilco <- subset(df1, PD_Colon == "P") %>%
        wilcox_test(Abundance ~ Transit, p.adjust.method = "holm", paired = F)
      wilco <- subset(wilco, p.adj.signif != "ns")
      
      if (nrow(wilco) == 3) {
        label <- c("a", "b", "c", "a", "a", "a")
      } else {
        if (nrow(wilco) == 2) {
          if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT" & wilco[2,2] == "ST" & wilco[2,3] == "LT") {
            label <- c("a", "b", "b", "a", "a", "a")
          } else {
            if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT" & wilco[2, 2] == "MT" & wilco[2,3] == "LT") {
              label <- c("a", "b", "a", "a", "a", "a")
            } else {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT" & wilco[2, 2] == "MT" &wilco[2, 3] == "LT") {
                label <- c("a", "a", "b", "a", "a", "a")
              }
            }
          }
        } else {
          if (nrow(wilco) == 1) {
            if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT") {
              label <- c("a", "b", "ab", "a", "a", "a")
            } else {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT") {
                label <- c("a", "ab", "b", "a", "a", "a")
              } else {
                if (wilco[1, 2] == "MT" & wilco[1, 3] == "LT") {
                  label <- c("ab", "a", "b", "a", "a", "a")
                }
              }
            }
          } else {
            label <- c("a", "a", "a", "a", "a", "a")
          }
        }
      }
      
      correlations <- data.frame(
        label = label, PD_Colon = c("P", "P", "P", "D", "D", "D"),
        Abundance = c(
          max(subset(df1, PD_Colon == "P")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "P" & Transit == "ST")$Abundance), 2)[1])/dividerPC,
          max(subset(df1, PD_Colon == "P")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "P" & Transit == "MT")$Abundance),2)[1])/dividerPC,
          max(subset(df1, PD_Colon == "P")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "P" & Transit == "LT")$Abundance),2)[1])/dividerPC,
          max(subset(df1, PD_Colon == "D")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC,
          max(subset(df1, PD_Colon == "D")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC,
          max(subset(df1, PD_Colon == "D")$Abundance) +
            (tail(sort(subset(df1, PD_Colon == "D")$Abundance),2)[1])/dividerDC),
        xpos = c(xInf, xInf),
        ypos = c(yInf, yInf),
        TT1 = c(8, 16, 24, 8, 16, 24)
      )
      correlations$PD_Colon <- as.ordered(factor(correlations$PD_Colon, levels = c("P", "D")))
      TESTY <- T
      
    } else {
      if (res.kruskal25P$p >= 0.05 & res.kruskal25D$p < 0.05) {
        if(i == "Otu00099"){df1$Abundance[df1$Abundance == 0] <- runif(nrow(subset(df1, Abundance ==0)), 0, 0.1)}
        wilco <- subset(df1, PD_Colon == "D") %>%
          wilcox_test(Abundance ~ Transit, p.adjust.method = "holm", paired = F)
        wilco <- subset(wilco, p.adj.signif != "ns")
        
        if (nrow(wilco) == 3) {
          label <- c("a", "a", "a", "a", "b", "c")
        } else {
          if (nrow(wilco) == 2) {
            if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT" & wilco[2, 2] == "ST" & wilco[2,3] == "LT") {
              label <- c("a", "a", "a", "a", "b", "b")
            } else {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT" & wilco[2, 2] == "MT" & wilco[2, 3] == "LT") {
                label <- c("a", "a", "a", "a", "b", "a")
              } else {
                if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT" & wilco[2, 2] == "MT" &wilco[2, 3] == "LT") {
                  label <- c("a", "a", "a", "a", "a", "b")
                }
              }
            }
          } else {
            if (nrow(wilco) == 1) {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT") {
                label <- c("a", "a", "a", "a", "b", "ab")
              } else {
                if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT") {
                  label <- c("a", "a", "a", "a", "ab", "b")
                } else {
                  if (wilco[1, 2] == "MT" & wilco[1, 3] == "LT") {
                    label <- c("a", "a", "a", "ab", "a", "b")
                  }
                }
              }
              
              
              
            } else {
              label <- c("a", "a", "a", "a", "a", "a")
            }
          }
        }
        
        correlations <- data.frame(
          label = label, PD_Colon = c("P", "P", "P", "D", "D", "D"),
          Abundance = c(
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "ST")$Abundance),2)[1])/dividerDC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "MT")$Abundance),2)[1])/dividerDC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "LT")$Abundance),2)[1])/dividerDC
          ),
          xpos = c(xInf, xInf),
          ypos = c(yInf, yInf),
          TT1 = c(8, 16, 24, 8, 16, 24)
        )
        correlations$PD_Colon <- as.ordered(factor(correlations$PD_Colon, levels = c("P", "D")))
        TESTY <- T
      } else {
        wilco <- subset(df1, PD_Colon == "P") %>%
          wilcox_test(Abundance ~ Transit, p.adjust.method = "holm", paired = F)
        wilco <- subset(wilco, p.adj.signif != "ns")
        
        if (nrow(wilco) == 3) {
          label <- c("a", "b", "c")
        } else {
          if (nrow(wilco) == 2) {
            if (wilco[1,2] == "ST" & wilco[1,3] == "MT" & wilco[2,2] == "ST" & wilco[2,3] == "LT") {
              label <- c("a", "b", "b")
            } else {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT" & wilco[2, 2] == "MT" & wilco[2, 3] == "LT") {
                label <- c("a", "b", "a")
              } else {
                if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT" & wilco[2, 2] == "MT" & wilco[2, 3] == "LT") {
                  label <- c("a", "a", "b")
                }
              }
            }
          } else {
            if (nrow(wilco) == 1) {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT") {
                label <- c("a", "b", "ab")
              } else {
                if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT") {
                  label <- c("a", "ab", "b")
                } else {
                  if (wilco[1, 2] == "MT" & wilco[1, 3] == "LT") {
                    label <- c("ab", "a", "b")
                  }
                }
              }
              
              
              
            } else {
              label <- c("a", "a", "a")
            }
          }
        }
        
        wilco <- subset(df1, PD_Colon == "D") %>%
          wilcox_test(Abundance ~ Transit, p.adjust.method = "holm", paired = F)
        wilco <- subset(wilco, p.adj.signif != "ns")
        
        if (nrow(wilco) ==3) {
          label <- c(label, "a", "b", "c")
        } else {
          if (nrow(wilco) ==2) {
            if (wilco[1,2] == "ST" & wilco[1,3] == "MT" & wilco[2,2] == "ST" & wilco[2,3] == "LT") {
              label <- c(label, "a", "b", "b")
            } else {
              if (wilco[1,2] == "ST" & wilco[1,3] == "MT" & wilco[2,2] == "MT" & wilco[2,3] == "LT") {
                label <- c(label, "a", "b", "a")
              } else {
                if (wilco[1,2] == "ST" & wilco[1,3] == "LT" & wilco[2,2] == "MT" & wilco[2,3] == "LT") {
                  label <- c(label, "a", "a", "b")
                }
              }
            }
          } else {
            if (nrow(wilco) ==1) {
              if (wilco[1, 2] == "ST" & wilco[1, 3] == "MT") {
                label <- c(label, "a", "b", "ab")
              } else {
                if (wilco[1, 2] == "ST" & wilco[1, 3] == "LT") {
                  label <- c(label, "a", "ab", "b")
                } else {
                  if (wilco[1, 2] == "MT" & wilco[1, 3] == "LT") {
                    label <- c(label, "ab", "a", "b")
                  }
                }
              }
              
              
              
            } else {
              label <- c(label, "a", "a", "a")
            }
          }
        }
        correlations <- data.frame(
          label = label, PD_Colon = c("P", "P", "P", "D", "D", "D"),
          Abundance = c(
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P" & Transit == "ST")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P" & Transit == "MT")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "P")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "P" & Transit == "LT")$Abundance),2)[1])/dividerPC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "ST")$Abundance),2)[1])/dividerDC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "MT")$Abundance),2)[1])/dividerDC,
            max(subset(df1, PD_Colon == "D")$Abundance) +
              (tail(sort(subset(df1, PD_Colon == "D" & Transit == "LT")$Abundance),2)[1])/dividerDC
          ),
          xpos = c(xInf, xInf),
          ypos = c(yInf, yInf),
          TT1 = c(8, 16, 24, 8, 16, 24)
        )
        correlations$PD_Colon <- as.ordered(factor(correlations$PD_Colon, levels = c("P", "D")))
        TESTY <- T
        
      }
    }
  }
  
 

  
  
  # Manually change stome stuff
  if(i == "Otu00069"| i == "Otu00099"| i == "Otu00112"| i == "Otu00177"| i == "Otu00237"| i == "Otu00305"| i == "Otu00323"| 
     i == "Otu00353"| i == "Otu00455"| i == "Otu00062"| i == "Otu00217"| i == "Otu00266"| i == "Otu00384"| i == "Otu00051"){
    if(profile == "quant"){
      corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *4
      correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
    }else{
      corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance 
      correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
    }}
  
  
  if(i == "Akkermansia"| i == "Otu00018"){
    if(profile == "quant"){
    corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *2
    correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
    }else{
      corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *2
      correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
    }}
  
  if(i == "Alistipes"| i == "Otu00056"| i == "Otu00082" | i == "Eisenbergiella"){
    corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *1.5
    correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  if(i == "Otu00255"& profile == "quant"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *1.5
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  if(i == "Otu00306"& profile == "quant"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *3
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  if(i == "Otu00377"& profile == "quant"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance *3
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  if(i == "Otu00306"& profile == "prop"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance/2
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  if(i == "Otu00377"& profile == "prop"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- corP$Abundance/2
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
  
  
  if(i == "Otu00021"){corD <- subset(correlations, PD_Colon == "D"); corD$Abundance <- corD$Abundance*2
  correlations <- rbind(corD, subset(correlations, PD_Colon == "P"))
  }
  
    if(i == "Otu00232"& profile == "quant"){corP <- subset(correlations, PD_Colon == "P"); corP$Abundance <- 0.5E+07
  correlations <- rbind(corP, subset(correlations, PD_Colon == "D"))
  }
 
  
  correlations$PD_Colon <- as.ordered(factor(correlations$PD_Colon, levels = c("P", "D")))
  
  return(correlations)
}
