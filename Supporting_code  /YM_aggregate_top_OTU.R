# Get top n OTUs and merge remaining OTU's as other
# Yorick Minnebo
# 2020

aggregate_top_otu <- function(phylobject, top, measure = mean){
  # phylobject <- phylobj.prop.otu; top = 75
  OTUtable <- data.frame(otu_table(phylobject))
  OTUtable.df <- rownames_to_column(OTUtable)
  OTUtable.a <- data.frame(OTU=OTUtable.df[,1], Means=rowMeans(OTUtable.df[,-1]))
  OTUtable.topn <- top_n(OTUtable.a, top)
  
  # Top levels
  OTUtable.top <- OTUtable.df[OTUtable.df$rowname %in% OTUtable.topn$OTU,]
  row.names(OTUtable.top) <- OTUtable.top$rowname; OTUtable.top <- OTUtable.top[,-1]
  
  #Bottom half
  OTUtable.bot <- OTUtable.df[OTUtable.df$rowname %nin% OTUtable.topn$OTU,]
  OTUtable.other <- data.frame(t(as.data.frame(colSums(OTUtable.bot[,-1]))))
  row.names(OTUtable.other) <- "Other"
  
  OTUtable <- otu_table(rbind(OTUtable.top, OTUtable.other), taxa_are_rows = TRUE)
  
  
  newphylobject.tax <- subset_taxa(phylobject, rownames(tax_table(phylobject)) %in% OTUtable.topn$OTU)
  Taxtable <- data.frame(tax_table(newphylobject.tax))
  Taxtable$Species <- row.names(Taxtable) 
  Taxtable.other <- data.frame(Kingdom = "Other", Phylum = "Other", Class = "Other", 
                               Order = "Other", Family = "Other", Genus = "Other", Species = "Other"); row.names(Taxtable.other) <- "Other"
  TAXtable <- rbind(Taxtable, Taxtable.other)
  TAXtable <- tax_table(as.matrix(TAXtable))
  test <- data.frame(TAXtable)
  
  
  x <- data.frame(sample_data(phylobject)); x$sa <- paste0("X", x$Samplenumber)
  row.names(sampledata) <- sampledata$sa; sampledata2 <- sample_data(sampledata)
  
  # Create new phylobject
  newphylobject <- merge_phyloseq(OTUtable, TAXtable,sampledata2)
  return(newphylobject)
}
