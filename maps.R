hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

tm_shape(mhi_s_cells) +
  tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
  tm_shape(hawaii,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title ="TESt") +
  tm_borders(lwd = 1.2) +
  tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"))

