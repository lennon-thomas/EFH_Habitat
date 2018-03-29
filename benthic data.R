#MHI

mosaicList <- function(rasList){
  
  #Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  raster.list$fun <- sum
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}

raster_files <- list.files(path =paste0(boxdir,"Hawaii_IKONOS"),pattern = ".tif",full.names = TRUE )

hawaii_benthic_layers <- lapply(raster_files,raster)

plot(hawaii_benthic_layers[[1]][2])
require(rgdal) 
data = readGDAL( paste0(boxdir,"Hawaii_IKONOS/Hawaii_East.tif") )

                