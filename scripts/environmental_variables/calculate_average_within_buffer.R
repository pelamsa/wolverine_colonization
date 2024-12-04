library(terra)
library(data.table)
library(dplyr)

points <- vect("") # here paste your file location
mean.rast = rast("") # here paste your file location

# calculate average values inside a buffer
dist_per_buffersize.list <- list()  # an empty list which will receive the average distance tables for each buffer size
radius_buffer.vec <- c(3131, 20000)  # radius in meters (based on the crs)

dist_per_path.list <- list()

raster_name <- names(mean.rast)

set.seed(123)
points <- points[base::sample(terra::nrow(points), 1),] # Ramdom selection of x triangles

for (i in 1:nrow(points)) {  # Loop over each point
  current_point <- points[i, ]
  dist_per_buffersize.list <- list()  # Reset the list for each point
  
  for (j in 1:length(radius_buffer.vec)) {  # loop on buffer sizes
    print(paste0("Point:", i))
    buffer_value <- radius_buffer.vec[j] 
    points.svec <- terra::buffer(current_point, buffer_value)  # Create buffer around the point
    all_values = terra::extract(mean.rast,points.svec,na.rm=TRUE)
    all_values_new <- all_values %>%
      mutate(raster_name = ifelse(raster_name == 32766, NA, raster_name)) # replace value 32766 (corresponds to NA) with NA
    mean_value <- mean(all_values_new[,2],na.rm=T)
    dist_per_buffersize.list[[j]] <- mean_value
  }
  
  dist_selectfeature.tab <- as.data.frame(base::do.call(cbind, dist_per_buffersize.list))  # Combine all list elements in a single table
  colnames(dist_selectfeature.tab) <- paste0(raster_name,"_", radius_buffer.vec)  # Assign column names based on the corresponding file
  dist_per_path.list[[i]] <- dist_selectfeature.tab  # Save each single table (one table per input file) in an empty list
}

dist.tab <- base::do.call(rbind, dist_per_path.list)  # Combine all list elements in a single table
dist.tab$triangle_ID <- points$triangle_ID  # Add associated triangle_ID in a column
fwrite(dist.tab, "") #Export table
