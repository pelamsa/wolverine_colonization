library(terra)
library(sf)
library(data.table)

#### Import data ####

input_dir="" # here paste your input directory path
output_dir="" # here paste your output directory path
triangles <- vect(paste0(input_dir,"")) # here paste your file name, snow-track data


## Input environmental data to calculate distance
path.list=c(paste0(input_dir,''),
  paste0(input_dir,''),
  paste0(input_dir,''),
) # here paste your file names, add as many as needed

features_name.list=c("variable1", "variable2", "variable3") # you may change the names of the variables accordingly, add as many as needed

## Format data: Triangles 

dist_per_path.list=list() # an empty list which will receive the average distance tables for each file
for(i in 1:length(path.list)){ # Loop on each input vector/raster file
  ## Format data: Input data to calculate distance
  print(i)
  if(length(grep(pattern=".shp",x=path.list[i]))==1){ # check if x is a vector
    input_dist_feat=vect(path.list[i])
    input_dist_feat$features=1 # Add a new column so any features = 1
    r_base <- terra::rast(input_dist_feat,res=16,vals=NA) # Create an empty raster grid with extent from the input vector and a resolution = 100 x 100m
    rc <- ceiling(dim(r_base)[1:2] / c(3,3))
    filename <- paste0(tempfile(), "_.tif")
    crop_r_path <- terra::makeTiles(r_base, rc, filename)
    cropped_and_rasterized <- list() # empty list to store the cropped & rasterized vector files
    for (j in 1:length(crop_r_path)){
      r=rast(crop_r_path[[j]])
      cropped_vector <- terra::crop(input_dist_feat, r) # crop the vector with part of the divided raster
      if(length(cropped_vector)>0){
        rasterized_vector <- terra::rasterize(cropped_vector, r, "features") # rasterize the cropped vector, features = 1, no features = NA
        cropped_and_rasterized[[j]] <- rasterized_vector # store the rasterized vector in the list
      } else if(length(cropped_vector)==0){
        cropped_and_rasterized[[j]] <- r
      }
    }
    r_cropped_and_rasterized=terra::sprc(cropped_and_rasterized)
    input_dist_feat_raster <- terra::mosaic(r_cropped_and_rasterized) # Combine the rasterized parts into a single raster
  }else if(length(grep(pattern=".tif",x=path.list[i]))==1){ # check if x is a raster
    input_dist_feat_raster=rast(path.list[i])
    NAflag(input_dist_feat_raster)=0 # 0 replaced by NA
    NAflag(input_dist_feat_raster)=32766 # 32766 replaced by NA
  }
  #### Compute distance ####
  print("calculating distance")
  distance.rast=terra::distance(input_dist_feat_raster,unit="m") # computes the distance for all cells that are NA in the SpatRaster to the nearest cell that is not NA
  print("exporting raster")
  terra::writeRaster(distance.rast,paste0(output_dir,features_name.list[i],"_distance.tif"),overwrite=T) # export distance raster
  ##### Extract the mean distance per triangle and per buffer sizes ####
  
  dist_per_buffersize.list=list() # an empty list which will receive the average distance tables for each buffer size
  radius_buffer.vec=c(3131,20000) # selected buffers, radius in meters (based on the crs)
  
  for(j in 1:length(radius_buffer.vec)){ # loop on buffer sizes
    buffer_value=radius_buffer.vec[j]
    print("creating a buffer")
    points.svec=terra::buffer(points,buffer_value)  # Create buffer around the triangles
    print("extracting mean distance in buffer polygon")
    meandist.tab=terra::extract(distance.rast,points.svec,fun="mean")  # Extract mean of distance values in the buffer polygons
    print("saving results")
    dist_per_buffersize.list[[j]]=meandist.tab[,2] # Save the average distance table for each buffer size in a list
  }
  print("combining results from all elements")
  dist_selectfeature.tab=as.data.frame(base::do.call(cbind,dist_per_buffersize.list)) # combine all list elements in a single table --> one table per input file
  colnames(dist_selectfeature.tab)=paste0(features_name.list[i],"_",radius_buffer.vec) # assign column names based on the corresponding file
  dist_per_path.list[[i]]=dist_selectfeature.tab # save each single table (one table per input file) in an empty list
}

dist.tab=base::do.call(cbind,dist_per_path.list) # combine all list elements in a single table --> one table combining all results from the input files
dist.tab$triangle_ID=points$triangle_ID # Add associated triangle_ID in a column
fwrite(dist.tab,paste0(output_dir,"")) # here paste preferred name of the resulting file
