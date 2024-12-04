library(terra)
library(sf)
library(units)

input_dir="" # here paste your path
output_dir="" # here paste your path

# Import shapefile as SpatVect object
points.SpatVec=terra::vect(paste0(input_dir,"")) # here paste your file name, snow-track data

# Extract the attribute table from the shapefile
points.tab <- as.data.frame(points.SpatVec)

# Filter duplicated triangle_ID values in the shapefile to keep only unique locations
points=points.SpatVec[which(duplicated(points.tab$triangle_ID) == F)]

# overwrite the attribute table with the one of the new filtered shapefile
points.tab <- as.data.frame(points)

#### Compute distance between locations ####
# Transform the SpatVector object into st_geometry
points=sf::st_as_sf(points)
surveyed_dist=10000 # set preferred distance

shortdist_points <- data.frame(x=c(1,1,1)) # a first shortdist_points df to start the loop

# as long as there are points too close together, removes them randomly one by one
while(nrow(shortdist_points)>2){
  print(nrow(shortdist_points))
  # Compute Euclidian distance between pairs of geometries in meters
  m <- st_distance(points)
  # Replace values of diagonal and lower triangle with NA (To exclude duplicated distances and diagonal)
  m[lower.tri(m,diag=TRUE)] <- NA
  
  ### Search for spatial  features at distances < XX m ###
  # assign a distance to check in meters XX 
  # transform in df to assign colnames and rownames
  dist.df=base::as.data.frame(m)
  # the output of the st_distance gives "Units numeric" format, not possible to work with "data frame" and "which" after
  dist.df  <- drop_units(dist.df) 
  # assign colnames using triangle_ID
  colnames(dist.df)=points.tab$triangle_ID 
  # assign rownames using triangle_ID
  rownames(dist.df)=points.tab$triangle_ID 
  
  # Create a data frame to summarize the distance < XXm and their associated triangle_ID, 
  # The which function searches for colnums and rownums corresponding to distance below XXm in the dist.df table (condition "dist.df<surveyed_dist")
  check_points=as.data.frame(which(dist.df<surveyed_dist,arr.ind=T))
  # create a new column with the corresponding triangle_ID using check_points$row (= row number in dist.df)
  check_points$triangle_IDrow=rownames(dist.df)[check_points$row]
  # # create a new column with the corresponding triangle_ID using check_points$col (= column number in dist.df)
  check_points$triangle_IDcol=colnames(dist.df)[check_points$col]
  # create a new column with the corresponding distance
  check_points$dist=dist.df[which(dist.df<surveyed_dist,arr.ind=T)]
  
  # Filter the corresponding points in the shapefile
  shortdist_points=points[which(points.tab$triangle_ID %in% as.numeric(check_points$triangle_IDrow) | points.tab$triangle_ID %in% as.numeric(check_points$triangle_IDcol)),]
  
  set.seed(123) # remove one point randomly from the points spatial data and create the associated points.tab
  shortdist_points_remove=shortdist_points[sample(nrow(shortdist_points),size=1),]
  points=points[which(points$triangle_ID!=shortdist_points_remove$triangle_ID),]
  points.tab=as.data.frame(points)
  
  # Visualize
  plot(st_geometry(points))
  plot(st_geometry(shortdist_points),col="red",add=TRUE)
  plot(st_geometry(shortdist_points_remove),col="green",add=TRUE)
}
crs(points)

# Select 2018-2021 data with corresponding ID
triangle_data <- st_read("") # here paste your file location, ORIGINAL snow-track data

unique_IDs <- points %>%
  pull(triangle_ID) %>%
  unique()

selection_2018_2021 <- triangle_data %>%
  filter(year %in% c(2018, 2019, 2020, 2021) & triangle_ID %in% unique_IDs)

# Export the shapefile
write.csv(selection_2018_2021,paste0(output_dir,"",surveyed_dist,"m.csv")) # here paste your preferred file location & name

