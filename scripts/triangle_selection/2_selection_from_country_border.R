library(dplyr)
library(terra)

boundary=terra::vect("") # here paste your file location, country boundary
triangles=terra::vect("") # here paste your file location, snow-track data

# Visualization
buffered_boundary = buffer(boundary, 20000)
plot(buffered_boundary)
plot(triangles, add = TRUE)

  # Triangles to be removed
triangles_within_buffer = terra::intersect(triangles, buffered_boundary)
plot(buffered_boundary)
plot(triangles_within_buffer, add = TRUE)

# Remove triangles inside the 20 km buffer
triangles_outside_buffer <- terra::erase(triangles, buffered_boundary)

plot(buffered_boundary)
plot(triangles_outside_buffer, add = TRUE)

# Save file
terra::writeVector(triangles_outside_buffer, "", overwrite=TRUE) # here paste your file location

