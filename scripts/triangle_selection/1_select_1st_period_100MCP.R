library(sf)
library(dplyr)
library(ggplot2)

# Read the file
triangle_data <- st_read("") # here paste your file location

# Select rows from 2009 and 2010 that have 0 observations
selected_2009_2010 <- triangle_data %>%
  filter(year %in% c(2009, 2010) & n_tracks == 0)

# check selected triangles
ggplot(data = selected_2009_2010) +
  geom_sf() +
  theme_minimal() +
  labs(x = "lon", y = "lat")

# 100 % MCP polygon
MCP_poly <- st_read("") # here paste your file location
st_crs(MCP_poly) = 3067

# Select triangles within 100 % MCP
points_within_mcp <- selected_2009_2010 %>%
  filter(lengths(st_within(geometry, MCP_poly)) > 0)

# Check results
ggplot(data = points_within_mcp) +
  # Plot the points
  geom_sf(color = "black", size = 2) +
  # Add the polygon layer
  geom_sf(data = MCP_poly, fill = NA, color = "red", size = 1) +
  # Styling and labels
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

# save results
st_write(points_within_mcp, "") # here paste your preferred saving location and file name
