# Load my_Channel
channel <- my_channel

# Extract bank line and centerline and set CRS
b1 <- channel$bankline
b1 <- st_set_crs(b1, 32610)
c1 <- sx_centerline(channel)
c1 <- st_set_crs(c1, 32610)

# Sample points along the centerline
len <- sum(st_length(c1))
pts <- st_line_sample(c1, density = 100 / len)
# Extract the coordinates of the points and create a data frame
pts_df <- st_as_sf(data.frame(st_coordinates(pts)), coords = c("X", "Y"), crs = st_crs(c1))

# Plot to check
plot(b1)
plot(c1, add = TRUE)
plot(pts, add = TRUE, col = 'red')

# Bounding box and maximum distance for perpendicular lines
bb <- st_bbox(b1)
maxd <- sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2) 

# Extract the first resampled point
first_pt <- st_coordinates(pts)[1, ]


class(c1)
coordinates <- st_coordinates(c1)
#coordinates
number_of_points <- nrow(coordinates)
number_of_points

c1
st_geometry(c1)

# Find the nearest segment of the centerline to the first resampled point
distances_to_first_pt <- st_distance(c1, st_sfc(st_point(first_pt), crs = st_crs(c1)))
distances_to_first_pt 

nearest_segment_index <- which.min(distances_to_first_pt)
nearest_segment_index # so the first segment of the centerline is closest to first_pt.

# Ensure there is a next point to form a segment
if (nearest_segment_index < nrow(st_coordinates(c1))) {
  centerline_segment <- st_coordinates(c1)[nearest_segment_index:(nearest_segment_index + 1), ]
} else {
  centerline_segment <- st_coordinates(c1)[(nearest_segment_index - 1):nearest_segment_index, ]
}

# Calculate the direction of the centerline at the first resampled point
centerline_direction <- diff(centerline_segment)
centerline_direction

# Perpendicular direction
perp_dir <- c(centerline_direction[2], centerline_direction[1])
perp_dir <- perp_dir / sqrt(sum(perp_dir^2))  # Normalize

# Create long perpendicular line extending both directions
long_line <- st_sfc(st_linestring(rbind(first_pt - perp_dir * maxd, first_pt + perp_dir * maxd)), crs = st_crs(b1))
#untersection with bankline
intersections <- st_intersection(long_line, b1)
intersection_points <- st_cast(intersections, "POINT")

#dist from sampled points
distances <- st_distance(intersection_points, st_sfc(st_point(first_pt), crs = st_crs(b1)))
ordered_intersections <- intersection_points[order(distances), ]
# create siple feature object
width <- st_sfc(st_linestring(st_coordinates(ordered_intersections[c(1, 2)])), crs = st_crs(b1))
  
  # Plot the perpendicular line and the final intersection segment
  plot(b1)
  plot(c1, add = TRUE, col = 'blue')
  plot(pts_df, add = TRUE, col = 'red')
  plot(long_line, add = TRUE, col = "green")
  plot(intersection_points, add = TRUE, col =  "black")
  plot(final_segment, add = TRUE, col = "black")

