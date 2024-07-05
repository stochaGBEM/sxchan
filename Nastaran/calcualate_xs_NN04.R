# load libraries
library(sf)
library(dplyr)
library(sxchan)
library(units)


# load my_Channel
channel <- my_channel
class(channel)
num <- 20 # replace with desired number of sample points

# Extract bankline and centerline
bl <- channel$bankline
bl <- st_set_crs(bl, 32610)
bl
class(bl)
cl <- sx_centerline(channel)
cl <- st_set_crs(cl, 32610)
cl

# Calculate length of centerline
len <- sum(st_length(cl))
len
class(cl)

# Sample points along the centerline
pts <- st_line_sample(cl, density = num / len)
pts
pts_df <- st_as_sf(data.frame(st_coordinates(pts)), coords = c("X", "Y"), crs = st_crs(cl))
pts_df

# plot to check
plot(bl)
plot(cl, add = TRUE)
plot(pts, add = TRUE, col = 'red')

# Get maximum distance from bounding box
bb <- st_bbox(bl)
maxd <- sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2)


# Extract the first resampled point
first_pt <- st_coordinates(pts)[1, ]


class(cl)
coordinates <- st_coordinates(cl)
number_of_points <- nrow(coordinates)
number_of_points


#Function to calculate the width for a given rotation angle
calculate_width <- function(angle, first_pt, cl, bl, maxd) {
  rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
  #calculate the dist from each pint on the centerline to the sample point
  distances_to_first_pt <- st_distance(cl, st_sfc(st_point(first_pt), crs = st_crs(cl))) #find where in cl the point is
  nearest_segment_index <- which.min(distances_to_first_pt)
  centerline_segment <- st_coordinates(cl)[nearest_segment_index:(nearest_segment_index + 1), ]
 
  
  # Calculate the direction of the centerline at the first resampled point
  centerline_direction <- diff(centerline_segment) # (x, y)
  perp_dir <- c(-centerline_direction[2], centerline_direction[1]) #(-y, x)
  perp_dir <- perp_dir / sqrt(sum(perp_dir^2))  #
  
  # Rotate perpendicular direction by the given angle
  rotated_dir <- as.vector(rotation_matrix %*% perp_dir)
  
  # Create long perpendicular line extending both directions
  long_line <- st_sfc(st_linestring(rbind(first_pt - rotated_dir * maxd, first_pt + rotated_dir * maxd)), crs = st_crs(bl))
  
  # Find intersections with banklines
  intersections <- st_intersection(long_line, bl)
  
  # Extract individual points from intersections
  intersection_points <- st_cast(intersections, "POINT")
  
  # Calculate distances from the sampled point to each intersection point
  distances <- st_distance(intersection_points, st_sfc(st_point(first_pt), crs = st_crs(bl)))
  ordered_intersections <- intersection_points[order(distances), ]
  width <- as.numeric(st_distance(ordered_intersections[1], ordered_intersections[2]))
  return(width)
}


#at which the perpendicular cross-section (width) of the channel is the narrowest 
#actually here we are looking for an angle ranging fro, -18 to 180 to minimize the width of channel
optimal_angle <- optimize(calculate_width, interval = c(-pi, pi), first_pt = first_pt, cl = cl, bl = bl, maxd = maxd)
optimal_angle

# Optimal angle and minimum width
min_width <- optimal_angle$objective
optimal_angle <- optimal_angle$minimum

optimal_angle
min_width

# Plot the optimal perpendicular line
rotation_matrix <- matrix(c(cos(optimal_angle), -sin(optimal_angle), sin(optimal_angle), cos(optimal_angle)), 2, 2)
perp_dir <- c(diff(st_coordinates(cl)[1:2, 2]), diff(st_coordinates(cl)[1:2, 1]))
perp_dir <- perp_dir / sqrt(sum(perp_dir^2))  # Normalize
rotated_dir <- as.vector(rotation_matrix %*% perp_dir)
long_line <- st_sfc(st_linestring(rbind(first_pt - rotated_dir * maxd, first_pt + rotated_dir * maxd)), crs = st_crs(bl))
intersections <- st_intersection(long_line, bl)
intersection_points <- st_cast(intersections, "POINT")
distances <- st_distance(intersection_points, st_sfc(st_point(first_pt), crs = st_crs(bl)))
ordered_intersections <- intersection_points[order(distances), ]
final_segment <- st_sfc(st_linestring(st_coordinates(ordered_intersections[c(1, 2)])), crs = st_crs(bl))

plot(bl)
plot(cl, add = TRUE, col = 'blue')
plot(pts_df, add = TRUE, col = 'red')
plot(long_line, add = TRUE, col = "green")
plot(final_segment, add = TRUE, col = "black")



