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
pts_df <- st_as_sf(data.frame(st_coordinates(pts)), coords = c("X", "Y"),
                   crs = st_crs(cl))
pts_df

# plot to check
plot(bl)
plot(cl, add = TRUE)
plot(pts, add = TRUE, col = 'red')

# Get maximum distance from bounding box
bb <- st_bbox(bl)
maxd <- sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2)


# Extract the first resampled point
# first_pt <- st_coordinates(pts)[1, ]
first_pt <- pts[1]

# Move the whole channel so that first_pt is at the origin
bl_moved <- bl - first_pt

# Check: plot the updated channel, along with a point at the origin:
plot(bl_moved)
plot(st_point(c(0, 0)), add = T)

# Construct a horizonal line going through the origin; but make sure
# it's made up of three points -- the two ends, and the origin itself,
# to avoid issues downstream.
horizontal_line <- st_geometry(
  st_linestring(rbind(c(-maxd, 0), c(0, 0), c(maxd, 0)))
  #crs = st_crs(bl)
)

plot(horizontal_line, add = TRUE)

# Construct the rotation matrix for a given angle (deliberately the transpose
# of the usual rotation matrix because the matrix needs to be on the RHS of
# the line to be rotated)
angle <- -pi / 4
rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)),
                          ncol = 2)

# Rotate the horizontal line by the given angle
angled_line <- horizontal_line * rotation_matrix

# Plot the rotated line
plot(angled_line, add = TRUE, col = "blue")

# Find the intersection of the rotated line with the banklines, and split
# out each intersection segment as a separate feature.
intersections <- st_intersection(angled_line, bl_moved)
intersections <- st_cast(intersections, "LINESTRING")

# Plot the intersection points
plot(intersections, add = TRUE, col = "green", size = 5)

# Only take the intersection segment that goes through the origin.
foo <- st_intersects(intersections, st_point(c(0, 0)), sparse = FALSE)
relevant_segment <- intersections[foo]

# Calculate this segment's width
width <- st_distance(relevant_segment)








class(cl)
coordinates <- st_coordinates(cl)
number_of_points <- nrow(coordinates)
number_of_points


#Function to calculate the width for a given rotation angle
calculate_width <- function(angle, first_pt, cl, bl, maxd) {
  rotation_matrix <- matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    byrow = TRUE, ncol = 2
  )


  # Calculate the direction of the centerline at the first resampled point
  centerline_direction <- diff(centerline_segment) # (x, y)
  perp_dir <- c(-centerline_direction[2], centerline_direction[1]) #(-y, x)
  perp_dir <- perp_dir / sqrt(sum(perp_dir^2))  #

  # Rotate perpendicular direction by the given angle
  rotated_dir <- as.vector(rotation_matrix %*% perp_dir)

  # Create long perpendicular line extending both directions
  long_line <- st_sfc(st_linestring(rbind(
    first_pt - rotated_dir * maxd, first_pt + rotated_dir * maxd)
  ), crs = st_crs(bl))

  # Find intersections with banklines
  intersections <- st_intersection(long_line, bl)

  # Extract individual points from intersections
  intersection_points <- st_cast(intersections, "POINT")

  # Calculate distances from the sampled point to each intersection point
  distances <- st_distance(intersection_points, st_sfc(st_point(first_pt),
                                                       crs = st_crs(bl)))
  ordered_intersections <- intersection_points[order(distances), ]
  width <- as.numeric(st_distance(ordered_intersections[1],
                                  ordered_intersections[2]))
  return(width)
}


#at which the perpendicular cross-section (width) of the channel is the narrowest
#actually here we are looking for an angle ranging fro, -18 to 180 to minimize
## the width of channel
optimal_angle <- optimize(calculate_width, interval = c(-pi, pi),
                          first_pt = first_pt, cl = cl, bl = bl, maxd = maxd)
optimal_angle

# Optimal angle and minimum width
min_width <- optimal_angle$objective
optimal_angle <- optimal_angle$minimum

optimal_angle
min_width

# Plot the optimal perpendicular line
rotation_matrix <- matrix(c(cos(optimal_angle), -sin(optimal_angle),
                            sin(optimal_angle), cos(optimal_angle)), 2, 2)
perp_dir <- c(diff(st_coordinates(cl)[1:2, 2]), diff(st_coordinates(cl)[1:2, 1]))
perp_dir <- perp_dir / sqrt(sum(perp_dir^2))  # Normalize
rotated_dir <- as.vector(rotation_matrix %*% perp_dir)
long_line <- st_sfc(st_linestring(rbind(first_pt - rotated_dir * maxd,
                                        first_pt + rotated_dir * maxd)),
                    crs = st_crs(bl))
intersections <- st_intersection(long_line, bl)
intersection_points <- st_cast(intersections, "POINT")
distances <- st_distance(intersection_points, st_sfc(st_point(first_pt),
                                                     crs = st_crs(bl)))
ordered_intersections <- intersection_points[order(distances), ]
final_segment <- st_sfc(st_linestring(st_coordinates(ordered_intersections[c(1, 2)])),
                        crs = st_crs(bl))

plot(bl)
plot(cl, add = TRUE, col = 'blue')
plot(pts_df, add = TRUE, col = 'red')
plot(long_line, add = TRUE, col = "green")
plot(final_segment, add = TRUE, col = "black")



