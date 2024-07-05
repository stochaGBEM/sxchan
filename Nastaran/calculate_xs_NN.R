# load libraries
library(sf)
library(dplyr)
library(sxchan)
library(units)


# load my_Channel
channel <- my_channel
class(channel)


# extract bank line and centerline and set crs
b1 <- channel$bankline
b1 <- st_set_crs(b1, 32610)
c1 <- sx_centerline(channel)
c1 <- st_set_crs(c1, 32610)




# Sample points along the centerline
len <- sum(st_length(c1))
pts <- st_line_sample(c1, density = 50 / len)
# extract the coordinates of the points and creat a df from that
pts_df <- st_as_sf(data.frame(st_coordinates(pts)), coords = c("X", "Y"), crs = st_crs(c1))


# plot to check
#plot(b1)
#plot(c1, add = TRUE)
#plot(pts, add = TRUE, col = 'red')


# Bounding box and maximum distance for perpendicular lines
bb <- st_bbox(b1)
# This should be a bit bigger than the maximum width otherwise the calculation would be wrong
# should be consider median? the it wont work on wide reaches
maxd <- sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2)- 1200


# create fun to generate lines at different angels
#This function generates a line extending in both directions from a given point at a specific angle.
create_line_at_angle <- function(pt, angle, distance) {
  radians <- angle * pi / 180
  # calculate direction based on angle
  direction <- c(cos(radians), sin(radians))
  # creates coordinates for the line by extending in both directions from the point
  line_coords <- rbind(pt, pt + direction * distance, pt - direction * distance)
  st_sfc(st_linestring(line_coords), crs = st_crs(b1))
}



# Loop through points and angles to create and find intersections
angles <- seq(0,180, by = 1)

smallest_intersections <- list()

for (i in 1:nrow(pts_df)) {
  pt <- st_coordinates(pts)[i, ]
  min_length <- set_units(Inf, "m")
  smallest_intersection <- NULL
  
  for (angle in angles) {
    angled_line <- create_line_at_angle(pt, angle, maxd)
    
    # Find intersections with banklines
    intersections <- st_intersection(angled_line, b1)
    
    if (length(intersections) > 0) {
      lengths <- st_length(intersections)
      if (min(lengths) < min_length) {
        min_length <- min(lengths)
        smallest_intersection <- intersections[which.min(lengths)]
      }
    }
  }
  
  if (!is.null(smallest_intersection)) {
    smallest_intersections[[i]] <- smallest_intersection
  }
}


# Plot the smallest intersections
plot(b1)
plot(c1, add = TRUE, col = 'blue')
plot(pts_df, add = TRUE, col = 'red')

for (intersection in smallest_intersections) {
  if (!is.null(intersection)) {
    plot(intersection, add = TRUE, col = 'green')
  }
}



