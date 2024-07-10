# Flow:
# 0. External function: bank_to_bank(this_point, bl, angle) --> generates segment bank to bank.
# 1. Sample points along centerline.
# 2. make function to optim angle.

#' Generate a line segment from bank to bank
#'
#' Given a point within a channel, generate a line segment that goes from
#' bank to bank, for a specified angle.
#' @param pt A point within the channel.
#' @param bl The bankline of the channel.
#' @param angle The angle of the line segment, in radians.
#' @return A line segment from bank to bank.
#' @export
bank_to_bank <- function(pt, banks, angle) {
  bb <- sf::st_bbox(bl)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  bank_to_bank_engine(
    pt, banks, angle, maxd = maxd, intersect = TRUE, reposition = TRUE
  )
}

bank_to_bank_engine <- function(pt, banks, angle, maxd, intersect, reposition) {
  # Move the whole channel so that first_pt is at the origin
  bl_moved <- banks - pt
  # Construct a horizonal line going through the origin; but make sure
  # it's made up of three points -- the two ends, and the origin itself,
  # so that when it's rotated, (0, 0) is always on the line (otherwise,
  # it wouldn't be, as a result of rounding errors)
  horizontal_line <- sf::st_geometry(
    sf::st_linestring(rbind(c(-maxd, 0), c(0, 0), c(maxd, 0)))
    #crs = st_crs(bl)
  )
  # Construct the rotation matrix for a given angle (deliberately the transpose
  # of the usual rotation matrix because the matrix needs to be on the RHS of
  # the line to be rotated)
  cos_angle <- cos(angle)
  sin_angle <- sin(angle)
  rotation_matrix <- matrix(
    c(cos_angle, -sin_angle, sin_angle, cos_angle),
    ncol = 2
  )
  # Rotate the horizontal line by the given angle
  angled_line <- horizontal_line * rotation_matrix
  if (!intersect) {
    if (reposition) {
      return(angled_line + pt)
    } else {
      return(angled_line)
    }
  }
  # Find the intersection of the rotated line with the banklines, and split
  # out each intersection segment as a separate feature.
  intersections <- sf::st_intersection(angled_line, bl_moved)
  intersections <- sf::st_cast(intersections, "LINESTRING")
  # Only take the intersection segment that goes through the origin.
  foo <- sf::st_intersects(intersections, sf::st_point(c(0, 0)), sparse = FALSE)
  relevant_segment <- intersections[foo]
  if (reposition) {
    return(relevant_segment + pt)
  } else {
    return(relevant_segment)
  }
}
