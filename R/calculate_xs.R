#' Generate cross sections from a channel
#'
#' @param bankline,centerline Bankline and centerline
#' @param num Number of cross sections to take from the channel.
#' @returns A list of cross sections.
calculate_xs <- function(channel, num) {
  bl <- channel$bankline
  cl <- sx_centerline(channel)
  # sf::st_sample(cl, size = num, type = "regular")
  len <- sum(sf::st_length(cl))
  # https://stackoverflow.com/questions/74844804/finding-a-set-of-equally-spaced-perpendicular-lines-along-boundaries-in-r
  pts <- sf::st_line_sample(cl, density = num / len)
  # Get maximum distance from bbox
  bb <- sf::st_bbox(bl)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 +
      (bb[["ymax"]] - bb[["ymin"]])^2
  )
  # Generate parallel cross sections from each line segment.
  xy0 <- sf::st_centroid(cl)
  line0 <- (cl - xy0) / st_length(cl) * maxd
  rot_mat90 <- matrix(c(0, 1, -1, 0), nrow = 2, ncol = 2, byrow = FALSE)
  perp0 <- line0 * rot_mat90
  perp <- perp0 + xy0
  # Put the lines on all sample points. Also, re-aggregate the points so that
  # they aren't assigned to each cross line segment.
  n <- length(pts) # Also = length(perp)
  xs <- list()
  pts_new <- list()
  for (id in seq_len(n)) {
    pts_new[[id]] <- pts[[id]]
    m <- nrow(pts[[id]])
    l <- list()
    shifts <- pts[[id]] - st_centroid(pts[[id]])
    # perp_vec <- rep(perp[id], m) + shifts
    # xy0_ <- sf::st_centroid(pt_)
    for (i in seq_len(m)) {
      long_xs <- perp[[id]] + shifts[i, ]
      l[[i]] <- sf::st_intersection(long_xs, bl)
    }
    xs[[id]] <- l #sf::st_multilinestring(l)
  }
  xs <- sf::st_as_sfc(unlist(xs, recursive = FALSE))
  # pts_new <- sf::st_as_sf(sf::st_multipoint(do.call(rbind, pts_new)))
  lapply(xs, \(x) {
    width_ <- sf::st_length(x)
    gbem::cross_section(width = width_, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  })
}
