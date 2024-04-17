#' Creates or obtains the centreline of the channel object
#'
#' @param channel channel object
#' @param centerline shapefile containing the centerline linestring geometries
#' If not supplied, the function will delineate a centerline based on the channel
#' object
#'
#' @export
sx_centreline <- function(channel, centerline = NULL) {
  if (!is.null(centerline)) {
    centerline <- sf::read_sf(centerline)
  # } else {

  }

  channel[["centerline"]] <- centerline
  channel
}
