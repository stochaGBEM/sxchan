#' Simple feature channel object containing a centerline, a cross section,
#' a gradient and banks
#'
#' @param channel Shapefile file containing channel geometries
#' @export
sx_channel <- function(channel, class = character()) {

  structure(channel, class = c(class, "list", "channel"))

  ch <- list()
  ch[["channel"]] <- sf::read_sf(channel)

  ch
}
