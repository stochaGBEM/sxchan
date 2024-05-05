#' Generate cross sections from a channel
#'
#' @param channel A channel object.
#' @param num Number of cross sections to take from the channel.
#' @returns A list of cross sections.
sx_xs <- function(channel, num) {
  cl <- sx_centerline(channel)
  sf::st_sample(cl, size = num, type = "regular")
  # Generate parallel cross sections from each point.
  # ...
  # Build cross section objects for each.
  # ...
}
