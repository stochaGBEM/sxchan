#' Creates or obtains the centerline of the channel object
#'
#' Creates or obtains the centerline of the channel object. `sx_centreline()`
#' is an alias for `sx_centerline()`.
#'
#' @param channel channel object
#' @rdname sx_centreline
#' @export
sx_xs <- function(channel) channel$cross_sections

#'
#' @param value Centerline to set in channel.
#' @rdname sx_centreline
#' @export
`sx_xs<-` <- function(channel, value) {
  channel$cross_sections <- value
  channel
}
