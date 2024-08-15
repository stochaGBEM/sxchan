#' Creates or obtains the centerline of the channel object
#'
#' Creates or obtains the centerline of the channel object. `sx_centreline()`
#' is an alias for `sx_centerline()`.
#'
#' @param channel channel object
#' @rdname sx_centreline
#' @export
sx_centerline <- function(channel) {
  if (!is.null(channel$centerline)) {
    channel$centerline
  } else {
    xt_generate_centerline(channel$bankline)
  }
}

#'
#' @param value Centerline to set in channel.
#' @rdname sx_centreline
#' @export
`sx_centerline<-` <- function(channel, value) {
  channel$centerline <- value
  channel
}

#' @rdname sx_centreline
#' @export
sx_centreline <- sx_centerline

#' @rdname sx_centreline
#' @export
`sx_centreline<-` <- `sx_centerline<-`
