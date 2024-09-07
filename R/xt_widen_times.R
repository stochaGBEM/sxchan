#' @rdname xt_widen
#' @param times Multiplicative factor to widen the channel by. Values less than
#' 1 will narrow cross sections. Either
#' a vector of length equal to the number of cross sections, or length 1.
#' @export
xt_widen_times <- function(object, times) UseMethod("xt_widen_times")

#' @export
xt_widen_times.sf <- function(object, times) {
  xs <- sf::st_geometry(object)
  if (!is_sxc(xs)) {
    stop(
      "The geometry column in the inputted sf object is not a cross section ",
      "object set (class 'sxc')."
    )
  }
  wider <- xt_widen_times(xs, times = times)
  sf::st_geometry(object) <- wider
  object
}

#' @export
xt_widen_times.sxc <- function(object, times) {
  n <- length(object)
  times <- vctrs::vec_recycle(times, n)
  for (i in seq_len(n)) {
    xs <- object[[i]]
    middle <- sf::st_centroid(xs)
    object[[i]] <- (xs - middle) * times[i] + middle
  }
  new_sxc(sf::st_sfc(object, recompute_bbox = TRUE))
}
