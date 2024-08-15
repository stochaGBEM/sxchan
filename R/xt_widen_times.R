#' @rdname xt_widen
#' @param times Multiplicative factor to widen the channel by. Values less than
#' 1 will narrow cross sections. Either
#' a vector of length equal to the number of cross sections, or length 1.
#' @export
xt_widen_times <- function(cross_section, times) UseMethod("xt_widen_times")

#' @export
xt_widen_times.sx <- function(cross_section, times) {
  wider <- xt_widen_times(cross_section$geom, times = times)
  cross_section$geom <- wider
  cross_section
}

#' @export
xt_widen_times.sxc <- function(cross_section, times) {
  n <- length(cross_section)
  times <- vctrs::vec_recycle(times, n)
  for (i in seq_len(n)) {
    xs <- cross_section[[i]]
    middle <- sf::st_centroid(xs)
    cross_section[[i]] <- (xs - middle) * times[i] + middle
  }
  new_sxc(sf::st_sfc(cross_section, recompute_bbox = TRUE))
}
