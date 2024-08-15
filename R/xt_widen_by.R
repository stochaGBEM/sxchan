#' Widen cross sections
#'
#' Widen or narrow a collection of cross sections. `xt_widen_by()` widens
#' (narrows) by unit length; `xt_widen_times()` widens (narrows) by
#' a multiplicative factor.
#'
#' @param cross_section A cross section object.
#' @param by Amount to widen the channel by, using units in common with the
#' cross sectional units. Negative values will narrow cross sections. Either
#' a vector of length equal to the number of cross sections, or length 1.
#' @rdname xt_widen
#' @export
xt_widen_by <- function(cross_section, by) UseMethod("xt_widen_by")

#' @export
xt_widen_by.sx <- function(cross_section, by) {
  wider <- xt_widen_by(cross_section$geom, by = by)
  cross_section$geom <- wider
  cross_section
}

#' @export
xt_widen_by.sxc <- function(cross_section, by) {
  n <- length(cross_section)
  by <- vctrs::vec_recycle(by, n)
  l <- xt_width(cross_section)
  times <- (l + by) / l
  xt_widen_times(cross_section, times = times)
}
