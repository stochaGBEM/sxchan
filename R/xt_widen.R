#' @export
xt_widen <- function(cross_section, by) UseMethod("xt_widen")

#' @export
xt_widen.sx <- function(cross_section, by) {
  wider <- xt_widen(cross_section$geom)
  cross_section$geom <- wider
  cross_section
}

#' @export
xt_widen.sxc <- function(cross_section, by) {
  n <- length(cross_section)
  by <- vctrs::vec_recycle(by, n)
  l <- xt_width(cross_section)
  mult <- (l + by) / l
  for (i in seq_len(n)) {
    cs <- cross_section[[i]]
    middle <- sf::st_centroid(cs)
    cross_section[[i]] <- (cs - middle) * mult[i] + middle
  }
  new_sxc(sf::st_sfc(cross_section, recompute_bbox = TRUE))
}
