#' Get and Set Channel Width
#'
#' @param cross_section Cross section.
#' @returns Cross section width; single numeric.
#' @examples
#' cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.01)
#' ch_width(cs)
#' ch_width(cs) <- 10
#' cs
#' @rdname ch_width
#' @export
xt_width <- function(cross_section) UseMethod("xt_width")

#' @export
xt_width.sx <- function(cross_section) {
  xt_width(cross_section$geom)
}

#' @export
xt_width.sxc <- function(cross_section) {
  class(cross_section) <- class(cross_section)[-1] # sf weirdness
  sf::st_length(cross_section)
}

#' #' @param value Channel width in meters; single positive numeric.
#' #' @rdname ch_width
#' #' @export
#' `ch_width<-` <- function(cross_section, value) {
#'   cross_section$width <- value
#'   validate_cross_section(cross_section)
#'   cross_section
#' }
