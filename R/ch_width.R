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
ch_width <- function(cross_section) {
  sf::st_length(cross_section$width)
}

#' @param value Channel width in meters; single positive numeric.
#' @rdname ch_width
#' @export
`ch_width<-` <- function(cross_section, value) {
  cross_section$width <- value
  validate_cross_section(cross_section)
  cross_section
}
