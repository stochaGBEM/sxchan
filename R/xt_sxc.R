#' @rdname xt_sx
#' @export
xt_sxc <- function(x, ...) UseMethod("xt_sxc")

#' @export
xt_sxc.default <- function(x, ...) {
  if (any(x <= 0)) stop("Must have a positive width.")
  ## Construct an sfc object with linestring geometry of specified widths.
  segs <- list()
  for (i in seq_along(x)) {
    segs[[i]] <- sf::st_linestring(matrix(c(0, x[i], i, i), ncol = 2))
  }
  geom <- sf::st_sfc(segs, ...)
  ## Add features
  # obj <- rlang::exec("sf::st_sf", geom = geom, !!!features)

  # res <- new_cross_section(l)
  new_sxc(geom)
  #validate_sxc(geom)
}

#' @export
xt_sxc.sfc <- function(x, ...) {
  new_sxc(sf::st_sfc(x, ...))
}

#' #' @export
#' xt_sxc.sf <- function(width, ..., grad, d50, roughness, rootdepth = 0) {
#'   # Only take the geometry column
#'   geometry_col <- attr(width, "sf_column")
#'   width_sfc <- width[[geometry_col]]
#'   cross_section(width_sfc, graad, d50, roughness, rootdepth)
#' }

#' #' @export
#' xt_sxc.sfg <- function(width, ..., grad, d50, roughness, rootdepth = 0) {
#'
#' }

#' #' Cross Section Features
#' #'
#' #' Makes a list of cross section features. Useful because it
#' #' indicates features that are useful for algorithms like gbem;
#' #' otherwise, there is no difference from a list.
#' xt_features <- function(
#'     ...,
#'     grad = NULL,
#'     d50 = NULL,
#'     d84 = NULL,
#'     roughness = NULL,
#'     rootdepth = NULL
#' ) {
#'   dots <- list2(
#'     ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
#'     rootdepth = rootdepth
#'   )
#'   nulls <- vapply(dots, is.null, logical(1))
#'   dots[!nulls]
#' }
#'
#' xt_add_features <- function(
#'     xs, ..., grad = NULL, d50 = NULL, d84 = NULL, roughness = NULL,
#'     rootdepth = NULL
#'   ) {
#'   f <- sx_features(
#'     ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
#'     rootdepth = rootdepth
#'   )
#'   rlang::exec("sf::st_sf", geom = xs, !!!f)
#' }
