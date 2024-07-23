#' Create channel cross sections
#'
#' Create channel cross sections. Cross sections are created
#' with `xt_sx()`, where cross section characteristics like roughness and
#' gradient can be specified. Cross section geometries (line segments)
#' are created with `xt_sxc()`.
#' @param x Object to create cross sections out of. Can be a vector of
#' (positive) widths if the spatial orientation is not important;
#' can be line segments created using the sf package.
#' @param ... For `xt_sx()`, optional named entries of cross section
#' characteristics other than the ones listed as arguments;
#' lengths are recycled to match the number of cross sections.
#' For `xt_sxc()`, optional arguments passed to `sf::st_sfc()` relating to the
#' geometrical properties of the cross sections, such as `crs`.
#' @param grad Energy gradient of the stream channel; single positive numeric.
#' @param d50,d84 Grain size distribution's 50th and 84th quantiles in
#' millimeters; single positive numerics.
#' @param roughness Manning's roughness; positive single numeric.
#' @param rootdepth Effective rooting depth for vegetation; single non-negative
#' numeric.
#' @details
#' Examples of effective rooting depth for vegetation, `rootdepth`, are:
#'
#' - grassy banks, no trees / shrubs: `rootdepth = 0.35`.
#' - 1 to 5% tree / shrub cover: `rootdepth = 0.50`.
#' - 5 to 50% tree / shrub cover: `rootdepth = 0.90`.
#' - more than 50% tree / shrub cover: `rootdepth = 1.10`.
#'
#' The nomenclature of these objects is inspired by the sf package.
#'
#' - "sxc" stands for "spatial cross-section column", and like sf's "sfc"
#'    objects, contains the cross section geometries.
#' - "sx" stands for "spatial cross-section", and like sf's "sf" objects,
#'    is a data frame where one of the columns is an "sxc" object, and
#'    other columns are features of the cross section (roughness, d50, etc.)
#' @examples
#' xt_sx(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
#' @returns For `xt_sx()`, cross section objects with class "sx",
#' which is a special type of "sf" object. For `st_sxc()`, cross section
#' geometry objects with class "sxc", which is a special type of "sfc"
#' object.
#' @rdname xt_sx
#' @export
xt_sx <- function(x, ...) {
  new_sx(sf::st_sf(x, ...))
}

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
