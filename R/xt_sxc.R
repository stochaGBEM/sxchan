#' Create channel cross sections
#'
#' Create channel cross sections.
#' Cross section geometries (line segments) are created with `xt_sxc()`.
#' Cross section geometries can be paired with cross section characteristics
#' like roughness and gradient with `xt_sx()`.
#' @param x Object to create cross sections out of. Can be a vector of
#' (positive) widths if the spatial orientation is not important;
#' can be line segments created using the sf package.
#' @param ... Optional arguments passed to `sf::st_sfc()` relating to the
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
#' a <- xt_sxc(1:3)
#' xt_sx(1:3, swimability = c(4, 2, 1))
#' xt_sx(a, swimability = c(4, 2, 1))
#'
#' @returns For `xt_sx()`, cross section objects with class "sx",
#' which is a special type of "sf" object. For `st_sxc()`, cross section
#' geometry objects with class "sxc", which is a special type of "sfc"
#' object.
#' @rdname xt_sxc
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
  new_sxc(geom)
}

#' @export
xt_sxc.sfc <- function(x, ...) {
  geom <- sf::st_sfc(x, ...)
  new_sxc(geom)
}

#' @export
xt_sxc.sfg <- function(x, ...) {
  geom <- sf::st_sfc(x, ...)
  new_sxc(geom)
}

#' @export
xt_sxc.sxc <- function(x, ...) {
  geom <- sf::st_sfc(x, ...)
  new_sxc(geom)
}
