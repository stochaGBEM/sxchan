#' Validator function for cross_section objects
#'
#' Checks that an object of class `"cross_section"` has a valid
#' structure and valid cross section properties.
#'
#' @param cross_section Object of class `"cross_section"`.
#' @returns Returns the original `cross_section` object. Note that this function is
#' intended to be run for its side effects: namely, throwing an error if the
#' cross_section is invalid.
validate_sxc <- function(x) {
  lengths <- vapply(x, length, FUN.VALUE = integer(1L))[-1]
  if (any(lengths != 1)) {
    stop("Cross Section properties must be single numerics. The following are not:",
         paste(names(x)[lengths != 1], collapse = ", "), ".")
  }
  if (x$grad <= 0) {
    stop("Cross Section must have a postive energy gradient (`grad`).")
  }
  if (x$d84 < x$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (x$d84 < x$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (x$d50 <= 0) {
    stop("Invalid grain size distribution: grain size must be positive.")
  }
  if (x$roughness <= 0) {
    stop("Manning's roughness must be positive.")
  }
  if (x$rootdepth < 0) {
    stop("Effective rooting depth for vegetation (`rootdepth`) cannot be ",
         "negative.")
  }
  x
}
