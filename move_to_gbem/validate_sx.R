#' Validator function for cross_section objects
#'
#' Checks that an object of class `"cross_section"` has a valid
#' structure and valid cross section properties.
#'
#' @param cross_section Object of class `"sx"`.
#' @returns Returns the original `cross_section` object. Note that this function is
#' intended to be run for its side effects: namely, throwing an error if the
#' cross_section is invalid.
validate_sx <- function(x) {
  if (!inherits(x, "sx")) {
    stop("Object does not inherit class 'sx'.")
  }
  if (!is.null(x$grad) && x$grad <= 0) {
    stop("Cross Section must have a postive energy gradient (`grad`).")
  }
  if (!is.null(x$d84 * x$d50) && x$d84 < x$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (!is.null(x$d84 * x$d50) && x$d84 < x$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (!is.null(x$d50) && x$d50 <= 0) {
    stop("Invalid grain size distribution: grain size must be positive.")
  }
  if (!is.null(x$grad * x$roughness) && !is.null(x$grad) && x$roughness <= 0) {
    stop("Manning's roughness must be positive.")
  }
  if (!is.null(x$grad * x$rootdepth) && !is.null(x$grad) && x$rootdepth < 0) {
    stop(
      "Effective rooting depth for vegetation (`rootdepth`) cannot be ",
      "negative."
    )
  }
  invisible(x)
}
