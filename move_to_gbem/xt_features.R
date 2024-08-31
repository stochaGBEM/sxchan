#' Add sxchan features
#'
#' Add features to sxchan objects. This is essentially a wrapper around
#' `sf::st_sf()`, but is preferred because it returns an object understood
#' by sxchan. Some suggestions
#' for meaningful watercourse features are also provided.
#'
#' @param object A watercourse object, like a bankline,
#' centerline, or cross sections.
xt_add_features <- function(object, ...) UseMethod("xt_add_features")

xt_add_features.sx <- function(
    object, ..., grad = NULL, d50 = NULL, d84 = NULL, roughness = NULL,
    rootdepth = NULL
) {
  dots <- rlang::list2(
    ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
    rootdepth = rootdepth
  )
  res <- rlang::exec(sf::st_sf, geom = object, !!!dots)
  new_sx()
}

#xt_features <- function(
#    ...,
#    grad = NULL,
#    d50 = NULL,
#    d84 = NULL,
#    roughness = NULL,
#    rootdepth = NULL
#) {
#  dots <- list2(
#    ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
#    rootdepth = rootdepth
#  )
#  nulls <- vapply(dots, is.null, logical(1))
#  dots[!nulls]
#}
