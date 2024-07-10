#' Cross Section Features
#'
#' Makes a list of cross section features. Useful because it
#' indicates features that are useful for algorithms like gbem;
#' otherwise, there is no difference from a list.
xt_features <- function(
    ...,
    grad = NULL,
    d50 = NULL,
    d84 = NULL,
    roughness = NULL,
    rootdepth = NULL
) {
  dots <- list2(
    ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
    rootdepth = rootdepth
  )
  nulls <- vapply(dots, is.null, logical(1))
  dots[!nulls]
}

xt_add_features <- function(
    xs, ..., grad = NULL, d50 = NULL, d84 = NULL, roughness = NULL,
    rootdepth = NULL
) {
  f <- sx_features(
    ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
    rootdepth = rootdepth
  )
  rlang::exec("sf::st_sf", geom = xs, !!!f)
}
