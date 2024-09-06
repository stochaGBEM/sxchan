#' Remove cross section class for use by sf
#'
#' The central dogma of sxchan is to work with the subset of sf geometries
#' that could represent channel cross sections. Programmatically, this is
#' done by defining a _subclass_. Unfortunately, the sf package does not always
#' anticipate subclasses, so that usual sf operations might not work on
#' cross section objects, despite still being sf objects.
#' `xt_unclass_for_sf()` strips away the subclass identity so that you can
#' use those sf functions. You might have to re-stamp the object identifying
#' it as a cross section object afterwards, probably with `xt_sxc()`, or
#' possibly with the lighter-weight `new_sxc()` function.
#' @param x An object inheriting class "sfg", "sfc", or "sf". In other words,
#' an sf object.
#' @returns If the input is NOT a cross section (sxc) object, the input is
#' returned unmodified. Otherwise, the class is shortened to contain only
#' those classes coming after "sxc".
#' @examples
#' library(sf)
#' a <- xt_sxc(2.5)
#' # sf gets confused since we've labelled `a` as a cross section:
#' try(st_sf(a, roughness = 0.1))
#' # unclass first:
#' a2 <- xt_unclass_for_sf(a)
#' a3 <- st_sf(a2, roughness = 0.1)
#' #
#'
xt_unclass_for_sf <- function(x) {
  is_sfg <- inherits(x, "sfg")
  is_sfc <- inherits(x, "sfc")
  is_sf <- inherits(x, "sf")
  if (!is_sfg && !is_sfc && !is_sf) {
    stop(
      "Inputted object does not inherit class 'sfg', 'sfc', or 'sf'."
    )
  }
  if (is_sxc(x)) {
    cl <- class(x)
    i <- which(cl == "sxc")
    n <- length(cl)
    class(x) <- cl[(i + 1):n]
    return(x)
  } else {
    return(x)
  }
}
