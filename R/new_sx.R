#' Constructor functions for cross section objects
#'
#' Stamps an object with either an "sxc" or "sx" class, on top of
#' the object's existing class(es).
#'
#' @param l Object to turn into a cross section.
#' @param ... Attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @returns An object of class `"sxc"`, although not necessarily with
#' valid properties. The naming "sxc" is inspired by the sf package,
#' which names geometries that have features "sfc".
#' @rdname new_sx
new_sxc <- function(l, ..., class = character()) {
  original_class <- class(l)
  structure(l, ..., class = c(class, "sxc", original_class))
}

#' @rdname new_sx
new_sx <- function(l, ..., class = character()) {
  original_class <- class(l)
  structure(l, ..., class = c(class, "sx", original_class))
}
