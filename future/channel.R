#' Constructor function for channel objects
#'
#' Not intended for front end use; to make a channel object, use `channel()`.
#'
#' @param l Object to stamp with the "channel" class.
#' @param ... Attributes to add to the object.
#' @param class Subclass; optional.
#' @return The object `l` with the attributes in `...` and the class `"channel"`
#' with subclass `class` (if provided).
#' @export
new_channel <- function(l, ..., class = character()) {
  structure(l, ..., class = c(class, "channel"))
}

#' Make a new channel
#'
#' @param bankline Bankline `sf` polygon.
#' @param centerline An optional `sf` centerline object. `NULL` if not provided.
#' @return A new channel object.
#' @examples
#' channel(my_banks)
#' @export
channel <- function(bankline, centerline = NULL, cross_sections = NULL) {
  river_channel <- list(bankline = bankline, centerline = centerline)
  new_channel(river_channel)
}
