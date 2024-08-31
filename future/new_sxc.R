#' @rdname new_sx
new_sxc <- function(l, ..., class = character()) {
  original_class <- class(l)
  structure(l, ..., class = c(class, "sxc", original_class))
}
