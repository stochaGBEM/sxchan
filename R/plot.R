#' @export
plot.sx <- function(x, ...) {
  class(x) <- class(x)[-1] # sf weirdness
  plot(x, ...)
}

#' @export
plot.sxc <- function(x, ...) {
  class(x) <- class(x)[-1] # sf weirdness
  plot(x, ...)
}
