#' @exportS3Method base::plot
plot.sxc <- function(x, ...) {
  class(x) <- class(x)[-1] # sf's plot does not currently anticipate subclasses
  plot(x, ...)
}
