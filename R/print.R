#' @exportS3Method base::print
print.sx <- function(x, ...) {
  n <- nrow(x)
  if (n == 1) {
    s <- ""
  } else {
    s <- "s"
  }
  ellipsis::check_dots_empty()
  cat(
    "Simple cross section collection with ", n, " cross section", s, "\n",
    sep = ""
  )
  NextMethod()
}

#' @exportS3Method base::print
print.sxc <- function(x, ...) {
  n <- length(x)
  if (n == 1) {
    s <- ""
  } else {
    s <- "s"
  }
  #ellipsis::check_dots_empty()
  cat("Geometry set for ", n, " cross section", s, "\n", sep = "")
  NextMethod()
}
