#' Function to calculate a centerline from bankline polygon
#'
#' @param sf_banks `sf`, `sfc`, or `SpatVector` polygons object.
#' @returns A centerline.
#' @details Currently just calls `centerline::cnt_path_guess()`.
bankline_to_centerline <- function(sf_banks) {
  centerline::cnt_path_guess(sf_banks)
}
