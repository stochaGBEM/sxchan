#' Get a new channel bankline from cross sections.
channelize <- function(cross_sections) UseMethod("channelize")

#' @export
channelize.sx <- function(cross_sections) {
  channelize(cross_sections$geom)
}

#' @export
channelize.sxc <- function(cross_sections) {
    ## Concatenate the cross sections into a multilinesegment
    cross_sections <- st_union(cross_sections)
    ## Turn the sfc column into a collection of points, and form a polygon from them
    ch <- st_polygonize(st_union(cross_sections))
    b <- st_cast(bar, "POINT")
    n <- length(b)
    n2 <- n / 2
    l <- b[1:n2 * 2]
    r <- b[1:n2 * 2 - 1]

}
