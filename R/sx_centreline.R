#' Creates or obtains the centreline of the channel object
#'
#' Creates or obtains the centreline of the channel object. `sx_centerline()`
#' is an alias for `sx_centreline()`.
#'
#' @param channel channel object
#' @param centerline shapefile containing the centerline linestring geometries
#' If not supplied, the function will delineate a centerline based on the channel
#' object
#'
#' @rdname sx_centreline
#' @export
sx_centreline <- function(channel, centreline = NULL) {

  if (!is.null(centreline)) {
    centerline <- sf::read_sf(centreline)
  } else {
    edge_points <- channel$channel %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_line_sample(density = 0.02) %>%
      sf::st_union()

    vor_edges <- sf::st_voronoi(edge_points, bOnlyEdges = TRUE) %>%

      # from single MULTILINESTRING to a set of LINESTRINGs
      sf::st_cast("LINESTRING")

    edge_within <- vor_edges %>%
      # for convenience of st_filter we need to convert sfc to sf
      sf::st_sf() %>%
      sf::st_filter(channel$channel, .predicate = sf::st_within)

    centerline <- edge_within %>%
      sfnetworks::as_sfnetwork(directed = FALSE) %>%
      tidygraph::activate(nodes) %>%
      # get_diameter returns a list node tbl indices, so we can use it with slice
      dplyr::slice(igraph::get_diameter(.) %>% as.vector()) %>%
      tidygraph::convert(sfnetworks::to_spatial_smooth) %>%
      sf::st_as_sf(active = "edges")

  }

  channel[["centreline"]] <- centerline
  channel
}

#' @rdname sx_centreline
#' @export
sx_centerline <- sx_centreline
