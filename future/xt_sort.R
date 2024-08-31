#' Sort cross sections from upstream to downstream.
#' 
#' @param sxc sxc object, not sx
#' @returns The same cross section object, but sorted from
#' upstream to downstream.
#' @export
xt_sort <- function(sxc) {

}

#' Calculate the distance between two individual cross sections
#' @param xs1,xs2 Individual cross sections (line segments)
#' @param centerline A line segment representing the centerline of the channel.
#' @returns The distance between the two cross sections as traversed along
#' the channel centerline.
xt_distance <- function(xs1, xs2, centerline) {
  # Truncate the centerline to the extent of the cross sections
  ab <- st_intersection(centerline, st_union(xs1, xs2))
  # Now cut off the centerline at the intersection points, taking
  # only the middle segment
    ab <- st_intersection(ab, xs1)
  # Calculate the distance of the truncated cross section
  st_length(ab)
}