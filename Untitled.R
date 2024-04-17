library(dplyr)
library(sf)

ch <- channel("../gbem/data/Oct2022_Banklines.shp")
ch <- channel("../catchment_descriptor_tools/data/07/07AA001/07AA001_DrainageBasin_BassinDeDrainage.shp")
ch <- ch %>%
  sx_centerline("../gbem/data/Oct2022_Coldwater_Centreline.shp")


sf::read_sf("../gbem/data/Oct2022_Coldwater_Centreline.shp")

 ch %>%
   st_triangulate(bOnlyEdges = TRUE) %>%
   st_cast("LINESTRING") %>%
   st_centroid() %>%
   st_intersection(., ch) %>%
   select(geometry) %>%
   write_sf("centerlines.shp")



centerline <- ch %>%
  st_segmentize(dfMaxLength = 10) %>%
  st_cast("POINT", do_split = TRUE) %>%
  #Use buffer to find overlap
  st_buffer(2.5) %>%
  #Merge overlapping polygons
  st_union() %>%
  st_cast("POLYGON") %>%
  #Convert to centroids
  st_centroid()


dxy1 <- deldir(st_coordinates(centerline)[, 'X'],
               st_coordinates(centerline)[, 'Y'])


tr <- interp::tri.mesh(st_coordinates(centerline)[, 'X'],
                       st_coordinates(centerline)[, 'Y'])
ctr <- data_frame(x = tr$x, y = tr$y)
st_crs(ctr) <- st_crs(ch)

mids=cbind((ctr$x + d$delsgs[,'x2'])/2,
           (d$delsgs[,'y1']+d$delsgs[,'y2'])/2)

st_as_sf(ctr, coords = c("x", "y"))

ctr %>%
  st_cast("LINESTRING")

write_sf(ctr, "triangulation.shp")


points <- data_frame(x = tr$x, y = tr$y) %>%
  select(x, y) %>%
  as.matrix() %>%
  st_multipoint(dim = "xy") %>%
  st_geometry() %>%
  st_cast("POINT")

# Build linestrings from Delaunay triangle object
net <- lapply(X = 1:(length(points) - 1), FUN = function(x) {
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING") %>% st_sf(.)
  return(line)
})

# Combine line strings
net <- do.call("rbind", net)
st_crs(net) <- st_crs(ch)

# Select segments that don't interest with banks
banks <- st_buffer(ch, 10000)
net <- net %>% filter(st_contains(banks, sparse = F))


plot(ch)

ch %>%
  sf::st_voronoi() %>%
  st_intersection(., ch) %>%
  plot()


v <- ch %>%
  sf::st_voronoi()

library(DirichletReg)

p <- ch %>%
  st_segmentize(dfMaxLength = 10)
  # st_cast("MULTIPOINT") %>%
  # st_voronoi() %>%
  # st_collection_extract()
# %>%
#   st_cast("LINESTRING") %>%
#   plot()

st_collection_extract(v)
dirichlet(ch$geometry)

write_sf(net, "centerline.shp")
