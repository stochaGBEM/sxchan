library(tidyverse)
library(sfnetworks)
d <- read_rds("~/Desktop/connection_matrix.rds")
x <- xt_generate_centerline(my_banks)

plot(x)
(n <- as_sfnetwork(x))
plot(n)
plot(n, n = Inf)
unclass(n)
sfnetworks:::vcount(n)
sfnetworks:::ecount(n)
st_precision(n)
sfnetworks:::describe_graph(sfnetworks:::as_tbl_graph(n))

n |>
  activate(edges) |>
  arrange(from) |>
  slice_head(n = 4)

p1 = st_point(c(7, 51))
p2 = st_point(c(7, 52))
p3 = st_point(c(8, 52))
p4 = st_point(c(8, 51.5))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p1, p4, p3)))
l3 = st_sfc(st_linestring(c(p3, p2)))

plot(c(l1, l2, l3))
plot(l2)
plot(l3)

edges = st_as_sf(c(l1, l2, l3), crs = 4326)
nodes = st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs = 4326)

plot(edges)
plot(nodes)

edges$from = c(1, 1, 3)
edges$to = c(2, 3, 2)

net = sfnetwork(nodes, edges)

plot(net)
