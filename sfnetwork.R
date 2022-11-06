bb <- matrix(c(141.31852,141.38815,43.03581,43.08913), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
x <- opq(bbox =bb,timeout='900') %>%
add_osm_feature(key = 'highway') %>%
osmdata_sf ()
leaflet() %>% 
  leaflet::addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png")  %>% 
  leaflet::addPolylines(
    data = x$osm_lines,
    color ="#165E83",
    weight = "2",
    opacity = 0.5) 

x_line_3100 <- st_transform(x$osm_lines, 3100)
net <- as_sfnetwork(x_line_3100, directed = FALSE,length_as_weight = TRUE)

net = convert(net, to_spatial_subdivision)

node_coords = net %>%
  activate("nodes") %>%
  st_coordinates()
clusters = dbscan(node_coords, eps = 30, minPts = 1)$cluster
clustered = net %>%
  activate("nodes") %>%
  mutate(cls = clusters)
clustered = clustered %>%
  mutate(cmp = group_components())
contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

net_simplified = as_sfnetwork(st_as_sf(contracted, "edges") %>% st_transform(4326), directed = FALSE)
net_simplified <- net_simplified %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())

p1 <- get_osm_pnt(bb,'円山公園')
p2 <- get_osm_pnt(bb,'中島公園駅')

p1_nearest <- get_nearest_node(p1,net_simplified)
p2_nearest <- get_nearest_node(p2,net_simplified)

shortest_path = st_network_paths(net_simplified, from = p1, to = p2)
node_path <- shortest_path %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

path_sf <- net_simplified %>% 
  activate(nodes) %>% 
  slice(node_path) %>% 
  st_as_sf("edges")


leaflet() %>% 
  leaflet::addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png")  %>% 
  leaflet::addPolylines(
    data = st_as_sf(net_simplified, "edges") %>% st_transform(4326),
    color ="#165E83",
    weight = "4",
    opacity = 0.5) %>%
  leaflet::addPolylines(
    data = path_sf,
    color = "	#B13546",
    weight="7",
    opacity = 1)%>%
  addMarkers(data = p1)%>%
  addMarkers(data = p2)

get_osm_pnt <- function(bb,x){
  p <- opq(bbox = bb,timeout='900') %>%
    add_osm_feature(key = "name", value = x, value_exact = FALSE) %>%
    osmdata_sf ()
  return(p$osm_points[1,])
}

get_nearest_node <- function(poi,n){
  nodes <- st_as_sf(n, "nodes") %>% st_transform(4326)
  return(nodes[st_nearest_feature(poi %>% st_transform(4326),net %>% st_transform(4326)),])
}

# 任意ポイントから一定距離の範囲を探す
nodes = activate(net_simplified, "nodes")%>% st_transform(4326)
p_centroid = net %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()%>% st_transform(4326)
iso = nodes %>%
  filter(node_distance_from(st_nearest_feature(p_centroid, nodes), weights = weight) <= 2000)

iso_poly = iso %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

leaflet() %>% 
  leaflet::addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png")  %>% 
  leaflet::addPolylines(
    data = st_as_sf(net_simplified, "edges") %>% st_transform(4326),
    color ="#165E83",
    weight = "4",
    opacity = 0.5) %>%
  leaflet::addPolygons(
    data = iso_poly,
    color = "	#B13546",
    weight="7",
    opacity = 1)%>%
  addMarkers(data = p_centroid)

# draw cut nodes and edges
library(igraph)
library(tidygraph)
library(tidyverse)
new_net = net_simplified %>%
  activate("nodes") %>%
  mutate(is_cut = node_is_cut()) %>%
  morph(tidygraph::to_linegraph) %>%
  mutate(is_cut = node_is_cut()) %>%
  unmorph()
cut_nodes = new_net %>%
  activate("nodes") %>%
  filter(is_cut) %>%
  st_geometry()

cut_edges = new_net %>%
  activate("edges") %>%
  filter(is_cut) %>%
  st_geometry()

leaflet() %>% 
  leaflet::addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png")  %>% 
  leaflet::addPolylines(
    data = st_as_sf(net_simplified, "edges") %>% st_transform(4326),
    color ="#165E83",
    weight = "4",
    opacity = 0.5) %>%
  leaflet::addPolylines(
    data = cut_edges,
    color = "	#B13546",
    weight="7",
    opacity = 1)

# to_spatial_contracted
new_net = net_simplified %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>%
  mutate(foo = sample(c(1:10), graph_order(), replace = TRUE)) %>%
  mutate(bar = sample(c(TRUE, FALSE), graph_order(), replace = TRUE)) %>%
  mutate(louvain = as.factor(group_louvain()))
contracted_net = convert(
  new_net,
  to_spatial_contracted,
  louvain,
  simplify = TRUE,
  summarise_attributes = list(
    foo = "sum",
    bar = function(x) any(x),
    louvain = "first"
  )
)
plot(st_geometry(new_net, "edges"), main = "Grouped nodes")
plot(st_as_sf(new_net)["louvain"], key.pos = NULL, pch = 20, add = TRUE)
plot(st_geometry(contracted_net, "edges"), main = "Contracted network")
plot(
  st_as_sf(contracted_net)["louvain"],
  cex = 2, key.pos = NULL,
  pch = 20, add = TRUE
)
