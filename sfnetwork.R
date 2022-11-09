# 1.札幌市中心部の道路網を取得する
# 市町村単位ではないので座標系でbboxを作成する
bb <- matrix(c(141.31852,141.38815,43.03581,43.08913), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
# opq()でクエリをoverpass APIに投げて、sfに変換する
x <- opq(bbox =bb,timeout='900') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()
edges <- x$osm_lines

# 取得したデータを確認
leaflet() %>% 
  leaflet::addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png")  %>% 
  leaflet::addPolylines(
    data = x$osm_lines,
    color ="#165E83",
    weight = "2",
    opacity = 0.5) 

# 2.道路網ポリラインをネットワークに合った形に整形
# 距離を計算するなので投影座標系に変換する
# sfnetworkを生成してみる
net <- as_sfnetwork(st_transform(edges, 3100), directed = FALSE)

# net <- convert(net, to_spatial_subdivision)
# 30m以内にあるポイントを一つのクラスターにする
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

# 新しいネットワークを生成する
net_simplified <- contracted %>% st_transform(4326) %>% convert(to_spatial_subdivision)

net_simplified = net_simplified %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())

# 地名でルーティングする
# osmdataで地名と一致するポイントを取得する
p1 <- get_osm_pnt(bb,'時計台')
p2 <- get_osm_pnt(bb,'東札幌駅')

# 一番近隣のnodeを検索
p1_nearest <- get_nearest_node(p1,net_simplified)
p2_nearest <- get_nearest_node(p2,net_simplified)

# 最短ルートを探す
shortest_path = st_network_paths(net_simplified, from =p1_nearest, to = p2_nearest, type = "shortest")
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
    opacity = 1)


get_osm_pnt <- function(bb,x){
  p <- opq(bbox = bb,timeout='900') %>%
    add_osm_feature(key = "name", value = x, value_exact = FALSE) %>%
    osmdata_sf ()
  return(p$osm_points[1,])
}

get_nearest_node <- function(poi,n){
  nodes <- st_as_sf(n, "nodes")
  return(st_nearest_feature(poi,nodes))
}

# 任意ポイントから一定距離の範囲を探す
nodes = activate(net_simplified, "nodes")
p <-st_as_sf(net_simplified, "nodes")[get_osm_pnt(bb,'時計台')%>%get_nearest_node(net_simplified),]
iso = nodes %>%
  filter(node_distance_from(st_nearest_feature(p, nodes), weights = weight) <= 1000)

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
  addMarkers(data = p)

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