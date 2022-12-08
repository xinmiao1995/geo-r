library(sfnetworks)
library(sf)
library(osmdata)
# 1.札幌市中心部の道路網を取得する
# 市町村単位ではないので座標系でbboxを作成する
bb <- matrix(c(141.31852,141.38815,43.03581,43.08913),
             byrow = TRUE, 
             nrow = 2,
             ncol = 2, 
             dimnames = list(c('x','y'),c('min','max'))) 
# opq()でクエリをoverpass APIに投げて、sfに変換する
sf <- opq(bbox =bb,timeout='900') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()

# 2.道路網ポリラインをネットワークに合った形に整形
# 距離を計算するなので投影座標系に変換する
# sfnetworkを生成する
net <- as_sfnetwork(st_transform(sf$osm_lines, 3100), directed = FALSE)

# 30m以内にあるポイントを一つのクラスターにする
node_coords <- net %>%
  activate("nodes") %>%
  st_coordinates()
clusters <- dbscan(node_coords, eps = 30, minPts = 1)$cluster
clustered <- net %>%
  activate("nodes") %>%
  mutate(cls = clusters)
clustered <- clustered %>%
  mutate(cmp = group_components())
contracted <- convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

# 連通性の調整
net_simplified <- contracted %>% st_transform(4326) %>% convert(to_spatial_subdivision)
# edge長さの計算
net_simplified <- net_simplified %>% 
  activate("edges") %>% mutate(weight = edge_length())
# CRSを地理座標系に戻す
net_simplified <- net_simplified %>% 
  st_transform(4326) 

# osmdataで地名と一致するポイントを取得し、一番近隣のnodeを検索
p1 <- get_osm_pnt(bb,'時計台') %>% get_nearest_node(net_simplified)
p2 <- get_osm_pnt(bb,'東札幌駅')%>% get_nearest_node(net_simplified)


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

# 最短ルートを探す
shortest_path = st_network_paths(net_simplified, from =p1, to = p2, type = "shortest")
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
