install.packages(c("ggspatial","leaflet"))
library(sf)

library(dplyr)
library(ggplot2)
library(ggspatial)

# GeoJsonを読み込み
hokkaido <- st_read("hokkaido.geojson")
landprice <- st_read("landprice_r4.geojson")
# landprice_sapporo <- landprice %>% dplyr::filter(L01_022 == "01101")
landprice$L01_022

# 属性情報によるフィルター
sapporo <- hokkaido %>%
  dplyr::filter(N03_003 == "札幌市") %>%
    st_transform(4612)

# 空間選択
landprice_sapporo <- landprice[sapporo, ]
landprice_sapporo <- select(landprice_sapporo,"L01_006")

# 空間結合
joined <- st_join(landprice_sapporo, sapporo["N03_004"])
colnames(joined) <- c("landprice", "ward", "geometry")
joined$landprice

# 空間解析

# 描画
ggplot2::ggplot() +
    ggplot2::geom_sf(data = sapporo, fill = NA, aes(fill = ward)) +
    ggplot2::geom_sf(data = joined %>% filter(ward == "中央区"), col = "#b9799a") +
    ggplot2::geom_sf(data = joined %>% filter(ward == "北区"), col = "#54d1dc") +
    ggplot2::geom_sf(data = joined %>% filter(landprice > 150000), col = "#e3092a") +
    ggspatial::annotation_scale() +
    ggspatial::annotation_north_arrow(location = "tr")

# leaflet
library(leaflet)

pal <- colorQuantile(palette = "Reds", domain = joined$landprice)
pal <- colorNumeric(palette = "Blues", domain = joined$landprice)

leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addTiles(urlTemplate = "https://tile.mierune.co.jp/mierune/{z}/{x}/{y}.png",
           group = "MIERUNE Color") %>%
    # https://www.google.com/maps/d/viewer?mid=z1tD77I5wNwc.kjf5OMKDCpLY&t=p&ll=43.055906%2C141.284278&z=17
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolygons(data = sapporo, fillColor = "grey", stroke = FALSE, label = sapporo$N03_004, group = "Sapporo City") %>%
    addMarkers(data = joined, label=joined$landprice,
                         clusterOptions = markerClusterOptions(markerOptions(clickable = T)),group = "Landprice")%>%
    # addCircles(data = joined, color = ~ pal(landprice), fillOpacity = 0.2, label = ~landprice, radius = ~ sqrt(landprice - 10000), group = "Landprice") %>%
    # addLegend("bottomright",
    #     pal = pal, values = joined$landprice, title = "標準地の地価「円/m2」", labFormat = labelFormat(prefix = "$"),
    #     opacity = 1
    # ) %>%
    addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite","MIERUNE Color"),
        overlayGroups = c("Sapporo City", "Landprice"),
        options = layersControlOptions(collapsed = FALSE)
    )
# # addCircles(data=joined %>% filter(ward == "中央区"), col = "#b9799a") %>%
# addCircles(data = joined, color = ~ pal(landprice),fillOpacity = 0.2) %>%

# providers
# https://leaflet-extras.github.io/leaflet-providers/preview/


# # tmap
# install.packages("tmap")
# library(tmap)
# tmap_mode("view")
# tm_shape(hokkaido) + tm_fill("N03_004")
# # Number of levels of the variable "N03_004" is 193, which is larger than max.categories (which is 30), so levels are combined. Set tmap_options(max.categories = 193) in the layer function to show all levels.

# # tips
# class(hokkaido)
# typeof(hokkaido)
# attributes(hokkaido)
# typeof(hokkaido$geometry)
# colnames(hokkaido)
# hokkaido$N03_004
# print(hokkaido, n = 3)
# sf::st_bbox(hokkaido)
# methods(class = "sf")
# methods(class = "list")


# raster
library(raster)
landuse <- raster("sapporo_landuse.tif")
youteizan <- raster("youteizan.tif")

# ラスタ読み込み
sapporo_dem <- raster("sapporo_dem.tif")
plot(sapporo_dem)
slotNames(sapporo_dem)
sapporo_dem@crs
ncell(sapporo_dem)

# POI情報取得
poi <- 
  st_point(c(141.3374223,43.079626)) %>% 
  st_sfc(crs = 4612)
poi <-st_read("P29-13_01.shp") 
poi <- poi[sapporo,] 
poi %>% select(P29_005)
poi <- select(poi,P29_005,P29_006,geometry)


dem_extract <- extract(sapporo_dem, 
        as(poi, "Spatial"),
        fun = mean)





