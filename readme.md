# 利用するライブラリー
## osmdata
- overpass APIを使ってOSMデータ(ベクター)をダウンロードできるライブラリー
- CRANからインストールできる
```{r setup, include=FALSE}
install.packages("osmdata")
library(osmdata)        
``` 
- クエリで地物データを取得する
  - bounding boxから始まるopq()メソッド
    - 地名文字列で指定:`q <- opq(bbox = 'greater london uk') `
    - 座標値で指定:`q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))`
  - キーとバリューを指定し、featureを追加していく
  `q <- opq(bbox = 'greater london uk') %>%
    add_osm_feature(key = 'highway', value = 'motorway')`
  - sf/spに変換する
  `x <- opq(bbox = 'greater london uk') %>%
    add_osm_feature(key = 'highway', value = 'motorway') %>%
    osmdata_sf ()`

- osmdataオブジェクト
  - A bounding box (which can be accessed with bridge$bbox)
  - A time-stamp of the query (bridge$timestamp, useful for checking data is up-to-date)
  - The spatial data, consisting of osm_points, osm_lines, osm_polygons, osm_multilines and osm_multipolygons.

## sfnetworks
- sfnetworksはネットワーク解析パッケージtidygraphと空間データパッケージsfを繋げている
- CRANからインストールできる
```{r setup, include=FALSE}
install.packages("sfnetworks")
library(sfnetworks)        
``` 
  - networkとラインデータの違い
    - network
      - nodeだけハンドリングすれば良い
    - ラインデータ
      - 座標・座標系を持つ
- sfnetworksの生成
  - From a nodes and edges table
  - From an sf object with linestring geometries
- Activation
- Extraction
- Visualization
- sfnetworksオブジェクトの特徴
  - Geometries
  - Coordinates
  - CRS
  - 属性

## Visulization
- leaflet


## RStudio環境を立ち上げる
- Docekrの起動
```sh
cd ./
docker-compose up --build -d 
```
- ローカルホストにアクセス
 
http://localhost:8787/
