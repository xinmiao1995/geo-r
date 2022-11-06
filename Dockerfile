FROM rocker/geospatial
WORKDIR /usr/src/app

RUN Rscript -e "install.packages(c('leaflet','osmdata','sfnetworks','dbscan'),repos = 'https://cran.rstudio.com')"

ADD geo_r.R /home/rstudio