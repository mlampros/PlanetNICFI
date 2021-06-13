FROM rocker/geospatial:latest

LABEL maintainer='Lampros Mouselimis'

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \
 apt-get install -y aria2 && \
 R -e "install.packages('devtools', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \
 R -e "install.packages(c( 'httr', 'sf', 'data.table', 'glue', 'gdalUtils', 'rmarkdown', 'knitr', 'raster', 'sp', 'RStoolbox', 'proj4', 'testthat', 'remotes' ), repos =  'https://cloud.r-project.org/' )"

RUN R -e "remotes::install_github('mlampros/PlanetNICFI', upgrade = 'always', dependencies = TRUE, ref = '6011fd7de44e7c23f17f5f48b1db189c9e1a0ce2', repos = 'https://cloud.r-project.org/')" && \
 apt-get autoremove -y && \
 apt-get clean

ENV USER rstudio
