suppressPackageStartupMessages({
  require(bslib)
  require(DT)
  require(easyalluvial)
  # require(ggplot2)
  require(leaflet)
  require(markdown)
  require(parcats)
  require(plotly)
  require(raster)
  require(RColorBrewer)
  require(rgdal)
  require(shiny)
  require(shinycssloaders)
  #require(shinythemes)
  #require(tmap)
  require(pals) # for donutplot
})

# Load the home-made functions
source("Tools.R")

# load the dataset
load("DataDashboard.Rdata")

# set the parameters
palph <- RColorBrewer::brewer.pal(4, "PuOr")
palroi <- RColorBrewer::brewer.pal(3, "RdYlGn")
pallime <- rev(RColorBrewer::brewer.pal(7, "RdYlGn")[-c(5,7)])