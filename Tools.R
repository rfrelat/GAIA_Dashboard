## Functions to create the outputs of the GAIA Dashboard
# Author: R. Frelat
# Last modification: 23 October 2022

## TAB 2 --------------------------------------------------
#' Map pH per country
#'
#' This function plot the pH with 4 different classes
#' using the Leaflet package
#' @param layer raster with pH data
#' @param breaks definition of the pH classes, by default 4 classes cut at 0, 5.5, 6, 6.5, and 14.
#' @param pal color palette for the different pH classes
#' @param opacity opacity of the pH layer over the openstreetmap background (default: 0.7)
#' @param title title of the legend, by default 'pH'
#'
map_ph <- function(layer, 
                  breaks=c(0,5.5,6,6.5,14), 
                  pal=RColorBrewer::brewer.pal(4, "PuOr"), 
                  opacity=0.7, title="pH"){
  # color palette
  pal_z = colorBin(palette = pal, domain = values(layer), 
                   bins = breaks,
                   na.color = "transparent")
  
  # create the plot
  p1 <- leaflet() %>% 
    addTiles() %>% 
    addRasterImage(x = layer, colors = pal_z, opacity = opacity) %>%
    addLegend_decreasing(pal = pal_z, values = values(layer), title = title)
  return(p1)
}

#' Alluvial plot of pH, crop and farming system
#'
#' This function create an alluvial plot 
#' between pH classes, farming systems, and crop type.
#' The plot rely on the parcats package.
#' @param df data with area per pH, per country, per farming system and per crop type
#' @param country selected country of interest
#' @param pal color palette for the different pH classes
#' @param lsize font size of the label (default: 10)
#' @param tsize font size of the tick (default: 8)
alluvial_parcats <- function(df, country, 
                             pal= RColorBrewer::brewer.pal(4, "PuOr"),
                             lsize=10, tsize=8){
  #select country
  subdf <- df[df$country%in%country,]
  
  # repeat lines according to perthousands
  dfcat <- subdf[rep(row.names(subdf), subdf$perc), c("ph","fsystem", "croptype")]
  names(dfcat) <- c("pH class", "Farming system", "Crop type")
  
  pal <- unique(pal[sort(dfcat$pH)])
  p <- alluvial_wide(data = dfcat, col_vector_flow=pal)
  fig <- parcats(p, marginal_histograms = FALSE,
                 hoveron='category',#'dimension, category
                 hoverinfo='probability', data_input = dfcat,
                 labelfont=list(size=lsize),
                 tickfont=list(size=tsize),
                 bundlecolors=FALSE) #seems to keep ph order ...
  return(fig)
}

#' Summary data for area per pH classes
#'
#' This function subset the pH dataset per selected country
#' @param df data with area per pH, per country, per farming system and per crop type
#' @param country selected country of interest
#' @param th threshold to remove insignificant crop (default: 1000 ha)
summaryPH <- function(df, country, th=1000){
  subdf <- df[df$country%in%country,]
  
  subdf[is.na(subdf)] <- 0
  subdf <- subdf[subdf$total>th,]
  
  # round areas (maybe that could be in DataPrep.R?)
  subdf$total <- round(subdf$total)
  subdf$area5.5_ha <- round(subdf$area5.5_ha)
  subdf$area6.0_ha <- round(subdf$area6.0_ha)
  subdf$area6.5_ha <- round(subdf$area6.5_ha)
  subdf$area14_ha <- round(subdf$area14_ha)
  
  names(subdf) <- c("Country", "Farming system", "Crop", "Total area (ha)",
                    "Area pH<5.5 (ha)", "Area 5.5<pH<6 (ha)", "Area 6<pH<6.5 (ha)",
                    "Area pH>6.5 (ha)")
  return(subdf)
}


# TAB 3 ---------------------------------------------------
#' Map lime requirement per country
#'
#' This function plot the lime requirement
#' using the Leaflet package
#' @param layer raster with lime requirement in t CaCO3/ha
#' @param border vector with the province border and lime requirement per province
#' @param met method to estimate lime requirement, either "Cochrane" or "Kamprath"
#' @param breaks definition of the lime classes, by default 6 classes with cuts at 0.2, 1, 2, 3, and 5
#' @param pal color palette for the different lime classes
#' @param opacity opacity of the lime requirement layer over the openstreetmap background (default: 0.7)
#' @param title title of the legend, by default 'lime rate'
map_lime <- function(layer, border,
                     met=c("Cochrane", "Kamprath"),
                     breaks=c(0, 0.2, 1, 2, 3, 5, Inf),
                     pal=RColorBrewer::brewer.pal(6, "YlOrRd"),
                     opacity=0.7,
                     title="lime rate"){
  
  met <- match.arg(met)
  
  # color palette
  pal_lime <-  colorBin(palette = pal, domain = values(layer), 
                        bins = breaks,
                        na.color = "transparent")
  
  # Specifiy the popup of the polygons
  if (met=="Cochrane"){
    leg <- paste(border$NAME_1, "<br> Lime requirement:", round(border$lime_mt_cochrane,2), "mt CaCO3")
  } else {
    leg <- paste(border$NAME_1, "<br> Lime requirement:", round(border$lime_mt_kamprath,2), "mt CaCO3")
  }
  
  # create the plot
  p1 <- leaflet(sf::st_as_sf(border)) %>% 
    addTiles() %>% 
    addRasterImage(x = layer, colors = pal_lime, opacity = opacity) %>%
    addPolygons(fillOpacity=0, color = "black", weight=2, popup = leg) %>%
    addLegend_decreasing(pal = pal_lime, values = values(layer), title = title)
  return(p1)
}


#' Donut plot for lime requirement per province
#'
#' This function plot the lime requirement
#' per province as a donut plot
#' @param df data with lime requirement with columns country, province, lime_mt_cochrane, lime_mt_kamprath
#' @param country country of interest
#' @param met method to estimate lime requirement, either "Cochrane" or "Kamprath"
#' @param th percentage threshold to discard a province into 'Other', default th=2%
#' @param pal color palette for the different provinces
#' @param textinfo define text shown in the donut: 'label' (default), 'label+percent', 'percent'
#'
donut_lime <- function(df, country, met=c("Cochrane", "Kamprath"), 
           th=2, pal=pals::cols25(), textinfo='label'){
  met <- match.arg(met)
  lr <- df[df$country==country,]
  
  if (met=="Cochrane"){
    lr$Mt <- lr$lime_mt_cochrane
  } else {
    lr$Mt <- lr$lime_mt_kamprath
  }
  lr$Perc <- lr$Mt/sum(lr$Mt)*100
  
  lrsel <- lr[lr$Perc>th, c("province", "Mt", "Perc")]
  #order lrsel
  lrsel <- lrsel[order(lrsel$Perc, decreasing = TRUE),]
  # set the colors
  col <- pal[1:nrow(lrsel)]
  
  # add other if regions were discarded
  if (nrow(lr)>nrow(lrsel)){
    other <- data.frame("province"="Other", 
                        "Mt"=sum(lr$Mt)-sum(lrsel$Mt), 
                        "Perc"= 100-sum(lrsel$Perc))
    lrsel <- rbind(lrsel, other)
    col <- c(col, "#DCDCDC")
  }

  # hover text
  leg <- paste0("<b>", lrsel$province, "</b>: <br>", round(lrsel$Mt,2), "mt CaCO3<br>", round(lrsel$Perc), "%")
  # create the figure
  fig <- plot_ly(lrsel, labels = ~province, values = ~Perc,
                 direction ='clockwise', sort=FALSE, 
                 marker = list(colors = col),
                 hoverinfo = 'text',
                 hovertext = leg,
                 textposition = 'inside', #insidetextorientation='radial',
                 textinfo = textinfo) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = " ",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations=list(text='Lime <br> requirement <br> (t CaCO3)', x=0.5, y=0.5, font_size=55,showarrow=FALSE),
           margin = list(l = 0, r = 0, b = 5, t = 10, pad = 4),
           paper_bgcolor='transparent') %>%
    config(
      modeBarButtons = list(list("toImage")),
      displaylogo = FALSE
    )
  return(fig)
}

#' Summary data for lime requirement per country
#'
#' This function subset the lime requirement dataset per selected country
#' @param df data with lime requirement per country, per province
#' @param country selected country of interest
summaryLime <- function(df, country){
  subdf <- df[df$country%in%country,]
  
  subdf$lime_mt_kamprath <- round(subdf$lime_mt_kamprath, 3)
  subdf$lime_mt_cochrane <- round(subdf$lime_mt_cochrane, 3)
  subdf$lime_mt_merlos <- round(subdf$lime_mt_merlos, 3)
  
  names(subdf) <- c("Country", "Province",
                    "Lime Kamprath (mt)", "Lime Cochrane (mt)", "Lime Merlos (mt)")
  return(subdf)
}

## TAB 4 ------------------------------
#' Map return on investment per country
#'
#' This function plot the return on investment
#' using the Leaflet package
#' @param layer raster with return on investment for a given lime price
#' @param border vector with the province border
#' @param breaks definition of the ROI classes, by default 3 classes with cuts at 1 and 2
#' @param pal color palette for the different ROI classes
#' @param opacity opacity of the ROI layer over the openstreetmap background (default: 0.9)
#' @param title title of the legend, by default 'return on investment'
map_roi <- function(layer, border, 
                    breaks = c(0,1,2,Inf),
                    pal=RColorBrewer::brewer.pal(3, "RdYlGn"), 
                    opacity=0.9, title="return on investment"){
  # color palette
  pal_z = colorBin(palette = pal, domain = values(layer), 
                   bins = breaks,
                   na.color = "transparent")
  # Specifiy the popup of the polygons
  leg <- border$NAME_1
  
  # create the plot
  p1 <- leaflet(sf::st_as_sf(border)) %>% 
    addTiles() %>% 
    addRasterImage(x = layer, colors = pal_z, opacity = opacity) %>%
    addPolygons(fillOpacity=0, color = "black", weight=2, popup = leg) %>%
    addLegend_decreasing(pal = pal_z, values = values(layer), title = title)
  return(p1)
}


#' Barplot for return on investment
#'
#' This function plot the % of ROI categories  per crop type
#' with different height based on the importance of crop type (in ha)
#' @param df data with return on investment per crop type. 
#'  Contains columns roi, roi_resid, crop_area_ha, croptype
#' @param breaks cut-off values for ROI, by default: c(0,1,2,Inf)
#' @param lab labels of the ROI categories, by default: c("ROI<1", "1<ROI<2", "ROI>2")
#' @param pal color palette for the different ROI classes
#'
barroi <- function(df, resid=TRUE,
                   breaks=c(0,1,2,Inf), 
                   lab=c("ROI<1", "1<ROI<2", "ROI>2"),
                   pal=RColorBrewer::brewer.pal(3, "RdYlBu")){
  if (resid){
    roi <- df$roi_resid
  } else {
    roi <- df$roi
  }
  nfac <- length(breaks)-1
  if (length(lab)!=nfac){
    print("error: incorrect number of labels")
    lab <- paste0("x",1:nfac)
  }
  
  roi_cat <- cut(roi, breaks=breaks, paste0("x",1:nfac), 
                 include.lowest=TRUE)
  
  area_roitype <- tapply(NAto0(df$crop_area_ha), list(df$croptype, roi_cat), sum)
  area_roitype[is.na(area_roitype)] <- 0
  perc_roitype <- as.data.frame(area_roitype/rowSums(area_roitype)*100)
  perc_roitype$croptype <- row.names(perc_roitype)
  
  area_croptype<- tapply(df$crop_area_ha, df$croptype, sum)
  width_type <- area_croptype/max(area_croptype)+0.1
  # ytick = cumsum(width_type)-width_type
  # space between bars
  space <- c(0,cumsum(rep(0.02,3)))
  # coordinates of the bars
  ytick <- cumsum(width_type)-width_type/2 - width_type[1]/2 +space
  
  leg <- paste0("<b>", perc_roitype$croptype,"</b>:", round(area_croptype/1000,1), "k ha")
  fig <- plot_ly(perc_roitype, 
                 x= ~x1,
                 y= ~ytick,
                 marker = list(color = pal[1],
                               line = list(color = 'grey',
                                           width = 1)),
                 hoverinfo = 'text',
                 hovertext = leg) %>%
    add_bars(width = width_type,
             name=lab[1],
             orientation = 'h',
             hoverinfo = 'text',
             hovertext = paste(lab[1], ":", round(perc_roitype[,1],1), "%"))
  for (i in 2:nfac){
    fig <- fig %>% add_bars(x= perc_roitype[,i],
                            y= ytick,
                            width = width_type,
                            marker = list(color = pal[i]),
                            name=lab[i], 
                            orientation = 'h',
                            hoverinfo = 'text',
                            hovertext = paste(lab[i], ":", round(perc_roitype[,i],1), "%"))
  }
  fig <- fig %>% add_bars(x = 0, y= ~ytick,
                          name = " ",
                          orientation = 'h',
                          marker = list(color = 'white',
                                        line = list(color = 'white',
                                                    width = 0)),
                          hoverinfo = 'text',
                          hovertext = leg)
  
  fig <- fig %>%
    layout(barmode = 'stack',
           yaxis= list(showline = TRUE, 
                       linecolor = '#000',
                       tickvals=ytick, 
                       ticktext= row.names(perc_roitype),
                       title = ""),
           xaxis=list(title = "% of crop type area"),
           hovermode = 'y unified',
           #margin = list(l = 0, r = 0, b = 5, t = 10, pad = 4 ),
           paper_bgcolor='transparent') %>%
    config(
      modeBarButtons = list(list("toImage")),
      displaylogo = FALSE
    )
  
  return(fig)
}


#' Summary data for return on investment
#'
#' This function subset the ROI dataset per selected country and province
#' @param df data with area per pH, per country, per farming system and per crop type
#' @param country selected country of interest
#' @param province selected province of interest (by default, All)
#' @param th threshold to remove insignificant crop (default: 1000 ha)
summaryROI <- function(df, country, province="All", th=1000){
  if (province =="All"){
    subdf <- df[df$country%in%country,]
  } else {
    subdf <- df[df$province %in% province,]
  }

  # remove lines with less than th ha
  subdf <- subdf[subdf$crop_area_ha>th,]
  
  # replace NA and infinite values by 0
  subdf$roi[is.na(subdf$roi)] <- 0
  subdf$roi[is.infinite(subdf$roi)] <- 0
  
  # select and sort the column
  colK <- c("country","province","crop","croptype","crop_area_ha",
            "roi","roi_resid","crop_price_us_per_t", "lime_price_us_per_t",
            "returns_us", "returns_us_resid", "lime_t","costs_us")
  subdf <- subdf[,colK]
  
  names(subdf) <- c("Country", "Province", "Crop", "Crop type",
                    "Crop area (ha)", "Return on investment", "Return on investment long term",
                    "Crop price (USD/t)", "Lime price (USD/t)", "Return (USD)",
                    "Return long term (USD)", "Lime requirement (t)", "Cost lime (USD)")
  return(subdf)
}



## All purposes functions -------------------

#' Replace NA and negative values by 0
#'
#' @param x numeric vector
#'
NAto0 <- function(x){
  x[x<0] <- 0
  x[is.na(x)] <- 0
  return(x)
}

#' Create leaflet legend with decreasing order for numeric variable
#'
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = TRUE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- labFormat(type = "numeric", cuts)
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}
