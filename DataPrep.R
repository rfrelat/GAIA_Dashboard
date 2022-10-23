# Script to prepare the dataset for the GAIA Dashboard
# Author: R. Frelat
# Last modification: 23 October 2022

# 0. Load package, functions and set parameters ------------
# Load require package
require(raster)
require(rgdal)
source("Tools.R")

crop_type <- list(
  'MAIZ'='Cereal',
  'SORG'= 'Cereal',
  'WHEA'= 'Cereal',
  'BARL'= 'Cereal',
  'RICE'= 'Cereal',
  'PMIL'= 'Cereal',
  'SMIL'= 'Cereal',
  'BEAN'= 'Legume',
  'CHIC'= 'Legume',
  'LENT'= 'Legume',
  'COWP'= 'Legume',
  'PIGE'= 'Legume',
  'SOYB'= 'Legume',
  'GROU'= 'Legume',
  'ACOF'= 'Commodity',
  'RCOF'= 'Commodity',
  'SUGC'= 'Commodity',
  'COTT'= 'Commodity',
  'COCO'= 'Commodity',
  'TEAS'= 'Commodity',
  'TOBA'= 'Commodity',
  'POTA'= 'RTBs',
  'SWPO'= 'RTBs',
  'CASS'= 'RTBs',
  'BANA'= 'RTBs',
  'PLNT'= 'RTBs')

#1. Tab 2: Soil acidity alluvial plot ---------------------
# load data for tab2
tab2 <- read.csv("rawdata/tab2_crop-fsystem-country.csv")
tab2 <- tab2[,-ncol(tab2)] #remove column diff

# get the list of SSA countries
# for selectInput() in Soil acidity tab
listSSA <- sort(unique(tab2$country))

# remove the number in farming systems
tab2$fsystem <- substr(tab2$fsystem, regexpr("\\.", tab2$fsystem)+2, nchar(tab2$fsystem))

# merge the area columns (transformation wide to long)
# for the alluvial plot, it is easier to have data in long format
# with country, farming system, area, ph and croptype as column
colarea <- grep("area", names(tab2), value=TRUE)
colothe <- names(tab2)[!names(tab2)%in%colarea]
area <- do.call(c, tab2[,colarea])
# create one variable ph for the 4 categories
phlevels <- c("pH<5.5", "5.5<pH<6.0", "6.0<pH<6.5","pH>6.5")
ph <- c(rep(phlevels[1], nrow(tab2)),
        rep(phlevels[2], nrow(tab2)),
        rep(phlevels[3], nrow(tab2)),
        rep(phlevels[4], nrow(tab2)))
ph <- factor(ph, levels=phlevels,
             ordered = TRUE)
# repeat tab2 4 times to get country and farming system information 
tab2b <- rbind(tab2[,colothe],tab2[,colothe],
               tab2[,colothe],tab2[,colothe])
tab2b$area <- area
tab2b$ph <- ph
# transform crop into croptype
tab2b$croptype <- as.character(unlist(crop_type[tab2b$crop]))

#remove all lines with no ha or NA
tab2b$area[is.na(tab2b$area)] <- 0
tab2b <- tab2b[tab2b$area>0,]

# simplify dataset per country to speed-up the alluvial plot
# create 'other' farming system if less than 5% of area per country
tab2c <- c()
# simplification loop per country
for (i in sort(unique(tab2b$country))){
  subdf <- tab2b[tab2b$country==i,]
  # simplify farming systems
  area_fsystem <- tapply(subdf$area, subdf$fsystem, sum)
  # replace if less than 5%
  othFS <- names(area_fsystem)[area_fsystem/sum(area_fsystem)<0.05]
  if (length(othFS)>1){
    subdf$fsystem[subdf$fsystem%in%othFS] <- "Other"
  }
  # further simplify the dataset by adding up repeated lines
  fsctph <- paste(subdf$fsystem, subdf$croptype, subdf$ph, sep="_")
  area <- tapply(subdf$area,fsctph, sum)
  cat <- strsplit(names(area), "_")
  newdf <- data.frame(
    "country"=i,
    "fsystem"=sapply(cat, function(x) x[[1]]),
    "croptype"=sapply(cat, function(x) x[[2]]),        
    "ph"=sapply(cat, function(x) x[[3]]), 
    "area"=area
  )
  newdf$ph <- factor(newdf$ph, levels = levels(subdf$ph), ordered = TRUE)
  # order the new data.frame per farming system, ph and crop type
  # to get same order in the alluvial plot
  newdf <- newdf[order(newdf$fsystem, newdf$ph, newdf$croptype),]
  
  # Calculate the per thousand of area
  newdf$perc <- round(newdf$area/sum(newdf$area)*1000)
  # Remove all flows that represent less than 0.5 per thousand
  newdf <- newdf[newdf$perc>0,]
  tab2c <- rbind(tab2c, newdf)
}
dim(tab2c) #1265 rows instead of 11830 in tab2b dim(tab2b)
tab2b <- tab2c

#2. Tab 2: mapping PH in SSA ------------------------------
# load the ph raster
soilPH <- raster("rawdata/ph_cropland.tif") # res: 0.0083
# load the country borders
borders <- shapefile("rawdata/gadm36_ssa_1.shp")

# replace naming of gadm to match names in listSSA
borders$NAME_0 <- gsub("Côte d'Ivoire", "Ivory Coast", borders$NAME_0)
borders$NAME_0 <- gsub("Guinea-Bissau", "Guinea Bissau", borders$NAME_0)
borders$NAME_0 <- gsub("Republic of Congo", "Congo", borders$NAME_0)
borders$NAME_0 <- gsub("Democratic Republic of the Congo", "DR Congo", borders$NAME_0)

# loop over SSA country
PH <- list()
for (i in listSSA){
  #select the border of country i
  bordi <- borders[borders$NAME_0==i,]
  #crop the raster to fit the country border
  phi <- crop(soilPH, bordi)
  phj <- mask(phi, bordi)
  # aggregate the raster (to reduce its size)
  # if large country, resolution 0.0333
  # if small country, resolution 0.0167
  if(ncol(phj)>800 | nrow(phj)>800){
    phj <- aggregate(phj, 4) 
  } else {
    phj <- aggregate(phj, 2) 
  }
  #save the cropped and aggregated raster in PH
  PH[[i]] <- phj
}

#3. Tab 3: Lime requirement donut plot -------------------------------
tab3 <- read.csv("rawdata/tab3_lime.csv")

#shorten the name of Southern Nations, Nationalities and Peoples
tab3$province <- gsub("Southern Nations, Nationalities and Peoples", "SNNP", tab3$province)

# get the list of GAIA countries
listGAIA <- sort(unique(tab3$country))

#4. Tab 4: Return on investment -------------------------------
tab4 <- read.csv("rawdata/tab4_roi.csv")

#replace the country by full country name
tab4$iso3 <- tab4$country
tab4$country <- borders$NAME_0[match(tab4$iso3, borders$GID_0)]
# replace the infinite values (due to no lime requirement) by 0
tab4$roi_resid[is.infinite(tab4$roi_resid)] <- 0
tab4$roi_resid[is.na(tab4$roi_resid)] <- 0

# convert crop into crop type
tab4$croptype <- as.character(unlist(crop_type[tab4$crop]))

#shorten the name of Southern Nations, Nationalities and Peoples
tab4$province <- gsub("Southern Nations, Nationalities and Peoples", "SNNP", tab4$province)

# calculate the roi for different crop price
df100 <- tab4
# price 50
df50 <- df100
df50$lime_price_us_per_t <- 50
df50$costs_us <- df50$lime_t * df50$lime_price_us_per_t
df50$roi <- ifelse(df50$costs_us>0, df50$returns_us / df50$costs_us, 0)
df50$roi_resid <- ifelse(df50$costs_us>0, df50$returns_us_resid / df50$costs_us, 0)
df50$roi_resid[is.na(df50$roi_resid)] <- 0

# price 75
df75 <- df100
df75$lime_price_us_per_t <- 75
df75$costs_us <- df75$lime_t * df75$lime_price_us_per_t
df75$roi <- ifelse(df75$costs_us>0, df75$returns_us / df75$costs_us, 0)
df75$roi_resid <- ifelse(df75$costs_us>0, df75$returns_us_resid / df75$costs_us, 0)
df75$roi_resid[is.na(df75$roi_resid)] <- 0

#save the different prices into a list
tab4 <- list("100"=df100,
             "75"=df75,
             "50"=df50)

#5. Tab 3&4: map lime requirement and ROI -----------------
# load lime requirement raster
limeC <- raster("rawdata/caco3_cochrane.tif") # res: 0.0083
limeK <- raster("rawdata/caco3_kamprath.tif") # res: 0.0083
# load roi raster and create a stack object (faster process)
roi100 <- raster("rawdata/hp_100_roi_resid.tif") # res: 0.083333
roi75 <- raster("rawdata/hp_75_roi_resid.tif") # res: 0.083333
roi50 <- raster("rawdata/hp_50_roi_resid.tif") # res: 0.083333
roiS <- stack(roi50, roi75, roi100)

# remove infinite values in roiS
roiS <- clamp(roiS, lower=-1, upper=max(tab4[["50"]]$roi_resid)*1000, useValues=FALSE)

# rename Southern Nations, Nationalities and Peoples
borders$NAME_1 <- gsub("Southern Nations, Nationalities and Peoples", "SNNP", borders$NAME_1)

# loop over GAIA countries
cochrane <- list()
kamprath <- list()
gadm36 <- list()
roi <- list()
for (i in listGAIA){
  # get the border of country i
  bordi <- borders[borders$NAME_0==i,]
  # add the lime requirement per province
  bordi$lime_mt_kamprath <-  tab3$lime_mt_kamprath[match(bordi$NAME_1, tab3$province)]
  bordi$lime_mt_cochrane <-  tab3$lime_mt_cochrane[match(bordi$NAME_1, tab3$province)]
  # save the country border
  gadm36[[i]] <- bordi
  
  # Cochrane lime requirement for country i
  limi <- crop(limeC, bordi)
  limj <- mask(limi, bordi)
  # aggregate the raster (to reduce its size)
  if(ncol(limj)>800 | nrow(limj)>800){
    limj <- aggregate(limj, 4) 
  } else {
    limj <- aggregate(limj, 2) 
  }
  #save the raster
  cochrane[[i]] <- limj
  
  # Kamprath lime requirement for country i
  limi <- crop(limeK, bordi)
  limj <- mask(limi, bordi)
  # aggregate the raster (to reduce its size)
  if(ncol(limj)>800 | nrow(limj)>800){
    limj <- aggregate(limj, 4) 
  } else{
    limj <- aggregate(limj, 2) 
  }
  kamprath[[i]] <- limj
  
  # return on investment for country i
  limi <- crop(roiS, bordi)
  limj <- mask(limi, bordi)
  # no aggregation because roi raster have low resolution
  roi[[i]] <- limj
}

# 6. Save data for shiny ----------------------------------
save(listSSA, listGAIA, tab2, tab2b, tab3, tab4,
     cochrane, kamprath, PH, roi, gadm36,
     file="DataDashboard.Rdata")
