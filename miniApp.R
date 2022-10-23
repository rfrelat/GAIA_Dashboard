# Script to make the plots of the GAIA Dashboard
source("global.R")

# TAB 2: Soil acidity ---------------------------
selCountry="Ethiopia"
map_ph(PH[[selCountry]])

summaryPH(tab2, selCountry)


# TAB 3: Lime requirement -----------------------
selCountry="Kenya" # "ETH" would be good to harmonize with tab 2
selLime="Cochrane" # Cochrane, Kamprath
selSoil="ph" #ecec, ph, ai

map_lime(cochrane[[selCountry]],
         gadm36[[selCountry]],
         pal = pallime, met = selLime)

b <- donut_lime(tab3, selCountry, selLime)

# TAB 4: Return on investment -------------------
selCountry="Ethiopia"
selPrice="100" # 50, 75, 100

lay <- paste0("hp_", selPrice, "_roi_resid")
rastroi <- raster(roi[[selCountry]], layer=lay)
names(rastroi) <- "ROI"
mapcatcountry(rastroi, 
              gadm36[[selCountry]], title="Return on investment",
              breaks = c(0,1,2,Inf), opacity = 0.9,
              col = RColorBrewer::brewer.pal(3, "RdYlBu"))

tabS <- tab4[[selPrice]]
tabS <- tabS[tabS$country %in% selCountry,]
barroi(tabS)

summaryROI(tab4[[selPrice]], selCountry)
