library(sp)
library(rgeos)
library(maptools)
library(gpclib)
library(plyr)
library(rgdal)
library(sf)
library(countrycode)
library(rworldmap)
library(scatterpie)
library(imputeTS)
library(naniar)

library(tidyverse)
library(dplyr)
library(RCurl)
library(animation)
  library(ffmpeg)


#### Load data from Johns Hopkins Github
web <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data <- read.csv(text=web)

bycountry <- aggregate(. ~Country.Region, data = data, sum, na.rm=TRUE)


#### Reshape wide to long

update <- colnames(bycountry)[ncol(bycountry)]
update_date <- str_replace(update,"X","")
update_date <- str_replace_all(update_date,"[.]","-")
update_date <- as.POSIXct(update_date, format="%m-%d-%y")

bycountry.long <- gather(bycountry, date, confirmed, X1.22.20:update, factor_key=TRUE)
bycountry.long <- bycountry.long[with(bycountry.long, order(Country.Region)),]

bycountry.long$date <- sub(".","",bycountry.long$date) 
bycountry.long$date <- str_replace_all(bycountry.long$date, "[.]", "-")
bycountry.long$date <- as.POSIXct(bycountry.long$date, format="%m-%d-%y")

write.csv(bycountry.long, "/Users/alexis_pro/Dropbox/WorldBank/AFRCE/coronavirus/data/coronavirus_jhu_long.csv")


##### Load world boundaries and centroids
world.boundaries <- read_sf("/Users/alexis_pro/Dropbox/WorldBank/AFRCE/shapefiles/WB country polygons/WB_CountryPolys.shp")
centroids <- read_sf("/Users/alexis_pro/Dropbox/WorldBank/AFRCE/shapefiles/country_centroids_az8/country_centroids_az8.shp")

##### get unique codes just to be sure
world.iso.codes <- world.boundaries$ISO_Codes %>%
  unique()

world.continents <- countrycode(sourcevar = world.iso.codes,
                                origin = "iso3c",
                                destination = "continent")

world.names.df <- data.frame(country = world.iso.codes,
                             continent = world.continents)


african.country.names <- world.names.df %>%
  filter(continent ==  "Africa") %>%
  pull(country) %>%
  as.character()

africa.boundaries.df <- world.boundaries %>%
  filter(ISO_Codes %in% african.country.names)


###### Merge world shapefile with data

#but first fix some country names
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "Korea, South", "Republic of Korea")
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "US", "United States")
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "Brunei", "Brunei Darussalam")
centroids$name_long <- str_replace_all(centroids$name_long, "Taiwan", "Taiwan*")
centroids$name_long <- str_replace_all(centroids$name_long, "Republic of Congo", "Congo (Brazzaville)")
centroids$name_long <- str_replace_all(centroids$name_long, "Democratic Republic of the Congo", "Congo (Kinshasa)")
centroids$name_long <- str_replace_all(centroids$name_long, "CÃ´te d'Ivoire", "Cote d'Ivoire")
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "Czechia", "Czech Republic")
centroids$name_long <- str_replace_all(centroids$name_long, "Swaziland", "Eswatini")
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "Holy See", "Vatican")
centroids$name_long <- str_replace_all(centroids$name_long, "Macedonia", "North Macedonia")
bycountry.long$Country.Region <- str_replace_all(bycountry.long$Country.Region, "Russia", "Russian Federation")

#And fix US lat long
centroids$Longitude[centroids$iso_a3 == "USA"] <- -98.6
centroids$Latitude[centroids$iso_a3 == "USA"] <- 40.9

bycountry.long.coord <- merge(bycountry.long,
                              centroids,
                              by.x = "Country.Region",
                              by.y = "name_long",
                              all.x=TRUE)

bycountry.long.coord$dummy <- 1

bycountry.long.coord <- bycountry.long.coord[with(bycountry.long.coord, order(Country.Region,date)), ]

bycountry.long.coord[,"days"] <- ave(bycountry.long.coord$dummy, bycountry.long.coord$Country.Region, FUN=cumsum)


#### Subset SSA countries

bycountry.long.coord.ssa <- subset(bycountry.long.coord, continent == "Africa")


#### iterate by date

start <- 1
end <- max(bycountry.long.coord.ssa$days)

bycountry.long.coord.ssa$month <- months(bycountry.long.coord.ssa$date)
bycountry.long.coord.ssa$month <- as.character(bycountry.long.coord.ssa$month)

bycountry.long.coord.ssa$day <- as.character(strftime(bycountry.long.coord.ssa$date, format = "%d"))

bycountry.long.coord.ssa$confirmed_log <- asinh(bycountry.long.coord.ssa$confirmed)*5


bycountry.long.coord.ssa$total <- ave(bycountry.long.coord.ssa$confirmed, bycountry.long.coord.ssa$date, FUN=sum)

bycountry.long.coord.ssa <- subset(bycountry.long.coord.ssa, Country.Region != "Western Sahara")






### Put logo

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

e4t <- get_png("/Users/alexis_pro/Dropbox/WorldBank/AFRCE/coronavirus/logo/E For T Gradient.png")



# loop to create images



for (i in 1:end)
{
  
  
  ##### subset by date
  
  bycountry.long.coord.ssa.date <- subset(bycountry.long.coord.ssa, days == i, select=c("Country.Region", "Latitude", "Longitude", "confirmed_log", "month", "day", "total"))
  
  
  bycountry.long.coord.ssa.date <- bycountry.long.coord.ssa.date %>%
    replace_with_na(replace = list(confirmed_log = c(0)))
  
  ###### Map data
  
  world.plot <- ggplot(data = africa.boundaries.df)+
    geom_sf(fill= "#2e318d", colour = "white", lwd=1)
  
  
  ## Add circles
  coronavirus.map <- world.plot+
    geom_point(data= bycountry.long.coord.ssa.date,
               shape = 21,
               fill = "#de6e4b",
               color = "white",
               alpha = 0.7,
               aes(x = Longitude,
                   y = Latitude,
                   size = confirmed_log))+
    scale_size_identity()+
    labs(title = "Spread and reported cases of COVID-19 across Africa since February 28th",
         caption = "Data source: own elaboration using CSSE at Johns Hopkins University data. Accessed on April 6, 2020")+
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 24),
          plot.caption = element_text(hjust = 0, size = 18, face = "italic"),
          panel.background = element_rect(fill = "white"))+
    annotation_custom(e4t, xmin = 43, xmax = 65, ymin = 30, ymax = 40)+
    geom_text(aes(label = paste0("Date: \n", bycountry.long.coord.ssa$month[i],", ", bycountry.long.coord.ssa$day[i], "\n\nAfrica total \nconfirmed cases: ", format(bycountry.long.coord.ssa$total[i],format="f",big.mark = ",")), x = -5, y = -25), colour = "2e318d", size = 10)+
    ggsave(filename=paste("/Users/alexis_pro/Dropbox/WorldBank/AFRCE/coronavirus/maps/raw/new/day",i,".png",sep=""), height=12, width=12)
  
  coronavirus.map
  
}  


# setwd("/Users/alexis_pro/Dropbox/WorldBank/AFRCE/coronavirus/maps/raw")
# 
# imgs <- list.files(pattern="*.png")
# saveVideo({
#   for(img in imgs){
#     im <- magick::image_read(img)
#     plot(as.raster(im))
#   }  
# })

# Create GIF from images --> looking for a better solution since this code breaks R

# library(purrr)
# library(magick)
# list.files(pattern = "*.png") %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=5) %>% # animates
#   image_write("animation.gif") # write to current dir




