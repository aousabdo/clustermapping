library(rrricanes)
library(tidyverse)
library(pryr)
library(leaflet)

storms_list_df <- rrricanes::get_storms(years = 1998:2018, basins = c("AL", "EP"))


pull_storm_data <- function(storm_name = NULL
                            , storm_df = NULL
                            , products_vec = c("discus", "fstadv")){
  # simple function to get storms data for a given storm
  require(rrricanes)
  
  # make sure we have the list of storms from the NHC
  if(is.null(storm_df)){
    invisible(cat("\tDataframe listing storms available on NHC was not given, getting list of storms form NHC\n"))
    storm_df <- rrricanes::get_storms(years = 1998:2018, basins = c("AL", "EP"))
  }
  
  # storm name must be exact
  if(!(storm_name %in% storm_df$Name)) stop("Storm requested doens't existed in database. Make sure spelling is correct!")
  
  invisible(cat("\tGetting storm data ...\n"))
  storm_data <- storm_df %>%
    filter(Name == storm_name) %>%
    pull(Link) %>%
    get_storm_data(products = c("discus", "fstadv"))

  return(list(storm_data = storm_data))
}

katrina_link <- storms_list_df %>% 
  filter(str_detect(Name, "Katrina")) %>% 
  filter(Year == 2005) %>% 
  pull(Link)

katrina_data <- get_storm_data(links = katrina_link, products = products_vec)

ds <- katrina_data

ds_har <-storms_list_df %>%
  filter(Name == "Hurricane Harvey") %>%
  pull(Link) %>%
  get_storm_data(products = c("discus", "fstadv"))

str(tidy_fcst(ds$fstadv))

library(ggmap)
register_google(key = "AIzaSyBus5_JLXu1gaXyeErmEL45I3LV1gRcYPQ")

mapgilbert <- get_map(location = c(lon = mean(ds$fstadv$Lon), lat = mean(ds$fstadv$Lat)), zoom = 4,
                      maptype = "satellite", scale = 2)
ggmap(mapgilbert) +
  geom_point(data = ds$fstadv, aes(x = Lon, y = Lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


tidy_fcst_wr(ds$fstadv) %>% head(2)
tidy_fcst(ds$fstadv) %>% head(2)
tidy_adv(ds$fstadv) %>% head(2)

left_join(tidy_fcst(ds$fstadv), tidy_fcst_wr(ds$fstadv), by = c("Key", "Adv", "Date", "FcstDate"))

tracking_chart(color = "black", size = 0.1, fill = "white")

al_tracking_chart(color = "black", size = 0.1, fill = "white")

ep_tracking_chart(color = "black", size = 0.1, fill = "white")

key <- ds_har$fstadv %>% pull(Key) %>% first()

x <- gis_advisory(key = key)
length(x)

gis_advisory(key = key, advisory = 19)

gis <- gis_advisory(key = key, advisory = 5) %>%
  gis_download()

str(gis)
names(gis)

gis$al092017_019_5day_pgn
class(gis$al092017_019_5day_pgn)

ds_har <-rrr_storms %>%
  filter(Name == "Hurricane Irma") %>%
  pull(Link) 


# irma <- get_storm_data(links = "https://www.nhc.noaa.gov/archive/2017/IRMA.shtml?", products = c("discus", "fstadv"))
irma <- pull_storm_data(storm_name = "Hurricane Irma", storm_df = storms_list_df, products_vec = c("discuss", "fstadv"))

# key <- irma$fstadv %>% pull(Key) %>% first()
# gis_advisories <- gis_advisory(key = key)
# gis <- gis_advisory(key = key, advisory = 40) %>%
#   gis_download()

pull_storm_gis_advisories <- function(storm_data_obj = NULL
                                      , download_all_advisories = FALSE){
  require(rrricanes)
  
  # get the key for the storm
  key <- storm_data_obj[["fstadv"]] %>% pull(Key) %>% first()

  gis_advisories <- gis_advisory(key = key)
  if(length(gis_advisories) == 0){
    stop("\tStorm has no gis advisories, quitting...\n")
  }else(invisible(cat("\tGetting advisories for storm key", key,"\n")))

  gis_advisories <- str_split_fixed(gis_advisories, "day_", n = 2)[, 2] 
  gis_advisories <- str_split_fixed(string = gis_advisories, pattern = ".zip", n = 2)[, 1]%>%
    unique()
  
  if(download_all_advisories){
    invisible(cat("\tGetting data for all advisoris, this might take a while...\n"))
    # create an empty list
    gis_advisories_list <- list()
    for(i in 1:length(gis_advisories)){
      invisible(cat("\tGetting data for advisory ", gis_advisories[i],"\n"))
      gis_advisories_list[[i]] <- gis_advisory(key = key, advisory = gis_advisories[i]) %>%
        gis_download()
    }
  }else{
    # if the user is not requesting to download all advisories, then just download the latest advisory
    invisible(cat("\tGetting data for latest advisory...\n"))
    gis_advisories_list <- gis_advisory(key = key, advisory = gis_advisories[length(gis_advisories)]) %>%
      gis_download()
  }

  return(list(gis_advisories = gis_advisories, gis_advisories_data = gis_advisories_list))
}


storm_map <- function(gis_adv_obj = NULL){
  if(!is.list(gis_adv_obj)) stop("gis_adv_obj needs to be a gis list downloaded with gis_download from the rrricanes library")
  gis_obj_names <- names(gis_adv_obj) 
  print(gis_obj_names)
  pgn_obj <- gis_obj_names[str_detect(gis_obj_names, "_pgn")]
  lin_obj <- gis_obj_names[str_detect(gis_obj_names, "_lin")]
  pts_obj <- gis_obj_names[str_detect(gis_obj_names, "_pts")]
  
  print(pgn_obj)
  storm_pgn <- sf::st_as_sf(gis_adv_obj[[pgn_obj]])
  storm_lin <- sf::st_as_sf(gis_adv_obj[[lin_obj]])
  storm_pts <- sf::st_as_sf(gis_adv_obj[[pts_obj]])
  
  labels <- storm_pts$DATELBL
 
  # create a blank map
  m <- leaflet () 
  
  # add the basemap
  m <- addProviderTiles(m, providers$OpenStreetMap)
  # m <- addProviderTiles(m , Esri.WorldStreetMap)
  
  # draw the 'cone of uncertainty'
  m <- addPolygons(m, data = storm_pgn, color = "black", weight = "2", fillColor="red", fillOpacity = 0.3) 
  
  # draw the predicted track
  m <- addPolylines(m, data = storm_lin) 
  
  # draw the predicted track positions
  m <- addCircleMarkers(m, data = storm_pts, color = "red", radius = 2, label = labels,
                        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)) 

  m
  return(m)
}


