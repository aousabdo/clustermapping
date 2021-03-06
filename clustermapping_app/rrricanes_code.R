library(rrricanes)
library(tidyverse)
library(pryr)
library(leaflet)

storms_list_df <- rrricanes::get_storms(years = 1998:2018, basins = c("AL", "EP"))


pull_storm_data <- function(storm_name = NULL
                            , storm_df = NULL
                            , products_vec = c("discus", "fstadv")){
  # simple function to get storms data from the National Hurricane Center databases
  
  # function arguments
  # storm_name: exact storm name as in the National Hurricane Center database. Exampels are Hurricane Irma, 
  #             Hurricane Harvey, etc.
  # storm_df: a data frame containing a data about storms available in the NHC databases. This dataframe 
  #           would contain the following columns: "Year"  "Name"  "Basin" "Link" 
  # products_vec: products vector, this is the product argument for the get_storm_data from the rrricanes pacakge
  
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
    get_storm_data(products = products_vec)
  
  return(list(storm_data = storm_data))
}

katrina_link <- storms_list_df %>% 
  filter(str_detect(Name, "Katrina")) %>% 
  filter(Year == 2005) %>% 
  pull(Link)

katrina_data <- get_storm_data(links = katrina_link, products = products_vec)

ds <- katrina_data

ds_har <-storms_list_df %>%
  filter(Name == "Hurricane Irma") %>%
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
                                      , advisory_num = NULL
                                      , download_all_advisories = FALSE){
  # Function to get gis storm advisories from the National Hurricane Center
  
  # fuction arguments
  # storm_data_obj: an object that contains data about a given storm. This is usually the output of the 
  # get_storm_data from the rrricanes package or the pull_storm_data function. This object must contain
  # the "fstadv" product
  # advisory_num: the advisory number to download
  # download_all_advisories: download all available advisories. If this is set to FALSE, the function 
  # will download the latest advisory
  
  require(rrricanes)
  
  # get the key for the storm
  key <- storm_data_obj[["fstadv"]] %>% pull(Key) %>% first()
  
  # get a list of the available advisories 
  gis_advisories <- gis_advisory(key = key)
  
  
  if(length(gis_advisories) == 0){
    # this gis advisory data is not available for all storms. 
    stop("\tStorm has no gis advisories, quitting...\n")
  }else{
    invisible(cat("\tGetting advisories for storm key", key,"\n"))
  }
  
  # get the names of advisories
  gis_advisories <- str_split_fixed(gis_advisories, "day_", n = 2)[, 2] 
  gis_advisories <- str_split_fixed(string = gis_advisories, pattern = ".zip", n = 2)[, 1]%>%
    unique()
  
  # if the user specified an advisory number, then just download that advisory GIS files
  if(!is.null(advisory_num)){
    # make sure the advisory number given is valid
    if(sum(advisory_num %in% gis_advisories) == 0){
      invisible(cat("\tadvisory number supplied is not valid for selected storm...\n"))
      adv_num_inv <- TRUE
    }else{
      gis_advisories_list <- gis_advisory(key = key, advisory = advisory_num) %>%
        gis_download()
    }
    # if he instead wants to download all advisories, then do so
  }
  
  # if the advisory number given is found to be invalid, then we need to see if the 
  # user has asked for all the advisory GIS data to be downloaded and if not then 
  # just download the latest advisory data
  
  if(adv_num_inv){
    if(download_all_advisories){
      invisible(cat("\tGetting data for all advisoris, this might take a while...\n"))
      # create an empty list to fill it later with the gis advisory data
      gis_advisories_list <- list()
      for(i in 1:length(gis_advisories)){
        invisible(cat("\tGetting data for advisory ", gis_advisories[i],"\n"))
        gis_advisories_list[[i]] <- gis_advisory(key = key, advisory = gis_advisories[i]) %>%
          gis_download()
      }
    }else{
      # if the user is not requesting to download all advisories, nor did he specify an advisory number, then 
      # just download the latest advisory
      invisible(cat("\tGetting data for latest advisory...\n"))
      gis_advisories_list <- gis_advisory(key = key, advisory = gis_advisories[length(gis_advisories)]) %>%
        gis_download()
    }
  }
  return(list(gis_advisories = gis_advisories, gis_advisories_data = gis_advisories_list))
}


storm_map <- function(gis_adv_obj = NULL
                      , providers_tile = providers$OpenStreetMap){
  # function to make leaflet map for a given gis advisory object
  
  # function arguments
  # gis_adv_obj: a gis advisory object, the output of gis_advisory %>% gis_download or
  # pull_storm_gis_advisories functions
  # providers_tile: providers tile, see providers
  
  if(!is.list(gis_adv_obj)) stop("gis_adv_obj needs to be a gis list downloaded with gis_download from the rrricanes library")
  
  # get the names of the gis object
  gis_obj_names <- names(gis_adv_obj) 
  
  # now get the names of the different gis components
  pgn_obj <- gis_obj_names[str_detect(gis_obj_names, "_pgn")]
  lin_obj <- gis_obj_names[str_detect(gis_obj_names, "_lin")]
  pts_obj <- gis_obj_names[str_detect(gis_obj_names, "_pts")]
  
  # obtain the gis data for each gis class and convert it into an sf object
  storm_pgn <- sf::st_as_sf(gis_adv_obj[[pgn_obj]])
  storm_lin <- sf::st_as_sf(gis_adv_obj[[lin_obj]])
  storm_pts <- sf::st_as_sf(gis_adv_obj[[pts_obj]])
  
  # get advisory labels
  labels <- storm_pts$DATELBL
  
  ############################################################
  # build a leaflet map
  
  # start with a blank map
  m <- leaflet () 
  
  # add the basemap
  m <- addProviderTiles(m, providers_tile)
  
  # draw the 'cone of uncertainty'
  m <- addPolygons(m, data = storm_pgn, color = "black", weight = "2", fillColor="red", fillOpacity = 0.3) 
  
  # draw the predicted track
  m <- addPolylines(m, data = storm_lin) 
  
  # draw the predicted track positions
  m <- addCircleMarkers(m, data = storm_pts, color = "red", radius = 2, label = labels,
                        labelOptions = labelOptions(noHide = TRUE,  textOnly = TRUE)) 
  
  return(m)
}

rm(list=ls())

source('global.R')

storms_list_df <- rrricanes::get_storms(years = 1998:2018, basins = c("AL", "EP"))


irma <- pull_storm_data(storm_name = "Hurricane Irma"
                        , storm_df = storms_list_df
                        , products_vec = c("discus", "fstadv"))


irma_gis_advisories_avlbl <- pull_storm_gis_advisories(storm_data_obj = irma)

irma_gis_advisories_data_all <- download_storm_gis_advisories(gis_advisories_vec = irma_gis_advisories_avlbl
                                                              , storm_key = "al112017"
                                                              # , advisory_num = "009"
                                                              , download_all_advisories = TRUE
                                                              , storm_data_obj = irma$storm_data)

irma_gis_advisories_data <- download_storm_gis_advisories(gis_advisories_vec = irma_gis_advisories_avlbl
                                                          , storm_key = "al112017"
                                                          , advisory_num = "009"
                                                          , storm_data_obj = irma$storm_data)

build_storm_map(gis_adv_obj = irma_gis_advisories_data)

build_storm_map(gis_adv_obj = irma_gis_advisories_data_all[[23]])

# get counties within a hurricane advisory
irma_pgn_sf <- irma_gis_advisories_data_all[[84]]$al112017_052_5day_pgn %>% st_as_sf()

# make sure the hurricane advisory has the same crs as the counties sf object
st_crs(irma_pgn_sf) <- st_crs(counties_sf)

# now subset counties that are inside the hurricane advisory
p1 <- counties_sf[irma_pgn_sf, ]

# p1 contains any county that touches the hurricane advisoty track
# to be more accurate, we can get only the counties that are totally 
# within the hurricane track

w1 <- st_within(counties_sf, irma_pgn_sf)

# filter the new w1 object
w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

p2 <- counties_sf[w2, ]

# p2 now only contains the counties that are inside the hurricane track

storm <- irma_gis_advisories_data_all[[15]]

storm_polygon_name <- names(storm)[str_detect(names(storm), "pgn")]
irma_pgn_sf <- storm[[storm_polygon_name]] %>% st_as_sf()

affectd <- get_affected_areas(storm_polygon_sf = irma_pgn_sf
                              , counties_sf_obj = counties_sf
                              , states_sf_obj = states_sf
                              , msa_sf_obj = msa_sf
                              , economic_areas_sf_obj = economic_areas_sf
                              , verbose = TRUE)

build_storm_map(gis_adv_obj = storm
                , counties_affected = affectd$counties_affected)

build_storm_map(gis_adv_obj = storm
                , counties_affected = affectd$counties_affected
                , counties_within = affectd$counties_within)
