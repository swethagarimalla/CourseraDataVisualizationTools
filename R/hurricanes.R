library(magrittr)
library(dplyr)
library(tidyr)
library(grid)

setwd("/Users/swethagarimalla/Desktop/Coursera/DataScientist/DataScientistPreReq/Course4BuildingDataVisualizationTools/")


load_data <- function(filename="ebtrk_atlc_1988_2015.txt", ...){
  
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1);
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final");
  
  ext_tracks <- read_fwf(filename, 
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99");
  return(ext_tracks);
}
clean_data <- function(raw.data, ...){
  data <- raw.data %>%  
    mutate(storm_id=paste0(storm_name,"-",year), 
           date = lubridate::parse_date_time(paste0(month, day, hour,year), 'mdHy')) %>% 
    gather(key = key, value = value, radius_34_ne:radius_64_nw) %>% 
    separate(key, sep = "_", into = c("radius", "wind_speed", "direction")) %>%
    spread(key = direction, value = value ) %>%
    mutate(longitude = as.numeric(longitude) * -1) %>%
    select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw);
  
  return(data)
}

load_data2 <- function(filename="ebtrk_atlc_1988_2015.txt", ...){

  raw.data <- readr::read_table(file = filename, col_names = FALSE);
  colnames <- c("storm_id", "storm_name", "mdH", "year", "latitude", "longitude", "max_wind_speed", 
                "min pressure", "rad_windspeed", "eye_d", "press_outer", "rad_outer", "n34", "n50",
                "n64", "code", "dist");

  colnames(raw.data) <- colnames;
  return(raw.data);
}
clean_data2 <- function(raw.data, ...){
  data <- raw.data %>%  
    mutate(storm_id=paste0(storm_name,"-",year), 
           date = lubridate::parse_date_time(paste0(mdH,year), 'mdHy')) %>% 
    gather(key = key, value = value, n34:n64) %>% 
    separate(value, sep = c(-9, -6, -3), into = c("ne", "se", "sw", "nw")) %>%
    mutate(wind_speed = substring(key, 2), longitude = as.numeric(longitude) * -1, 
           ne = as.integer(ne), se = as.integer(se), nw = as.integer(nw),  sw = as.integer(sw)) %>%
    select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw);
  
  return(data)
}

select_hurricane <- function(data, starts_with="IKE", ...){  
  hurricane <- data %>% filter(stringr::str_detect(storm_id, paste0(starts_with)));
  return(hurricane);
}


#build geom here#
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                   required_aes = c("center_x", "center_y","radius_ne", "radius_nw", "radius_se", "radius_sw" ),
                   default_aes = aes(fill = 1, color = 1, shape = 1, alpha = 1, shape = 1 ),
                   draw_key = <a function used to draw the key in the legend>,
                   draw_panel = function(data, panel_scales, coord) {
                     ## Function that returns a grid grob that will 
                     ## be plotted (this is where the real work occurs)
                   }
)



#run data#
hurricane <- load_data() %>% clean_data() %>% select_hurricane("IKE")
hurricane2 <- load_data2() %>% clean_data2() %>% select_hurricane("IKE")

