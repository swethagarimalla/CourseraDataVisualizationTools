library(magrittr)
library(dplyr)
library(tidyr)

setwd("/Users/swethagarimalla/Desktop/Coursera/DataScientist/DataScientistPreReq/Course4BuildingDataVisualizationTools/")

load_data <- function(filename="ebtrk_atlc_1988_2015.txt", ...){

  raw.data <- readr::read_table(file = filename, col_names = FALSE);
  colnames <- c("storm_id", "storm_name", "mdH", "year", "latitude", "longitude", "max_wind_speed", 
                "min pressure", "rad_windspeed", "eye_d", "press_outer", "rad_outer", "n34", "n50",
                "n64", "code", "dist");

  colnames(raw.data) <- colnames;
  return(raw.data);
}

clean_data <- function(raw.data, ...){
  data <- raw.data %>%  
    mutate(storm_id=paste0(storm_name,"-",year), 
           date = lubridate::parse_date_time(paste0(mdH,year), 'mdHy')) %>% 
    gather(key = key, value = value, n34:n64) %>% 
    separate(value, sep = c(-9, -6, -3), into = c("ne", "se", "sw", "nw")) %>%
    mutate(wind_speed = substring(key, 2), longitude = as.numeric(longitude) * -1, 
           ne = as.integer(ne), se = as.integer(se), sw = as.integer(sw),  sw = as.integer(sw)) %>%
    select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw);
  
  return(data)
}
  
select_hurricane <- function(data, starts_with="IKE", ...){  
  hurricane <- data %>% filter(stringr::str_detect(storm_id, paste0(starts_with)));
  return(hurricane);
}



hurricane <- load_data() %>% clean_data() %>% select_hurricane("IKE")

